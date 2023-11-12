{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where

import FRP.StreamDeck.App
import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock), ButtonEvent (..), buttonEvents)
import Internal.Prelude
import FRP.StreamDeck qualified as StreamDeck

keyImageWidth :: Int
keyImageWidth = 72

keyImageHeight :: Int
keyImageHeight = 72

encodeImage :: (ColorSpaceConvertible px PixelYCbCr8) => Image px -> ByteString
encodeImage = LBS.toStrict . encodeJpegAtQuality 95 . convertImage . flipHorizontally . flipVertically

encodeDynamicImage :: DynamicImage -> ByteString
encodeDynamicImage = encodeImage . convertRGB8

generateKeyImage :: (Pixel px) => (Int -> Int -> px) -> Image px
generateKeyImage f = generateImage f keyImageWidth keyImageHeight

imageToDynamic :: (BmpEncodable px) => Image px -> DynamicImage
imageToDynamic = fromRight (error "generateDynamicKeyImage") . decodeBitmap . LBS.toStrict . encodeBitmap

generateDynamicKeyImage :: (Pixel px, BmpEncodable px) => (Int -> Int -> px) -> DynamicImage
generateDynamicKeyImage = imageToDynamic . generateKeyImage

setClickedKeyToImg :: DynamicImage -> App ()
setClickedKeyToImg img = do
    maybeKey <- StreamDeck.readActiveKey
    case maybeKey of
        Just key -> setKeyImage key img
        Nothing -> pure ()
    setClickedKeyToImg img

setKeyImage :: Int -> DynamicImage -> App ()
setKeyImage (fromIntegral -> key) (encodeDynamicImage -> image) = StreamDeck.setKeyImage key image

black :: PixelRGB8
black = PixelRGB8 0 0 0

blackDynamicKeyImage :: DynamicImage
blackDynamicKeyImage = generateDynamicKeyImage . const . const $ black

red :: PixelRGB8
red = PixelRGB8 255 0 0

redDynamicKeyImage :: DynamicImage
redDynamicKeyImage = generateDynamicKeyImage . const . const $ red

runStreamdeck :: AppState -> App a -> IO a
runStreamdeck state f = runReaderT f._runApp state

mainRhine :: Rhine App ButtonClock () ()
mainRhine =
    buttonEventStream
        >-> liftBaseS (trace "ButtonEvents: ")
        >-> handleButtonEvents
        @@ ButtonClock
  where
    initialButtonStates :: [Bool]
    initialButtonStates = replicate StreamDeck.keyCount False
    buttonEventStream :: ClSF App ButtonClock () [ButtonEvent]
    buttonEventStream = feedback initialButtonStates $ proc ((), c) -> do
        evs <- buttonEvents -< c
        states <- tagS -< ()
        returnA -< (evs, states)
    handleButtonEvents :: ClSF App ButtonClock [ButtonEvent] ()
    handleButtonEvents = arrMCl $ \events -> do
        forM_ events $ \case
            ButtonPressed key -> setKeyImage key redDynamicKeyImage
            ButtonReleased key -> setKeyImage key blackDynamicKeyImage

someFunc :: IO ()
someFunc =
    StreamDeck.enumerate >>= \case
        [] -> fail "no devices found"
        [device] -> StreamDeck.withDevice device $ flow mainRhine
        _ -> fail "multiple devices found"
