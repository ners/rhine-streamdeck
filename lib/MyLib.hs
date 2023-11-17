{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock), ButtonEvent (..), buttonEvents)
import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck
import System.Hardware.Devices.StreamDeckMk2


encodeImage :: (ColorSpaceConvertible px PixelYCbCr8) => Image px -> ByteString
encodeImage = LBS.toStrict . encodeJpegAtQuality 95 . convertImage . flipHorizontally . flipVertically

encodeDynamicImage :: DynamicImage -> ByteString
encodeDynamicImage = encodeImage . convertRGB8

generateKeyImage :: forall px s. (Pixel px, IsStreamDeckWithDisplayButtons s) => (Int -> Int -> px) -> Image px
generateKeyImage f = generateImage f (StreamDeck.buttonImageWidth @s) (StreamDeck.buttonImageHeight @s)

imageToDynamic :: (BmpEncodable px) => Image px -> DynamicImage
imageToDynamic = fromRight (error "generateDynamicKeyImage") . decodeBitmap . LBS.toStrict . encodeBitmap

generateDynamicKeyImage :: forall px s. (Pixel px, BmpEncodable px, IsStreamDeckWithDisplayButtons s) => (Int -> Int -> px) -> DynamicImage
generateDynamicKeyImage = imageToDynamic . generateKeyImage @px @s

setClickedKeyToImg :: forall m s. (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s) => DynamicImage -> StreamDeckT m s ()
setClickedKeyToImg img = do
    maybeKey <- StreamDeck.readActiveKey
    case maybeKey of
        Just key -> setKeyImage key img
        Nothing -> pure ()
    setClickedKeyToImg img

setKeyImage :: forall m s. (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s) => Int -> DynamicImage -> StreamDeckT m s ()
setKeyImage key (encodeDynamicImage -> image) = StreamDeck.setKeyImage key image

black :: PixelRGB8
black = PixelRGB8 0 0 0

blackDynamicKeyImage :: forall s. IsStreamDeckWithDisplayButtons s => DynamicImage
blackDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ black

red :: PixelRGB8
red = PixelRGB8 255 0 0

redDynamicKeyImage :: forall s. IsStreamDeckWithDisplayButtons s => DynamicImage
redDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ red

mainRhine :: forall m s. (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s) => Rhine (StreamDeckT m s) ButtonClock () ()
mainRhine =
    buttonEventStream
        >-> traceMSF "ButtonEvents: "
        >-> handleButtonEvents
        @@ ButtonClock
  where
    initialButtonStates :: [Bool]
    initialButtonStates = replicate (StreamDeck.buttonCount @s) False
    buttonEventStream :: ClSF (StreamDeckT m s) ButtonClock () [ButtonEvent]
    buttonEventStream = feedback initialButtonStates $ proc ((), c) -> do
        evs <- buttonEvents -< c
        states <- tagS -< ()
        returnA -< (evs, states)
    handleButtonEvents :: ClSF (StreamDeckT m s) ButtonClock [ButtonEvent] ()
    handleButtonEvents = arrMCl $ \events -> do
        forM_ events $ \case
            ButtonPressed key -> setKeyImage key $ redDynamicKeyImage @s
            ButtonReleased key -> setKeyImage key $ blackDynamicKeyImage @s

someFunc :: IO ()
someFunc = do
    StreamDeck.enumerate @StreamDeckMk2 >>= \case
        [] -> fail "no devices found"
        [device] -> StreamDeck.runStreamDeck device $ flow mainRhine
        _ -> fail "multiple devices found"
