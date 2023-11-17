{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock), ButtonEvent (..))
import FRP.StreamDeck.Layer
import GHC.Records (HasField (getField))
import Internal.Prelude
import System.Hardware.Devices.StreamDeckMk2
import System.Hardware.StreamDeck qualified as StreamDeck

encodeImage :: (ColorSpaceConvertible px PixelYCbCr8) => Image px -> ByteString
encodeImage =
    LBS.toStrict
        . encodeJpegAtQuality 95
        . convertImage
        . flipHorizontally
        . flipVertically

encodeDynamicImage :: DynamicImage -> ByteString
encodeDynamicImage = encodeImage . convertRGB8

generateKeyImage
    :: forall px s
     . (Pixel px, IsStreamDeckWithDisplayButtons s)
    => (Int -> Int -> px)
    -> Image px
generateKeyImage f =
    generateImage
        f
        (StreamDeck.buttonImageWidth @s)
        (StreamDeck.buttonImageHeight @s)

imageToDynamic :: (BmpEncodable px) => Image px -> DynamicImage
imageToDynamic =
    fromRight (error "generateDynamicKeyImage")
        . decodeBitmap
        . LBS.toStrict
        . encodeBitmap

generateDynamicKeyImage
    :: forall px s
     . (Pixel px, BmpEncodable px, IsStreamDeckWithDisplayButtons s)
    => (Int -> Int -> px)
    -> DynamicImage
generateDynamicKeyImage = imageToDynamic . generateKeyImage @px @s

setClickedKeyToImg
    :: forall m s
     . (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s)
    => DynamicImage
    -> StreamDeckT m s ()
setClickedKeyToImg img = do
    maybeKey <- StreamDeck.readActiveKey
    case maybeKey of
        Just key -> setKeyImage key img
        Nothing -> pure ()
    setClickedKeyToImg img

setKeyImage
    :: forall m s
     . (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s)
    => Int
    -> DynamicImage
    -> StreamDeckT m s ()
setKeyImage key (encodeDynamicImage -> image) = StreamDeck.setKeyImage key image

black :: PixelRGB8
black = PixelRGB8 0 0 0

blackDynamicKeyImage
    :: forall s. (IsStreamDeckWithDisplayButtons s) => DynamicImage
blackDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ black

red :: PixelRGB8
red = PixelRGB8 255 0 0

redDynamicKeyImage
    :: forall s. (IsStreamDeckWithDisplayButtons s) => DynamicImage
redDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ red

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

blueDynamicKeyImage
    :: forall s. (IsStreamDeckWithDisplayButtons s) => DynamicImage
blueDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ red

data Teletubbies = Red | Blue deriving stock (Bounded, Enum, Eq, Show)

instance HasField "successor" Teletubbies Teletubbies where
    getField Red = Blue
    getField Blue = Red

instance Layer Teletubbies where
    layerEvent (ButtonPressed 0) l =
        SwitchLayers
            { fromLayer = l
            , toLayer = l.successor
            }
    layerEvent event onLayer = LayerButtonEvent{..}

mainRhine
    :: forall m s
     . ( MonadFail m
       , MonadIO m
       , IsStreamDeckWithDisplayButtons s
       )
    => Rhine (StreamDeckT m s) ButtonClock () ()
mainRhine =
    layer Red
        >-> traceMSF "LayerEvents: "
        >-> handleLayerEvent
        @@ ButtonClock
  where
    --handleButtonEvent :: ClSF (StreamDeckT m s) ButtonClock ButtonEvent ()
    --handleButtonEvent = arrMCl $ \case
    --    ButtonPressed key -> setKeyImage key $ redDynamicKeyImage @s
    --    ButtonReleased key -> setKeyImage key $ blackDynamicKeyImage @s
    handleLayerEvent :: ClSF (StreamDeckT m s) ButtonClock (LayerEvent Teletubbies) ()
    handleLayerEvent = arrMCl $ \case
        LayerButtonEvent{event = ButtonPressed key, onLayer = Red} -> setKeyImage key $ redDynamicKeyImage @s
        LayerButtonEvent{event = ButtonPressed key, onLayer = Blue} -> setKeyImage key $ blueDynamicKeyImage @s
        LayerButtonEvent{event = ButtonReleased key} -> setKeyImage key $ blackDynamicKeyImage @s
        _ -> pure ()

someFunc :: IO ()
someFunc = do
    StreamDeck.enumerate @StreamDeckMk2 >>= \case
        [] -> fail "no devices found"
        [device] -> StreamDeck.runStreamDeck device $ flow mainRhine
        _ -> fail "multiple devices found"
