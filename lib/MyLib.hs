{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock), ButtonEvent (..))
import FRP.StreamDeck.Layer
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

setButtonImage
    :: forall m s
     . (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s)
    => Int
    -> DynamicImage
    -> StreamDeckT m s ()
setButtonImage key (encodeDynamicImage -> image) = StreamDeck.setButtonImage key image

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
blueDynamicKeyImage = generateDynamicKeyImage @_ @s . const . const $ blue

data Teletubbies = Red | Blue deriving stock (Bounded, Enum, Eq, Show)

instance Layer ButtonEvent Teletubbies where
    layerEvent (ButtonReleased 0) Blue =
        SwitchLayers
            { fromLayer = Blue
            , toLayer = Red
            }
    layerEvent (ButtonPressed 0) Red =
        SwitchLayers
            { fromLayer = Red
            , toLayer = Blue
            }
    layerEvent event onLayer = LayerEvent{..}

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
        >-> arrMCl handleLayerEvent
        @@ ButtonClock
  where
    handleLayerEvent LayerEvent{event = ButtonPressed key, onLayer = Red} = setButtonImage key $ redDynamicKeyImage @s
    handleLayerEvent LayerEvent{event = ButtonPressed key, onLayer = Blue} = setButtonImage key $ blueDynamicKeyImage @s
    handleLayerEvent LayerEvent{event = ButtonReleased key} = setButtonImage key $ blackDynamicKeyImage @s
    handleLayerEvent _ = pure ()

someFunc :: IO ()
someFunc = do
    void $ StreamDeck.runStreamDeck @StreamDeckMk2 $ do
        sn <- view $ #deviceInfo . #serialNumber
        traceShowM sn
        when (sn == Just "DL17L2A82959") $ flow mainRhine
