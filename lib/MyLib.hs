{-# LANGUAGE AllowAmbiguousTypes #-}

module MyLib where

import Data.ByteString.Lazy qualified as LBS
import Data.Either (fromRight)
import FRP.StreamDeck.DisplayButtonClock
import FRP.StreamDeck.Layer
import Internal.Prelude
import System.Hardware.Devices.StreamDeckMk2
import System.Hardware.Devices.StreamDeckPlus
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
     . ( Pixel px
       , IsStreamDeckWithDisplayButtons s
       )
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

setDisplayButtonImage
    :: forall m s
     . (MonadFail m, MonadIO m, IsStreamDeckWithDisplayButtons s)
    => Int
    -> DynamicImage
    -> StreamDeckT m s ()
setDisplayButtonImage key (encodeDynamicImage -> image) = StreamDeck.setButtonImage key image

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

instance Layer DisplayButtonEvent Teletubbies where
    layerEvent (DisplayButtonReleased 0) Blue =
        SwitchLayers
            { fromLayer = Blue
            , toLayer = Red
            }
    layerEvent (DisplayButtonPressed 0) Red =
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
    => Rhine (StreamDeckT m s) DisplayButtonClock () ()
mainRhine =
    layer Red
        >-> traceMSF "LayerEvents: "
        >-> arrMCl handleLayerEvent
        @@ DisplayButtonClock
  where
    handleLayerEvent LayerEvent{event = DisplayButtonPressed key, onLayer = Red} =
        setDisplayButtonImage key $ redDynamicKeyImage @s
    handleLayerEvent LayerEvent{event = DisplayButtonPressed key, onLayer = Blue} =
        setDisplayButtonImage key $ blueDynamicKeyImage @s
    handleLayerEvent LayerEvent{event = DisplayButtonReleased key} =
        setDisplayButtonImage key $ blackDynamicKeyImage @s
    handleLayerEvent _ = pure ()

someFunc :: IO ()
someFunc = do
    let fn
            :: ( MonadUnliftIO m
               , MonadFail m
               , IsStreamDeckWithDisplayButtons s
               )
            => StreamDeckT m s ()
        fn = do
            sn <- view $ #deviceInfo . #serialNumber
            traceShowM sn
            flow mainRhine
    void $ StreamDeck.runStreamDeck @StreamDeckMk2 fn
    void $ StreamDeck.runStreamDeck @StreamDeckPlus fn
