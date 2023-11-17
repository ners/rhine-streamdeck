{-# OPTIONS_GHC -Wno-partial-fields #-}

module FRP.StreamDeck.Layer where

import FRP.StreamDeck.ButtonClock (ButtonClock, ButtonEvent (..))
import GHC.Records (HasField (getField))
import Internal.Prelude

data LayerEvent l
    = SwitchLayers {fromLayer :: l, toLayer :: l}
    | LayerButtonEvent {onLayer :: l, event :: ButtonEvent}
    deriving stock (Show, Eq)

instance HasField "nextLayer" (LayerEvent l) l where
    getField SwitchLayers{..} = toLayer
    getField LayerButtonEvent{..} = onLayer

class Layer l where
    layerEvent :: ButtonEvent -> l -> LayerEvent l
    layerEvent event onLayer = LayerButtonEvent{..}

layerEvents :: (Monad m, Layer l) => ClSF m ButtonClock l (LayerEvent l)
layerEvents = tagS &&& id >>> arrMCl (pure . uncurry layerEvent)

-- layer
--    :: forall l m. (Layer l, MonadFix m) => l -> ClSF m ButtonClock () (LayerEvent l)
-- layer initialLayer = proc () -> do
--    rec layerEvent <- layerEvents <<< iPre initialLayer -< layerEvent.nextLayer
--    returnA -< layerEvent

layer
    :: forall l m
     . (Layer l, Monad m)
    => l
    -> ClSF m ButtonClock () (LayerEvent l)
layer initialLayer =
    feedback initialLayer $ arr snd >>> layerEvents >>> arr (toSnd (.nextLayer))
