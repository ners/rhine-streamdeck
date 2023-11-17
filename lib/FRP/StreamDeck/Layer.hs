{-# OPTIONS_GHC -Wno-partial-fields #-}

module FRP.StreamDeck.Layer where

import GHC.Records (HasField (getField))
import Internal.Prelude

data LayerEvent e l
    = SwitchLayers {fromLayer :: l, toLayer :: l}
    | LayerEvent {onLayer :: l, event :: e}
    deriving stock (Show, Eq)

instance HasField "nextLayer" (LayerEvent e l) l where
    getField SwitchLayers{..} = toLayer
    getField LayerEvent{..} = onLayer

class Layer e l where
    layerEvent :: e -> l -> LayerEvent e l
    layerEvent event onLayer = LayerEvent{..}

layerEvents :: (Monad m, Layer e l, e ~ Tag cl) => ClSF m cl l (LayerEvent e l)
layerEvents = tagS &&& id >>> arrMCl (pure . uncurry layerEvent)

-- layer
--    :: forall l m. (Layer l, MonadFix m) => l -> ClSF m ButtonClock () (LayerEvent l)
-- layer initialLayer = proc () -> do
--    rec layerEvent <- layerEvents <<< iPre initialLayer -< layerEvent.nextLayer
--    returnA -< layerEvent

layer
    :: (Layer e l, Monad m, e ~ Tag cl)
    => l
    -> ClSF m cl () (LayerEvent e l)
layer initialLayer =
    feedback initialLayer $ arr snd >>> layerEvents >>> arr (toSnd (.nextLayer))
