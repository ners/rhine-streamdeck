module FRP.StreamDeck.Layer where

import FRP.StreamDeck.ButtonClock (ButtonEvent (..))
import Internal.Prelude

class (Bounded l, Enum l) => Layer l where
    toLayer :: l -> ButtonEvent -> l
