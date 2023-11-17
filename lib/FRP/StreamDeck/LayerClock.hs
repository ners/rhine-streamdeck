module FRP.StreamDeck.LayerClock where

import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock))
import FRP.StreamDeck.Layer
import Internal.Prelude

type LayerClock l = SelectClock ButtonClock l

layerClock :: LayerClock l
layerClock =
    SelectClock
        { mainClock = ButtonClock
        , select
        }
  where
    select :: Tag ButtonClock -> Maybe l
    select = undefined
