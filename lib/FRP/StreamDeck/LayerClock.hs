module FRP.StreamDeck.LayerClock where

import FRP.StreamDeck.App (App)
import FRP.StreamDeck.ButtonClock (ButtonClock (ButtonClock))
import FRP.StreamDeck.Layer
import Internal.Prelude

type LayerClock l = SelectClock ButtonClock l

layerClock :: (Layer l) => LayerClock l
layerClock =
    SelectClock
        { mainClock = ButtonClock
        , select
        }
  where
    select :: Tag ButtonClock -> Maybe l
    select 

buttonStates :: ClSF App ButtonClock () [Bool]
buttonStates = tagS

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    deriving stock (Show)

buttonEvents :: ClSF App ButtonClock [Bool] [ButtonEvent]
buttonEvents = proc oldStates -> do
    newStates <- tagS -< ()
    let stateChanges =
            zip3 [0 ..] oldStates newStates
                & Data.Maybe.mapMaybe toButtonEvent
    returnA -< stateChanges
  where
    toButtonEvent :: (Int, Bool, Bool) -> Maybe ButtonEvent
    toButtonEvent (i, False, True) = Just $ ButtonPressed i
    toButtonEvent (i, True, False) = Just $ ButtonReleased i
    toButtonEvent _ = Nothing
