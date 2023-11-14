module FRP.StreamDeck.ButtonEventClock where

import FRP.StreamDeck.App
import FRP.StreamDeck.ButtonClock
import Internal.Prelude

type EventApp = EventChanT ButtonEvent App

type ButtonEventClock = HoistClock EventApp App (EventClock ButtonEvent)

buttonEventClock :: ButtonEventClock
buttonEventClock = liftClock ButtonClock
