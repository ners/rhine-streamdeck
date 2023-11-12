module FRP.StreamDeck.ButtonClock where

import Data.Maybe qualified
import FRP.StreamDeck qualified as StreamDeck
import FRP.StreamDeck.App
import Internal.Prelude

data ButtonClock = ButtonClock

instance Clock App ButtonClock where
    type Time ButtonClock = UTCTime
    type Tag ButtonClock = [Bool]
    initClock ButtonClock = do
        initialTime <- getCurrentTime
        let
            update :: MSF App () (Time ButtonClock, Tag ButtonClock)
            update = proc () -> do
                time <- constM getCurrentTime -< ()
                states <- constM StreamDeck.readKeyStates -< ()
                returnA -< (time, states)
        pure (update, initialTime)

instance GetClockProxy ButtonClock

instance Semigroup ButtonClock where
    t <> _ = t

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
