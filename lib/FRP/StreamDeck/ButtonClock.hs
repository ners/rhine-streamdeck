module FRP.StreamDeck.ButtonClock where

import Data.Maybe qualified
import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    deriving stock (Show, Eq)

data ButtonClock = ButtonClock

type ButtonStates = [Bool]

type TimedTag = (Time ButtonClock, Tag ButtonClock)

instance
    ( MonadUnliftIO m
    , MonadFix m
    , IsStreamDeck s
    )
    => Clock (StreamDeckT m s) ButtonClock
    where
    type Time ButtonClock = UTCTime
    type Tag ButtonClock = ButtonEvent
    initClock
        :: ButtonClock
        -> RunningClockInit
            (StreamDeckT m s)
            (Time ButtonClock)
            (Tag ButtonClock)
    initClock ButtonClock = do
        initialTime <- getCurrentTime
        let
            toButtonEvent :: (Int, Bool, Bool) -> Maybe ButtonEvent
            toButtonEvent (i, False, True) = Just $ ButtonPressed i
            toButtonEvent (i, True, False) = Just $ ButtonReleased i
            toButtonEvent _ = Nothing

            initialStates = replicate (StreamDeck.buttonCount @s) False
            nextEvents
                :: ([TimedTag], ButtonStates)
                -> StreamDeckT m s ([TimedTag], ButtonStates)
            nextEvents ([], oldStates) = do
                newStates <- StreamDeck.readKeyStates
                time <- getCurrentTime
                let events =
                        zip3 [0 ..] oldStates newStates
                            & Data.Maybe.mapMaybe toButtonEvent
                            & fmap (time,)
                pure (events, newStates)
            nextEvents x = pure x

            runningClock :: MSF (StreamDeckT m s) () TimedTag
            runningClock = proc () -> do
                -- nextEvents is guaranteed to not return an empty list of events 🤯
                rec (t : events, newStates) <-
                        arrM nextEvents
                            <<< iPre ([], initialStates)
                            -<
                                (events, newStates)
                returnA -< t
        pure (runningClock, initialTime)

instance GetClockProxy ButtonClock

instance Semigroup ButtonClock where
    t <> _ = t
