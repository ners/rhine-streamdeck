module FRP.StreamDeck.ButtonClock where

import Data.Maybe qualified
import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    deriving stock (Show)

data ButtonClock = ButtonClock

instance
    ( MonadUnliftIO m
    , MonadUnliftIO (StreamDeckT m s)
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
        eventQ <- newTBQueueIO 100
        let
            toButtonEvent :: (Int, Bool, Bool) -> Maybe ButtonEvent
            toButtonEvent (i, False, True) = Just $ ButtonPressed i
            toButtonEvent (i, True, False) = Just $ ButtonReleased i
            toButtonEvent _ = Nothing
            initialStates = replicate (StreamDeck.buttonCount @s) False
            producer = iterateM $ \oldStates -> do
                newStates <- StreamDeck.readKeyStates
                time <- getCurrentTime
                let events =
                        zip3 [0 ..] oldStates newStates
                            & Data.Maybe.mapMaybe toButtonEvent
                            & fmap (time,)

                atomically $ forM_ events $ writeTBQueue eventQ
                pure newStates

            consumer = atomically $ readTBQueue eventQ

        forkIO $ producer initialStates
        pure (constM consumer, initialTime)

instance GetClockProxy ButtonClock

instance Semigroup ButtonClock where
    t <> _ = t
