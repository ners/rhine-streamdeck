module FRP.StreamDeck.InputClock where

import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data InputClock = InputClock

instance
    ( MonadIO m
    , IsStreamDeck s
    )
    => Clock (StreamDeckT m s) InputClock
    where
    type Time InputClock = UTCTime
    type Tag InputClock = ByteString
    initClock
        :: InputClock
        -> RunningClockInit
            (StreamDeckT m s)
            (Time InputClock)
            (Tag InputClock)
    initClock InputClock = do
        initialTime <- getCurrentTime
        let
            runningClock :: StreamDeckT m s (Time InputClock, Tag InputClock)
            runningClock = do
                input <- StreamDeck.readInput
                time <- getCurrentTime
                pure (time, input)
        pure (constM runningClock, initialTime)

instance GetClockProxy InputClock
