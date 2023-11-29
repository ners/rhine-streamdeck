module FRP.StreamDeck.DisplayButtonClock where

import Data.Maybe qualified
import FRP.StreamDeck.InputClock
import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data DisplayButtonEvent
    = DisplayButtonPressed Int
    | DisplayButtonReleased Int
    deriving stock (Show, Eq)

data DisplayButtonClock = DisplayButtonClock

instance
    ( MonadIO m
    , IsStreamDeckWithDisplayButtons s
    )
    => Clock (StreamDeckT m s) DisplayButtonClock
    where
    type Time DisplayButtonClock = UTCTime
    type Tag DisplayButtonClock = DisplayButtonEvent
    initClock
        :: DisplayButtonClock
        -> RunningClockInit
            (StreamDeckT m s)
            (Time DisplayButtonClock)
            (Tag DisplayButtonClock)
    initClock DisplayButtonClock = do
        (inputClock, initialTime) <- initClock InputClock
        pure (runningSelectClock initialTime inputClock, initialTime)

runningSelectClock
    :: forall m s
     . ( MonadIO m
       , IsStreamDeckWithDisplayButtons s
       )
    => Time InputClock
    -> MSF (StreamDeckT m s) () (Tick InputClock)
    -> MSF (StreamDeckT m s) () (Tick DisplayButtonClock)
runningSelectClock initialTime inputClock = filterS
    . feedback ([], initialStates, initialTime)
    $ proc ((), (oldEvents, oldStates, oldTime)) -> do
        case oldEvents of
            e : es -> returnA -< (Just e, (es, oldStates, oldTime))
            [] -> do
                (newTime, newInput) <- inputClock -< ()
                let none = (Nothing, (oldEvents, oldStates, oldTime))
                case StreamDeck.parseDisplayButtonStates @s newInput of
                    Nothing -> returnA -< none
                    Just newStates ->
                        case (newTime,) <$> stateChanges oldStates newStates of
                            [] -> returnA -< none
                            e : es -> returnA -< (Just e, (es, newStates, newTime))
  where
    toDisplayButtonEvent :: (Int, Bool, Bool) -> Maybe DisplayButtonEvent
    toDisplayButtonEvent (i, False, True) = Just $ DisplayButtonPressed i
    toDisplayButtonEvent (i, True, False) = Just $ DisplayButtonReleased i
    toDisplayButtonEvent _ = Nothing
    stateChanges :: [Bool] -> [Bool] -> [DisplayButtonEvent]
    stateChanges = (Data.Maybe.mapMaybe toDisplayButtonEvent .) . zip3 [0 ..]
    initialStates = replicate (StreamDeck.displayButtonCount @s) False

instance GetClockProxy DisplayButtonClock
