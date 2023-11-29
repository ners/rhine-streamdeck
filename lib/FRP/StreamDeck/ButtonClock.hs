module FRP.StreamDeck.ButtonClock where

import Data.Maybe qualified
import FRP.StreamDeck.InputClock
import Internal.Prelude
import System.Hardware.StreamDeck qualified as StreamDeck

data ButtonEvent
    = ButtonPressed Int
    | ButtonReleased Int
    deriving stock (Show, Eq)

data ButtonClock = ButtonClock

instance
    ( MonadIO m
    , IsStreamDeckWithButtons s
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
        (inputClock, initialTime) <- initClock InputClock
        pure (runningSelectClock initialTime inputClock, initialTime)

runningSelectClock
    :: forall m s
     . ( MonadIO m
       , IsStreamDeckWithButtons s
       )
    => Time InputClock
    -> MSF (StreamDeckT m s) () (Tick InputClock)
    -> MSF (StreamDeckT m s) () (Tick ButtonClock)
runningSelectClock initialTime inputClock = filterS
    . feedback ([], initialStates, initialTime)
    $ proc ((), (oldEvents, oldStates, oldTime)) -> do
        case oldEvents of
            e : es -> returnA -< (Just e, (es, oldStates, oldTime))
            [] -> do
                (newTime, newInput) <- inputClock -< ()
                let none = (Nothing, (oldEvents, oldStates, oldTime))
                case StreamDeck.parseButtonStates @s newInput of
                    Nothing -> returnA -< none
                    Just newStates ->
                        case (newTime,) <$> stateChanges oldStates newStates of
                            [] -> returnA -< none
                            e : es -> returnA -< (Just e, (es, newStates, newTime))
  where
    toButtonEvent :: (Int, Bool, Bool) -> Maybe ButtonEvent
    toButtonEvent (i, False, True) = Just $ ButtonPressed i
    toButtonEvent (i, True, False) = Just $ ButtonReleased i
    toButtonEvent _ = Nothing
    stateChanges :: [Bool] -> [Bool] -> [ButtonEvent]
    stateChanges = (Data.Maybe.mapMaybe toButtonEvent .) . zip3 [0 ..]
    initialStates = replicate (StreamDeck.buttonCount @s) False

instance GetClockProxy ButtonClock
