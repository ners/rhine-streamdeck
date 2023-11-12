module FRP.StreamDeck
    ( module System.Hardware.Streamdeck
    , withDevice
    , reset
    , readActiveKey
    , readKeyStates
    , setKeyImage
    )
where

import FRP.StreamDeck.App
import Internal.Prelude
import System.Hardware.Streamdeck (enumerate, keyCount)
import System.Hardware.Streamdeck qualified as HW

withDeckIO :: (Device -> IO c) -> App c
withDeckIO f = liftIO . f =<< view #deck

runStreamdeck :: AppState -> App a -> IO a
runStreamdeck state f = runReaderT f._runApp state

withDevice :: DeviceInfo -> App () -> IO ()
withDevice device a = HW.withDevice device $ \deck -> runStreamdeck AppState{..} $ reset >> a

reset :: App ()
reset = withDeckIO HW.reset

readKeyStates :: App [Bool]
readKeyStates = withDeckIO HW.readKeyStates

readActiveKey :: App (Maybe Int)
readActiveKey = withDeckIO HW.readActiveKey

setKeyImage :: Word8 -> ByteString -> App ()
setKeyImage = (withDeckIO .) . HW.setKeyImage
