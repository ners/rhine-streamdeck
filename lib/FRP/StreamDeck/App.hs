module FRP.StreamDeck.App where

import Control.Monad.Base (MonadBase (liftBase))
import Internal.Prelude

newtype AppState = AppState
    { deck :: Device
    }
    deriving stock (Generic)

newtype AppT m a = App
    { _runApp :: ReaderT AppState m a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader AppState
        , MonadIO
        , MonadFail
        )

instance (MonadIO m) => MonadBase IO (AppT m) where
    liftBase = liftIO

type App = AppT IO
