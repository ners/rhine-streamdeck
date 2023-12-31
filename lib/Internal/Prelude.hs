module Internal.Prelude
    ( module Codec.Picture
    , module Codec.Picture.Extra
    , module Codec.Picture.Types
    , module Control.Concurrent
    , module Control.Lens.Combinators
    , module Control.Lens.Operators
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Control.Monad.Reader
    , module Data.Bits
    , module Data.ByteString
    , module Data.Functor
    , module Data.Maybe
    , module Data.Ord
    , module Data.Word
    , module FRP.Rhine
    , module GHC.Generics
    , module Prelude
    , module System.Hardware.StreamDeck
    , module Debug.Trace
    , getCurrentTime
    , traceMSF
    )
where

import Codec.Picture
import Codec.Picture.Extra
import Codec.Picture.Types
import Control.Concurrent
import Control.Lens.Combinators (view)
import Control.Lens.Operators
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bits
import Data.ByteString (ByteString)
import Data.Functor
import Data.Generics.Labels ()
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord
import Data.Time qualified
import Data.Word (Word16, Word8)
import Debug.Trace
import FRP.Rhine hiding (trace)
import GHC.Generics
import System.Hardware.StreamDeck (IsStreamDeck, IsStreamDeckWithDisplayButtons, StreamDeckT)
import Prelude

getCurrentTime :: (MonadIO m) => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

traceMSF :: forall a m t. (Show a, Monad m, Show (Diff (Time t))) => String -> ClSF m t a a
traceMSF prefix = proc a -> do
    t <- sinceInitS -< ()
    arrMCl traceM -< logStr t a
    returnA -< a
    where
        logStr :: Diff (Time t) -> a -> String
        logStr t x = concat ["[", show t, "] ", prefix, show x]
