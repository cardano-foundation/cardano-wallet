{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra glue functions to work with "MonadUnliftIO" exception types.  We need
-- this because the @retry@ package uses the generalized exception handler type
-- from 'Control.Monad.Catch.Handler'. But the 'UnliftIO.Exceptions' module has
-- its own definition of exactly the same type.

module UnliftIO.Compat
     ( -- * Handler conversion
       coerceHandler
     , coerceHandlers
     , mkRetryHandler

       -- * Missing combinators
     , handleIf

       -- * IO conversion utilities
     , unliftIOWith
     , unliftIOTracer

       -- * Re-export unsafe things
     , AsyncCancelled (..)
     ) where

import Prelude

import Control.Concurrent.Async
    ( AsyncCancelled (..) )
import Control.Exception.Base
    ( Exception )
import Control.Monad
    ( (<=<) )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..), askRunInIO, liftIO, wrappedWithRunInIO )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Control.Tracer
    ( Tracer (..) )
import UnliftIO.Exception
    ( Typeable, throwIO, try )

import qualified Control.Monad.Catch as Exceptions
import qualified Servant.Server as Servant
import qualified UnliftIO.Exception as UnliftIO

-- | Convert the generalized handler from 'UnliftIO.Exception' type to 'Control.Monad.Catch' type
coerceHandler :: UnliftIO.Handler IO Bool -> Exceptions.Handler IO Bool
coerceHandler (UnliftIO.Handler h) = Exceptions.Handler h

-- | Convert a list of handler factories from the 'UnliftIO.Exception' type to
-- 'Control.Monad.Catch' type. Such handlers are used in
-- 'Control.Retry.Recovering' for example.
coerceHandlers
    :: [a -> UnliftIO.Handler IO Bool]
    -> [a -> Exceptions.Handler IO Bool]
coerceHandlers = map (coerceHandler .)

-- | Shortcut for creating a single 'Control.Retry' handler, which doesn't use
-- the 'Control.Retry.RetryStatus' info.
mkRetryHandler
    :: Exception e
    => (e -> m Bool)
    -> [a -> Exceptions.Handler m Bool]
mkRetryHandler shouldRetry = [const $ Exceptions.Handler shouldRetry]

-- | A 'MonadUnliftIO' version of 'Control.Monad.Catch.handleIf'.
handleIf
    :: (MonadUnliftIO m, Exception e)
    => (e -> Bool)
    -> (e -> m a)
    -> m a
    -> m a
handleIf f h = UnliftIO.handle
    (\e -> if f e then h e else UnliftIO.throwIO e)

-- NOTE: This instance is problematic when parameter e is an Exception instance,
-- and 'catchAny' or similar functions are used in the wrapper.
--
-- See: https://github.com/fpco/unliftio/issues/68
instance (MonadUnliftIO m, Typeable e) => MonadUnliftIO (ExceptT e m) where
  withRunInIO exceptToIO =
    withExceptT unInternalException $ ExceptT $ try $
      withRunInIO $ \runInIO ->
        exceptToIO
          (runInIO . (either (throwIO . InternalException) pure <=< runExceptT))

-- | Wrapper for Left results of runExceptT. Used to deliver return values in
-- the MonadUnliftIO instance of ExceptT.
newtype InternalException e = InternalException { unInternalException :: e }
    deriving Typeable
instance Show (InternalException e) where
    show _ = "MonadUnliftIO InternalException"
instance Typeable e => Exception (InternalException e)

-- Use above instance to define unlift for servant Handler
instance MonadUnliftIO Servant.Handler where
    withRunInIO = wrappedWithRunInIO Servant.Handler Servant.runHandler'

-- | Lift a 'withResource' type function which runs in IO.
-- TODO: could maybe use existing functions
unliftIOWith
    :: MonadUnliftIO m
    => ((a -> IO b) -> IO b)
    -> ((a -> m b) -> m b)
unliftIOWith with action = do
    u <- askRunInIO
    liftIO (with (u . action))

-- | Provides a Tracer in IO given a 'Tracer m'.
unliftIOTracer :: MonadUnliftIO m => Tracer m a -> m (Tracer IO a)
unliftIOTracer tr = do
    u <- askRunInIO
    pure $ Tracer $ \a -> u $ runTracer tr a
