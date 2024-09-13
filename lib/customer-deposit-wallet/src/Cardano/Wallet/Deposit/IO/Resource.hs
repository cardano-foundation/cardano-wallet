{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use void" #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Implementation of a 'Resource' (think REST) which can be initialized.
module Cardano.Wallet.Deposit.IO.Resource
    ( Resource
    , withResource
    , ErrResourceMissing (..)
    , onResource
    , ErrResourceExists (..)
    , putResource
    , ResourceStatus (..)
    , readStatus
    ) where

import Prelude

import Control.Concurrent
    ( forkFinally
    )
import Control.Concurrent.Class.MonadMVar
    ( MonadMVar (..)
    , putMVar
    , takeMVar
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    , TVar
    , atomically
    , readTVar
    , writeTVar
    )
import Control.Monad
    ( join
    , void
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow (..)
    , SomeException
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )

{-----------------------------------------------------------------------------
    Resource
------------------------------------------------------------------------------}

-- | Mutable resource (think REST) that holds a reference of type @a@
-- that has to be initialized with a 'with…' function.
data Resource e a = Resource
    { content :: TVar IO (ResourceStatus e a)
    , waitForEndOfLife :: IO ()
    -- ^ Wait until the 'Resource' is out of scope.
    }

-- | Possible status of the content of a 'Resource'.
data ResourceStatus e a
    = NotInitialized
    | Initializing
    | Initialized a
    | FailedToInitialize e
    | Vanished SomeException
    deriving (Show)

instance Functor (ResourceStatus e) where
    fmap _ NotInitialized = NotInitialized
    fmap _ Initializing = Initializing
    fmap f (Initialized a) = Initialized (f a)
    fmap _ (Vanished e) = Vanished e
    fmap _ (FailedToInitialize e) = FailedToInitialize e

-- | Read the status of a 'Resource'.
readStatus :: Resource e a -> IO (ResourceStatus e ())
readStatus resource = void <$> readTVarIO (content resource)

-- | Make a 'Resource' that can be initialized later.
--
-- Once the 'Resource' has been initialized,
-- it will also be cleaned up once the 'withResource' function has finished.
--
-- If the 'Resource' vanishes because of an exception,
-- the 'withResource' will /not/ be interrupted.
-- You can use 'getStatus' to poll the current status.
withResource
    :: (Resource e a -> IO b)
    -- ^ Action to perform on the 'Resource'.
    -> IO b
    -- ^ Result of the action.
withResource action = do
    content <- newTVarIO NotInitialized
    finished <- newEmptyMVar
    let waitForEndOfLife = takeMVar finished
        resource = Resource{content, waitForEndOfLife}
    action resource `finally` putMVar finished ()

-- | Error condition for 'onResource'.
data ErrResourceMissing e
    = -- | The 'Resource' has not been initialized yet.
      ErrNotInitialized
    | -- | The 'Resource' is currently being initialized.
      ErrStillInitializing
    | -- | The 'Resource' has not been initialized yet.
      ErrVanished SomeException
    | -- ErrFailedToInitialize
      ErrFailedToInitialize e
    deriving (Show)

-- | Perform an action on a 'Resource' if it is initialized.
onResource
    :: (a -> IO b)
    -- ^ Action to perform on the initialized 'Resource'.
    -> Resource e a
    -- ^ The 'Resource' to act on.
    -> IO (Either (ErrResourceMissing e) b)
onResource action resource = do
    eContent <- readTVarIO $ content resource
    case eContent of
        NotInitialized -> pure $ Left ErrNotInitialized
        Initializing -> pure $ Left ErrStillInitializing
        Initialized a -> Right <$> action a
        Vanished e -> pure $ Left $ ErrVanished e
        FailedToInitialize e -> pure $ Left $ ErrFailedToInitialize e

-- | Error condition for 'putResource'.
data ErrResourceExists e a
    = -- | The resource 'a' is currently being initialized.
      ErrAlreadyInitializing
    | -- | The resource 'a' has already been initialized.
      ErrAlreadyInitialized a
    | -- | The resource 'a' has vanished.
      ErrAlreadyVanished SomeException
    | -- | The resource 'a' has failed to initialize.
      ErrAlreadyFailedToInitialize e
    deriving (Show)

-- | Initialize a 'Resource' using a @with…@ function.
-- This @with…@ function will be called with an argument that does
-- not terminate until 'withResource' terminates.
-- The function can logically fail returning a 'Left' value.
-- Exceptions will be caught and stored in the 'Resource' as well
putResource
    :: (forall b. (a -> IO b) -> IO (Either e b))
    -- ^ Function to initialize the resource 'a'
    -> Tracer IO (ResourceStatus e a)
    -> Resource e a
    -- ^ The 'Resource' to initialize.
    -> IO (Either (ErrResourceExists e a) ())
putResource start trs resource = do
    forking <- atomically $ do
        ca :: ResourceStatus e a <- readTVar (content resource)
        case ca of
            FailedToInitialize e -> pure $ Left $ ErrAlreadyFailedToInitialize e
            Vanished e -> pure $ Left $ ErrAlreadyVanished e
            Initializing -> pure $ Left ErrAlreadyInitializing
            Initialized a -> pure $ Left $ ErrAlreadyInitialized a
            NotInitialized -> do
                writeTVar (content resource) Initializing
                pure $ Right (forkInitialization >> traceWith trs Initializing)
    case forking of
        Left e -> pure $ Left e
        Right action -> Right <$> action
  where
    controlInitialization = do
        r <- start run
        join $ atomically $ case r of
            Left e -> do
                writeTVar (content resource) (FailedToInitialize e)
                pure $ traceWith trs (FailedToInitialize e)
            Right () -> pure (pure ())
    forkInitialization = void $ forkFinally controlInitialization vanish

    run a = do
        atomically $ writeTVar (content resource) (Initialized a)
        traceWith trs (Initialized a)
        waitForEndOfLife resource

    vanish (Left e) = do
        atomically $ writeTVar (content resource) (Vanished e)
        traceWith trs (Vanished e)
    vanish (Right _) =
        pure () -- waitForEndOfLife has succeeded
