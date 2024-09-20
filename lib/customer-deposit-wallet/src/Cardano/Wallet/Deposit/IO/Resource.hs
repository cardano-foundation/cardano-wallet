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
    , closeResource
    ) where

import Prelude

import Control.Concurrent
    ( forkFinally
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM (..)
    , TVar
    , atomically
    , readTVar
    , writeTVar
    )
import Control.Monad
    ( void
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow (..)
    , SomeException
    )

{-----------------------------------------------------------------------------
    Resource
------------------------------------------------------------------------------}

-- | Mutable resource (think REST) that holds a reference of type @a@
-- that has to be initialized with a 'with…' function.
data Resource e a = Resource
    { content :: TVar IO (ResourceStatus e a)
    , waitForEndOfLife :: IO (Either (Either SomeException e) ())
    -- ^ Wait until the 'Resource' is out of scope.
    }

-- | Possible status of the content of a 'Resource'.
data ResourceStatus e a
    = Closed
    | Opening
    | Open a
    | FailedToOpen e
    | Vanished SomeException
    | Closing
    deriving (Show)

instance Functor (ResourceStatus e) where
    fmap _ Closed = Closed
    fmap _ Opening = Opening
    fmap f (Open a) = Open (f a)
    fmap _ (Vanished e) = Vanished e
    fmap _ (FailedToOpen e) = FailedToOpen e
    fmap _ Closing = Closing

-- | Read the status of a 'Resource'.
readStatus :: Resource e a -> STM IO (ResourceStatus e a)
readStatus resource = readTVar (content resource)

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
    content <- newTVarIO Closed
    let waitForEndOfLife = atomically $ do
            state <- readTVar content
            case state of
                Closing -> pure $ Right ()
                Vanished e -> pure $ Left $ Left e
                FailedToOpen e -> pure $ Left $ Right e
                _ -> retry
        resource = Resource{content, waitForEndOfLife}
    action resource `finally` closeResource resource

-- | Error condition for 'onResource'.
data ErrResourceMissing e
    = -- | The 'Resource' has not been initialized yet.
      ErrNotInitialized
    | -- | The 'Resource' is currently being initialized.
      ErrStillInitializing
    | -- | The 'Resource' has not been initialized yet.
      ErrVanished SomeException
    | -- | The 'Resource' has vanished due to an unhandled exception.
      ErrFailedToInitialize e
    -- | The 'Resource' has failed to initialize.
    | ErrClosing
    -- | The 'Resource is currently being closed.
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
        Closed -> pure $ Left ErrNotInitialized
        Opening -> pure $ Left ErrStillInitializing
        Open a -> Right <$> action a
        Vanished e -> pure $ Left $ ErrVanished e
        FailedToOpen e -> pure $ Left $ ErrFailedToInitialize e
        Closing -> pure $ Left ErrClosing

closeResource :: Resource e a -> IO (Either (ErrResourceMissing e) ())
closeResource resource = do
    r <- atomically $ do
        status <- readTVar $ content resource
        case status of
            Closed -> pure $ Right ()
            Opening -> pure $ Left ErrStillInitializing
            Open _ -> do
                writeTVar (content resource) Closing
                pure $ Right ()
            Vanished e -> pure $ Left $ ErrVanished e
            FailedToOpen e -> pure $ Left $ ErrFailedToInitialize e
            Closing -> pure $ Left ErrClosing
    case r of
        Right () -> waitForClose resource
        Left e' -> pure $ Left e'

waitForClose :: Resource e a -> IO (Either (ErrResourceMissing e) ())
waitForClose resource = do
    e <- atomically $ do
        status <- readTVar (content resource)
        case status of
            Closed -> pure $ Right ()
            Vanished e -> pure $ Left $ ErrVanished e
            FailedToOpen e -> pure $ Left $ ErrFailedToInitialize e
            _ -> retry
    case e of
        Right () -> pure $ Right ()
        Left e' -> pure $ Left e'

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
    | -- | The resource 'a' is currently being closed.
        ErrAlreadyClosing
    deriving (Show)

-- | Initialize a 'Resource' using a @with…@ function.
-- This @with…@ function will be called with an argument that does
-- not terminate until 'withResource' terminates.
-- The function can logically fail returning a 'Left' value.
-- Exceptions will be caught and stored in the 'Resource' as well
putResource
    :: (forall b. (a -> IO b) -> IO (Either e b))
    -- ^ Function to initialize the resource 'a'
    -> Resource e a
    -- ^ The 'Resource' to initialize.
    -> IO (Either (ErrResourceExists e a) ())
putResource start resource = do
    forking <- atomically $ do
        ca :: ResourceStatus e a <- readTVar (content resource)
        case ca of
            FailedToOpen e -> pure $ Left $ ErrAlreadyFailedToInitialize e
            Vanished e -> pure $ Left $ ErrAlreadyVanished e
            Opening -> pure $ Left ErrAlreadyInitializing
            Open a -> pure $ Left $ ErrAlreadyInitialized a
            Closed -> do
                writeTVar (content resource) Opening
                pure $ Right forkInitialization
            Closing -> pure $ Left ErrAlreadyClosing
    case forking of
        Left e -> pure $ Left e
        Right action -> Right <$> action
  where
    controlInitialization = do
        r <- start run
        atomically $ case r of
            Right (Right ()) -> do
                writeTVar (content resource) Closed
            Right (Left (Left e)) -> do
                writeTVar (content resource) (Vanished e)
            Right (Left (Right e)) -> do
                writeTVar (content resource) (FailedToOpen e)
            Left e -> do
                writeTVar (content resource) (FailedToOpen e)

    forkInitialization = void $ forkFinally controlInitialization vanish

    run a = do
        atomically $ writeTVar (content resource) (Open a)
        waitForEndOfLife resource

    vanish (Left e) = do
        atomically $ writeTVar (content resource) (Vanished e)
    vanish (Right _) =
        pure () -- waitForEndOfLife has succeeded
