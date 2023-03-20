{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

{- HLINT ignore "Use newtype instead of data" -}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a utility for ordering concurrent actions
-- via locks.
module Control.Concurrent.Concierge
    ( Concierge
    , newConcierge
    , atomicallyWith
    , atomicallyWithLifted
    )
    where

import Prelude

import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    , TVar
    , atomically
    , modifyTVar
    , newTVarIO
    , readTVar
    , retry
    , writeTVar
    )
import Control.Monad.Class.MonadFork
    ( MonadThread, ThreadId, myThreadId )
import Control.Monad.Class.MonadThrow
    ( MonadThrow, bracket )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Map.Strict
    ( Map )

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
    Concierge
-------------------------------------------------------------------------------}
-- | At a 'Concierge', you can obtain a lock and
-- enforce sequential execution of concurrent 'IO' actions.
--
-- Back in the old days, hotel concierges used to give out keys.
-- But after the cryptocurrency revolution, they give out locks. :)
-- (The term /lock/ is standard terminology in concurrent programming.)
data Concierge m lock = Concierge
    { locks :: TVar m (Map lock (ThreadId m))
    }

-- | Create a new 'Concierge' that keeps track of locks.
newConcierge :: MonadSTM m => m (Concierge m lock)
newConcierge = Concierge <$> newTVarIO Map.empty

-- | Obtain a lock from a 'Concierge' and run an 'IO' action.
--
-- If the same (equal) lock is already taken at this 'Concierge',
-- the thread will be blocked until the lock becomes available.
--
-- The action may throw a synchronous or asynchronous exception.
-- In both cases, the lock is returned to the concierge.
atomicallyWith
    :: (Ord lock, MonadIO m, MonadThrow m)
    => Concierge IO lock -> lock -> m a -> m a
atomicallyWith = atomicallyWithLifted liftIO

-- | More polymorphic version of 'atomicallyWith'.
atomicallyWithLifted
    :: (Ord lock, MonadSTM m, MonadThread m, MonadThrow n)
    => (forall b. m b -> n b)
    -> Concierge m lock -> lock -> n a -> n a
atomicallyWithLifted lift Concierge{locks} lock action =
    bracket acquire (const release) (const action)
  where
    acquire = lift $ do
        tid <- myThreadId
        atomically $ do
            ls <- readTVar locks
            case Map.lookup lock ls of
                Just _  -> retry
                Nothing -> writeTVar locks $ Map.insert lock tid ls
    release = lift $
        atomically $ modifyTVar locks $ Map.delete lock
