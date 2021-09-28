{-# LANGUAGE DeriveGeneric #-}
-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a utility for caching the results of long running actions.
module Control.Cache
    ( CacheConfig (..)
    , CacheWorker
    , readCache
    , invalidateCache
    , runWorker

    , MkCacheWorker
    , newCacheWorker
    , don'tCacheWorker

    , threadWait

    , module Data.Time.Clock
    )
    where

import Prelude

import Control.Monad
    ( forever, void )
import Data.Time.Clock
    ( NominalDiffTime )
import Data.Void
    ( Void )
import GHC.Generics
    ( Generic )
import UnliftIO
    ( MonadIO )
import UnliftIO.Async
    ( race )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( catchAny, throwIO )
import UnliftIO.STM
    ( atomically, newTVarIO, readTVar, retrySTM, writeTVar )

{-------------------------------------------------------------------------------
    Cache Worker
-------------------------------------------------------------------------------}
-- | Caching behavior configuration.
data CacheConfig
    = NoCache
    -- ^ The value is not cached at all.
    | CacheTTL NominalDiffTime
    -- ^ The value is cached immediately
    -- and re-requested after the time to live (TTL) has passed.
    deriving (Eq, Ord, Show, Generic)

-- | A 'CacheWorker'
-- runs a given action periodically and caches the result.
data CacheWorker a = CacheWorker
    { _readCache :: IO a
    , _invalidateCache :: IO ()
    , _runWorker :: IO Void
    }

-- | Request the cached value.
--
-- The 'readCache' action will block if the cache has not been filled yet.
readCache :: CacheWorker a -> IO a
readCache = _readCache

-- | Invalidate the cached value.
--
-- The worker will stop waiting and execute the action to be cached again.
invalidateCache :: CacheWorker a -> IO ()
invalidateCache = _invalidateCache

-- | Run the worker that periodically executes
-- the function and fills the cache.
--
-- The 'runWorker' action should be called only once.
-- It will not return.
--
-- The function which is to be cached may throw exceptions:
--
-- * Any synchronous exception will be treated as a return value:
--   the exception is stored in the cache and rethrown when attempting
--   to read the cache.
-- * Any asynchronous exception is meant for the worker thread
--   and will terminate it; the cache will be left in its current state
--   (unfilled or stale).
runWorker :: CacheWorker a -> IO Void
runWorker = _runWorker

-- | Type synonym for an action that creates a 'CacheWorker'.
type MkCacheWorker a = IO a -> IO (CacheWorker a)

-- | Create a new cache worker from an action.
newCacheWorker
    :: NominalDiffTime -- ^ cache time to live (TTL)
    -> NominalDiffTime -- ^ grace period before calling the action the first time
    -> IO a -- ^ action whose result we want to cache
    -> IO (CacheWorker a)
newCacheWorker ttl gracePeriod action = do
    cache <- newTVarIO Nothing
    invalidated <- newTVarIO False

    let worker :: IO Void
        worker = forever $ do
            threadWait gracePeriod
            ea <- (Right <$> action) `catchAny` (pure . Left)
            writeCache cache ea
            void $ race (threadWait (max 0 ttl)) (stopWhen invalidated)
        writeCache v x = atomically $ do
            writeTVar invalidated False
            writeTVar v $ Just x
        _readCache v = do
            ea <- atomically (readTVar v >>= maybe retrySTM pure)
            either throwIO pure ea

    pure $ CacheWorker
        { _readCache = _readCache cache
        , _invalidateCache = atomically $ writeTVar invalidated True
        , _runWorker = worker
        }
  where
    stopWhen v = atomically $ do
        stop <- readTVar v
        if stop then pure () else retrySTM

-- | For testing: A worker that does not run anything,
-- the action is simply performed each time that its result is requested.
don'tCacheWorker :: MkCacheWorker a
don'tCacheWorker action = pure $ CacheWorker
    { _readCache = action
    , _invalidateCache = pure ()
    , _runWorker = forever $ threadWait (24*60*60)
    }

-- | Variant of 'threadDelay' where the argument has type 'NominalDiffTime'.
--
-- The resolution for delaying threads is microseconds.
threadWait :: MonadIO m => NominalDiffTime -> m ()
threadWait s = threadDelay $ round (s / microsecond)
  where microsecond = 1e-6 :: NominalDiffTime
