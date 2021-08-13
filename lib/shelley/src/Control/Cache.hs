-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
-- This module provides a utility for caching the results of long running actions.
module Control.Cache
    ( CacheWorker (..)
    , newCacheWorker
    , don'tCacheWorker

    , threadWait

    , module Data.Time.Clock
    )
    where

import Prelude

import Control.Monad
    ( forever )
import Data.Time.Clock
    ( NominalDiffTime )
import UnliftIO
    ( MonadIO )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( catchAny, throwIO )
import UnliftIO.STM
    ( atomically
    , newTVarIO
    , readTVar
    , retrySTM
    , writeTVar
    )

{-------------------------------------------------------------------------------
    Cache Worker
-------------------------------------------------------------------------------}
-- | A worker (an action of type @IO ()@) that
-- runs a function periodically and caches the result.
newtype CacheWorker = CacheWorker { runCacheWorker :: IO () }

-- | Run an action periodically and cache the results.
-- 
-- Requesting the cached value before the cache has
-- been filled will lead to waiting.
-- 
-- The action may throw exceptions:
-- * Any synchronous exception will be treated as a return value:
--   the exception is stored in the cache and rethrown when attempting
--   to read the cache.
-- * Any asynchronous exception is meant for the worker thread
--   and will terminate it; the cache will be left in its current state
--   (unfilled or stale).
newCacheWorker
    :: NominalDiffTime -- ^ cache time to live (TTL)
    -> NominalDiffTime -- ^ grace period before calling the action the first time
    -> IO a -- ^ action whose result we want to cache
    -> IO (CacheWorker, IO a)
    -- ^ (worker thread that fills the cache, action to request the cache)
newCacheWorker ttl gracePeriod action = do
    cache <- newTVarIO Nothing
    let worker :: IO ()
        worker = forever $ do
            threadWait gracePeriod
            ea <- (Right <$> action) `catchAny` (pure . Left)
            writeCache cache ea
            threadWait $ max 0 ttl
    return (CacheWorker worker, readCache cache)
  where
    writeCache v = atomically . writeTVar v . Just
    readCache  v = do
        ea <- atomically (readTVar v >>= maybe retrySTM pure)
        either throwIO pure ea

-- | For testing: A worker that does not run anything,
-- the action is simply performed each time that its result is requested.
don'tCacheWorker :: NominalDiffTime -> IO a -> IO (CacheWorker, IO a)
don'tCacheWorker _ action = pure (CacheWorker $ pure (), action)

-- | Variant of 'threadDelay' where the argument has type 'NominalDiffTime'.
--
-- The resolution for delaying threads is microseconds.
threadWait :: MonadIO m => NominalDiffTime -> m ()
threadWait s = threadDelay $ round (s / microsecond)
  where microsecond = 1e-6 :: NominalDiffTime
