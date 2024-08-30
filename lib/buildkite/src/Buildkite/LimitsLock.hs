{-# LANGUAGE RecordWildCards #-}

module Buildkite.LimitsLock
    ( LimitsLock (..)
    , SetLimit
    , LimitsLockLog (..)
    , newLimitsLock
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay
    )
import Control.Concurrent.STM
    ( atomically
    , newTVarIO
    , readTVar
    , retry
    , writeTVar
    )
import Control.Monad
    ( join
    , when
    )
import Control.Tracer
    ( Tracer
    , traceWith
    )
import Data.Functor
    ( ($>)
    )

-- | Set the limit of requests to a resource. This specific interface is dictated
-- by buildkite's API, but it could be generalized to any rate limiting system.
type SetLimit =
    Int
    -- ^ Remaining requests.
    -> Int
    -- ^ Seconds to wait before the allowed limit is reset.
    -> IO ()

-- | A lock to limit the number of requests to a resource.
data LimitsLock = LimitsLock
    { setLimit :: SetLimit
    -- ^ Try to lock the resource.
    , checkLimit :: IO ()
    -- ^ Check if the resource is locked and wait until it is available.
    }

-- | Log messages for 'LimitsLock'.
newtype LimitsLockLog = RateLimitReached Int
    deriving (Show)

-- | Create a new 'LimitsLock'. To simplify the implementation, the setLimit is
-- implemented as blocking. A more sophisticated implementation could use
-- a thread to reset the lock after the time has passed.
newLimitsLock
    :: Tracer IO LimitsLockLog
    -- ^ Tracer for logging messages.
    -> Int
    -- ^ Number of remaining requests to consider as the limit to block the resource.
    -> IO LimitsLock
newLimitsLock tracer safeRemaining = do
    lock <- newTVarIO False
    let
        pass = pure ()
        block secs = do
            traceWith tracer $ RateLimitReached secs
            threadDelay $ secs * 1000000
            atomically $ writeTVar lock False
        setLimit remaining secs = join $ atomically $ do
            locked <- readTVar lock
            if locked
                then pure pass
                else
                    if remaining < safeRemaining
                        then writeTVar lock True $> block secs
                        else pure pass
        checkLimit = do
            atomically $ do
                locked <- readTVar lock
                when locked retry
    pure $ LimitsLock{..}
