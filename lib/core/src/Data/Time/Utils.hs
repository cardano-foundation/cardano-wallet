{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Utility functions for manipulating time values.

module Data.Time.Utils
    ( nominalDiffTimeToMicroseconds
    , utcTimePred
    , utcTimeSucc
    , waitUntil
    ) where

import Prelude

import Control.Concurrent
    ( threadDelay )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Data.Time
    ( NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime )

-- | Converts the specified time difference into an integral number of
--   microseconds.
--
nominalDiffTimeToMicroseconds :: Integral i => NominalDiffTime -> i
nominalDiffTimeToMicroseconds = ceiling . (* 1_000_000) . toRational

-- | For a given time 't0', get the closest representable time 't1' to 't0'
--   for which 't0 < t1'.
utcTimeSucc :: UTCTime -> UTCTime
utcTimeSucc = addUTCTime $ succ 0

-- | For a given time 't0', get the closest representable time 't1' to 't0'
--   for which 't1 < t0'.
utcTimePred :: UTCTime -> UTCTime
utcTimePred = addUTCTime $ pred 0

-- | Suspends the current thread until after the specified time has passed.
--
-- There is no guarantee that the thread will be rescheduled promptly when the
-- delay has expired, but the thread will never continue to run earlier than
-- specified.
--
waitUntil :: MonadIO m => UTCTime -> m ()
waitUntil futureTime = liftIO $ do
    currentTime <- getCurrentTime
    threadDelay
        $ nominalDiffTimeToMicroseconds
        $ futureTime `diffUTCTime` currentTime
