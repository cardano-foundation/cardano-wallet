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
    ) where

import Prelude

import Data.Time
    ( NominalDiffTime, UTCTime, addUTCTime )

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
