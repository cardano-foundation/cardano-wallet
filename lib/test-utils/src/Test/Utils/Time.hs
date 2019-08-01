{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides utility functions relating to testing with times and dates.

module Test.Utils.Time
    ( UniformTime
    , genUniformTime
    , getUniformTime
    ) where

import Prelude

import Data.Time
    ( Day (ModifiedJulianDay), DiffTime, UTCTime (..) )
import Data.Time.Clock
    ( picosecondsToDiffTime, secondsToDiffTime )
import Test.QuickCheck
    ( Arbitrary, Gen, arbitrary, choose, oneof )

-- | A wrapper for 'UTCTime' whose 'Arbitrary' instance spans a uniform range
--   of dates and a mixture of time precisions.
--
newtype UniformTime = UniformTime { getUniformTime :: UTCTime }
    deriving (Eq, Ord, Show)

instance Arbitrary UniformTime where
    arbitrary = UniformTime <$> genUniformTime

-- | Generate 'UTCTime' values over a uniform range of dates and a mixture of
--   time precisions.
--
genUniformTime :: Gen UTCTime
genUniformTime = oneof
    [ genWith
        hoursToDiffTime
        hoursInOneDay
    , genWith
        secondsToDiffTime
        secondsInOneDay
    , genWith
        picosecondsToDiffTime
        picosecondsInOneDay
    ]
  where
    genWith :: (Integer -> DiffTime) -> Integer -> Gen UTCTime
    genWith unitsToDiffTime unitsInOneDay = do
        numberOfDays <- ModifiedJulianDay
            <$> choose (0, daysInOneThousandYears)
        timeSinceMidnight <- unitsToDiffTime
            <$> choose (0, unitsInOneDay)
        pure $ UTCTime numberOfDays timeSinceMidnight

-- | The approximate number of days in one thousand years.
daysInOneThousandYears :: Integral a => a
daysInOneThousandYears = 365 * 1000

-- | The number of hours in a day.
hoursInOneDay :: Integral a => a
hoursInOneDay = 24

-- | The maximum number of picoseconds in one day, allowing for leap seconds.
picosecondsInOneDay :: Integral a => a
picosecondsInOneDay = secondsInOneDay * picosecondsInOneSecond

-- | The exact number of picoseconds in one second.
picosecondsInOneSecond :: Integral a => a
picosecondsInOneSecond = 1_000_000_000_000

-- | The maximum number of seconds in one day, allowing for leap seconds.
secondsInOneDay :: Integral a => a
secondsInOneDay = (secondsInOneHour * hoursInOneDay) + 1

-- | The exact number of seconds in one hour.
secondsInOneHour :: Integral a => a
secondsInOneHour = 60 * 60

-- | Convert a number of hours into a 'DiffTime' value.
hoursToDiffTime :: Integral a => a -> DiffTime
hoursToDiffTime hours =
    secondsToDiffTime (secondsInOneHour * fromIntegral hours)
