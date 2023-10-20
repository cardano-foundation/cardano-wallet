{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides utility functions relating to testing with times and dates.

module Test.Utils.Time
    ( UniformTime
    , genUniformTime
    , genUniformTimeWithinRange
    , getUniformTime
    ) where

import Prelude

import Data.Time
    ( Day (ModifiedJulianDay)
    , NominalDiffTime
    , UTCTime (..)
    , addUTCTime
    , toModifiedJulianDay
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , choose
    , oneof
    )

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
-- Dates will be generated in a range that's bounded by 'defaultLowerBound' and
-- 'defaultUpperBound'.

genUniformTime :: Gen UTCTime
genUniformTime = genUniformTimeWithinRange defaultLowerBound defaultUpperBound

-- | Generate 'UTCTime' values over a uniform range of dates and a mixture of
--   time precisions.
--
-- Dates will be generated in a range that's bounded by the given minimum and
-- maximum Julian day arguments.
--
genUniformTimeWithinRange :: Day -> Day -> Gen UTCTime
genUniformTimeWithinRange lowerBound upperBound
    | lowerBound > upperBound = error $
        "genUniformTimeWithinRange: invalid bounds: "
            <> show (lowerBound, upperBound)
    | otherwise = oneof
        [ genWith
            hoursToNominalDiffTime
            hoursInOneDay
        , genWith
            secondsToNominalDiffTime
            secondsInOneDay
        , genWith
            picosecondsToNominalDiffTime
            picosecondsInOneDay
        ]
  where
    genWith :: (Integer -> NominalDiffTime) -> Integer -> Gen UTCTime
    genWith unitsToNominalDiffTime unitsInOneDay = do
        numberOfDays <- ModifiedJulianDay
            <$> choose
                ( toModifiedJulianDay lowerBound
                , toModifiedJulianDay upperBound
                )
        timeSinceMidnight <- unitsToNominalDiffTime
            <$> choose (0, unitsInOneDay)
        pure $ addUTCTime timeSinceMidnight (UTCTime numberOfDays 0)

defaultLowerBound :: Day
defaultLowerBound = ModifiedJulianDay 0

defaultUpperBound :: Day
defaultUpperBound = ModifiedJulianDay $ 365 * 50

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

-- | Convert a number of hours into a 'NominalDiffTime' value.
hoursToNominalDiffTime :: Integral a => a -> NominalDiffTime
hoursToNominalDiffTime = fromIntegral . (secondsInOneHour *)

-- | Convert a number of picoseconds into a 'NominalDiffTime' value.
picosecondsToNominalDiffTime :: Integral a => a -> NominalDiffTime
picosecondsToNominalDiffTime = toEnum . fromIntegral

-- | Convert a number of seconds into a 'NominalDiffTime' value.
secondsToNominalDiffTime :: Integral a => a -> NominalDiffTime
secondsToNominalDiffTime = fromIntegral
