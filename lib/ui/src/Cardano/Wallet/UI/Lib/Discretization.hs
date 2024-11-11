{-# LANGUAGE PatternSynonyms #-}

module Cardano.Wallet.UI.Lib.Discretization
    ( nextDiscretizedTime
    , discretizeTime
    , Window (..)
    )
where

import Prelude hiding
    ( lookup
    )

import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( DayOfWeek
    , DiffTime
    , NominalDiffTime
    , UTCTime (..)
    , addUTCTime
    , secondsToDiffTime
    , weekFirstDay
    , pattern YearMonthDay
    )

-- | A time window for discretization.
data Window
    = Minute5
    | Minute10
    | Minute15
    | Minute30
    | Hour1
    | Hour2
    | Hour4
    | Hour6
    | Hour12
    | Day
    | Week
    | Month
    | Year
    deriving (Eq, Show, Enum, Bounded)

instance ToText Window where
    toText Minute5 = "5 minutes"
    toText Minute10 = "10 minutes"
    toText Minute15 = "15 minutes"
    toText Minute30 = "30 minutes"
    toText Hour1 = "1 hour"
    toText Hour2 = "2 hours"
    toText Hour4 = "4 hours"
    toText Hour6 = "6 hours"
    toText Hour12 = "12 hours"
    toText Day = "1 day"
    toText Week = "1 week"
    toText Month = "1 month"
    toText Year = "1 year"

discretizeSeconds :: DiffTime -> Integer -> DiffTime
discretizeSeconds t q =
    q' * fromIntegral (floor (t / q') :: Integer)
  where
    q' = secondsToDiffTime q

minSecondsOfWindow :: Window -> NominalDiffTime
minSecondsOfWindow Minute5 = 5 * 60
minSecondsOfWindow Minute10 = 10 * 60
minSecondsOfWindow Minute15 = 15 * 60
minSecondsOfWindow Minute30 = 30 * 60
minSecondsOfWindow Hour1 = 3600
minSecondsOfWindow Hour2 = 2 * 3600
minSecondsOfWindow Hour4 = 4 * 3600
minSecondsOfWindow Hour6 = 6 * 3600
minSecondsOfWindow Hour12 = 12 * 3600
minSecondsOfWindow Day = 24 * 3600
minSecondsOfWindow Week = 7 * 24 * 3600
minSecondsOfWindow Month = 31 * 24 * 3600
minSecondsOfWindow Year = 366 * 24 * 3600 -- no idea ...

-- | Discretize a time according to a given window.
discretizeTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
discretizeTime _ Year (UTCTime (YearMonthDay y _ _) _) =
    UTCTime (YearMonthDay y 1 1) 0
discretizeTime _ Month (UTCTime (YearMonthDay y m _) _) =
    UTCTime (YearMonthDay y m 1) 0
discretizeTime fdk Week (UTCTime d _) = UTCTime d' 0
  where
    d' = weekFirstDay fdk d
discretizeTime _ Day (UTCTime d _) = UTCTime d 0
discretizeTime _ Hour12 (UTCTime d t) =
    UTCTime d (discretizeSeconds t $ 12 * 3600)
discretizeTime _ Hour6 (UTCTime d t) =
    UTCTime d (discretizeSeconds t $ 6 * 3600)
discretizeTime _ Hour4 (UTCTime d t) =
    UTCTime d (discretizeSeconds t $ 4 * 3600)
discretizeTime _ Hour2 (UTCTime d t) =
    UTCTime d (discretizeSeconds t $ 2 * 3600)
discretizeTime _ Hour1 (UTCTime d t) = UTCTime d (discretizeSeconds t 3600)
discretizeTime _ Minute30 (UTCTime d t) = UTCTime d (discretizeSeconds t 1800)
discretizeTime _ Minute15 (UTCTime d t) = UTCTime d (discretizeSeconds t 900)
discretizeTime _ Minute10 (UTCTime d t) = UTCTime d (discretizeSeconds t 600)
discretizeTime _ Minute5 (UTCTime d t) = UTCTime d (discretizeSeconds t 300)

-- | Compute the next discretized time after a given time. This will work on non
-- discretized times as well, but will potentially bread on the last day of 2025
-- as addGregorianMonthsRollOver (-1) "2016-12-31" == "2016-12-01"
-- OTOH when used after discretization it will be fine
nextDiscretizedTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
nextDiscretizedTime fdk window time =
    -- subtract 2 seconds to jump past a leap second
    discretizeTime
        fdk
        window
        ( addUTCTime
            (minSecondsOfWindow window + 2)
            timeDiscretizedOnce
        )
  where
    timeDiscretizedOnce = discretizeTime fdk window time
