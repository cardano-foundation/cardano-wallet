{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Lib.Discretization
    ( nextDiscretizedTime
    , discretizeTime
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.UI.Deposit.API
    ( Window (..)
    )
import Data.Time
    ( DayOfWeek
    , DiffTime
    , UTCTime (..)
    , addDays
    , addGregorianMonthsRollOver
    , addGregorianYearsRollOver
    , pattern YearMonthDay
    , secondsToDiffTime
    , weekFirstDay
    )

discretizeSeconds :: DiffTime -> Integer -> DiffTime
discretizeSeconds t q =
    q' * fromIntegral @Integer (floor (t / q'))
  where
    q' = secondsToDiffTime q

discretizeTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
discretizeTime _ Year (UTCTime (YearMonthDay y _ _) _) = UTCTime (YearMonthDay y 1 1) 0
discretizeTime _ Month (UTCTime (YearMonthDay y m _) _) = UTCTime (YearMonthDay y m 1) 0
discretizeTime fdk Week (UTCTime d _) = UTCTime d' 0
  where
    d' = weekFirstDay fdk d
discretizeTime _ Day (UTCTime d _) = UTCTime d 0
discretizeTime _ Hour12 (UTCTime d t) = UTCTime d (discretizeSeconds t $ 12 * 3600)
discretizeTime _ Hour6 (UTCTime d t) = UTCTime d (discretizeSeconds t $ 6 * 3600)
discretizeTime _ Hour4 (UTCTime d t) = UTCTime d (discretizeSeconds t $ 4 * 3600)
discretizeTime _ Hour2 (UTCTime d t) = UTCTime d (discretizeSeconds t $ 2 * 3600)
discretizeTime _ Hour1 (UTCTime d t) = UTCTime d (discretizeSeconds t 3600)
discretizeTime _ Minute30 (UTCTime d t) = UTCTime d (discretizeSeconds t 1800)
discretizeTime _ Minute15 (UTCTime d t) = UTCTime d (discretizeSeconds t 900)
discretizeTime _ Minute10 (UTCTime d t) = UTCTime d (discretizeSeconds t 600)
discretizeTime _ Minute5 (UTCTime d t) = UTCTime d (discretizeSeconds t 300)

nextDiscretizedTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
nextDiscretizedTime _ Year (UTCTime d s) = UTCTime (addGregorianYearsRollOver (-1) d) s
nextDiscretizedTime _ Month (UTCTime d s) = UTCTime (addGregorianMonthsRollOver (-1) d) s
nextDiscretizedTime fdk Week (UTCTime d s) = UTCTime (weekFirstDay fdk $ addDays (-7) d) s
nextDiscretizedTime _ Day (UTCTime d s) = UTCTime (addDays (-1) d) s
nextDiscretizedTime _ Hour12 (UTCTime d s) = UTCTime d (s - 12 * 3600)
nextDiscretizedTime _ Hour6 (UTCTime d s) = UTCTime d (s - 6 * 3600)
nextDiscretizedTime _ Hour4 (UTCTime d s) = UTCTime d (s - 4 * 3600)
nextDiscretizedTime _ Hour2 (UTCTime d s) = UTCTime d (s - 2 * 3600)
nextDiscretizedTime _ Hour1 (UTCTime d s) = UTCTime d (s - 3600)
nextDiscretizedTime _ Minute30 (UTCTime d s) = UTCTime d (s - 1800)
nextDiscretizedTime _ Minute15 (UTCTime d s) = UTCTime d (s - 900)
nextDiscretizedTime _ Minute10 (UTCTime d s) = UTCTime d (s - 600)
nextDiscretizedTime _ Minute5 (UTCTime d s) = UTCTime d (s - 300)
