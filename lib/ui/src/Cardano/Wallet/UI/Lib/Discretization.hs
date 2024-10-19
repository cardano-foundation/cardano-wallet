{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Lib.Discretization
    ( nextQuantizedTime
    , quantize
    , quantizeByTime'
    , minKey
    )
where

import Prelude hiding
    ( lookup
    )

import Cardano.Wallet.Deposit.Pure.API.TxHistory
    ( DownTime
    )
import Cardano.Wallet.Deposit.Read
    ( WithOrigin (..)
    )
import Cardano.Wallet.UI.Deposit.API
    ( Window (..)
    )
import Data.Foldable
    ( Foldable (..)
    )
import Data.Map.Monoidal.Strict
    ( MonoidalMap (..)
    )
import Data.Map.Strict
    ()
import Data.Ord
    ( Down (..)
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

import qualified Data.Map.Monoidal.Strict as MonoidalMap

quantizeSeconds :: DiffTime -> Integer -> DiffTime
quantizeSeconds t q =
    q' * fromIntegral @Integer (floor (t / q'))
  where
    q' = secondsToDiffTime q

quantize :: DayOfWeek -> Window -> UTCTime -> UTCTime
quantize _ Year (UTCTime (YearMonthDay y _ _) _) = UTCTime (YearMonthDay y 1 1) 0
quantize _ Month (UTCTime (YearMonthDay y m _) _) = UTCTime (YearMonthDay y m 1) 0
quantize fdk Week (UTCTime d _) = UTCTime d' 0
  where
    d' = weekFirstDay fdk d
quantize _ Day (UTCTime d _) = UTCTime d 0
quantize _ Hour12 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 12 * 3600)
quantize _ Hour6 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 6 * 3600)
quantize _ Hour4 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 4 * 3600)
quantize _ Hour2 (UTCTime d t) = UTCTime d (quantizeSeconds t $ 2 * 3600)
quantize _ Hour1 (UTCTime d t) = UTCTime d (quantizeSeconds t 3600)
quantize _ Minute30 (UTCTime d t) = UTCTime d (quantizeSeconds t 1800)
quantize _ Minute15 (UTCTime d t) = UTCTime d (quantizeSeconds t 900)
quantize _ Minute10 (UTCTime d t) = UTCTime d (quantizeSeconds t 600)
quantize _ Minute5 (UTCTime d t) = UTCTime d (quantizeSeconds t 300)

nextQuantizedTime :: DayOfWeek -> Window -> UTCTime -> UTCTime
nextQuantizedTime _ Year (UTCTime d s) = UTCTime (addGregorianYearsRollOver (-1) d) s
nextQuantizedTime _ Month (UTCTime d s) = UTCTime (addGregorianMonthsRollOver (-1) d) s
nextQuantizedTime fdk Week (UTCTime d s) = UTCTime (weekFirstDay fdk $ addDays (-7) d) s
nextQuantizedTime _ Day (UTCTime d s) = UTCTime (addDays (-1) d) s
nextQuantizedTime _ Hour12 (UTCTime d s) = UTCTime d (s - 12 * 3600)
nextQuantizedTime _ Hour6 (UTCTime d s) = UTCTime d (s - 6 * 3600)
nextQuantizedTime _ Hour4 (UTCTime d s) = UTCTime d (s - 4 * 3600)
nextQuantizedTime _ Hour2 (UTCTime d s) = UTCTime d (s - 2 * 3600)
nextQuantizedTime _ Hour1 (UTCTime d s) = UTCTime d (s - 3600)
nextQuantizedTime _ Minute30 (UTCTime d s) = UTCTime d (s - 1800)
nextQuantizedTime _ Minute15 (UTCTime d s) = UTCTime d (s - 900)
nextQuantizedTime _ Minute10 (UTCTime d s) = UTCTime d (s - 600)
nextQuantizedTime _ Minute5 (UTCTime d s) = UTCTime d (s - 300)

minKey :: MonoidalMap k a -> Maybe k
minKey = fmap fst . MonoidalMap.lookupMin

quantizeByTime'
    :: Monoid a
    => DayOfWeek
    -> Window
    -> MonoidalMap DownTime a
    -> MonoidalMap DownTime a
quantizeByTime' fdk w mm = case MonoidalMap.lookupMin mm of
    Just ((Down (At t)), _) ->
        let
            t' = quantize fdk w t
            nt = Down $ At $ nextQuantizedTime fdk w t'
            (before, match, after) = MonoidalMap.splitLookup nt mm
            after' =
                maybe
                    after
                    (\v -> MonoidalMap.insert nt v after)
                    match
        in
            MonoidalMap.singleton (Down (At t')) (fold before)
                <> quantizeByTime' fdk w after'
    Just (Down Origin, _) ->
        let
            (before, match, after) = MonoidalMap.splitLookup (Down Origin) mm
            before' =
                maybe
                    before
                    (\v -> MonoidalMap.insert (Down Origin) v before)
                    match
        in
            MonoidalMap.singleton (Down Origin) (fold before')
                <> quantizeByTime' fdk w after
    Nothing -> MonoidalMap.empty
