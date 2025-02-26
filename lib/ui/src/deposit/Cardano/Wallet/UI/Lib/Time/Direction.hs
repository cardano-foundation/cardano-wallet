{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.UI.Lib.Time.Direction where

import Prelude hiding
    ( span
    )

import Data.List
    ( sortBy
    )
import Data.Ord
    ( Down (..)
    , comparing
    )
import Data.Time
    ( UTCTime (..)
    , addGregorianMonthsClip
    , fromGregorian
    )

data Direction = Asc | Desc

sortByDirection :: Ord b => Direction -> (a -> b) -> [a] -> [a]
sortByDirection Asc f = sortBy (comparing f)
sortByDirection Desc f = sortBy (comparing (Down . f))

data Match b = Match b | NoMatch | DirectionMatch

match :: (b -> a) -> a -> a -> Match b -> a
match check no checkD = \case
    Match w -> check w
    NoMatch -> no
    DirectionMatch -> checkD

filterByDirection :: Ord b => Direction -> b -> (a -> Match b) -> [a] -> [a]
filterByDirection Asc w f = filter (match (>= w) False False . f)
filterByDirection Desc w f = filter (match (<= w) False True . f)

utcTimeByDirection :: Direction -> Int -> Int -> UTCTime
utcTimeByDirection dir year month =
    UTCTime (correction day) 0
  where
    day = fromGregorian (fromIntegral year) month 1
    correction = case dir of
        Asc -> id
        Desc -> addGregorianMonthsClip 1
