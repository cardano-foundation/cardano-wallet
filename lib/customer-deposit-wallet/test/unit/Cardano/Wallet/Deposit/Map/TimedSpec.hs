{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Wallet.Deposit.Map.TimedSpec where

import Prelude

import Cardano.Wallet.Deposit.Map.Timed
    ( Timed (..)
    , TimedSeq
    , maxKey
    , minKey
    , takeAfter
    , takeBefore
    )
import Data.FingerTree
    ( fromList
    , (<|)
    )
import Data.List
    ( sort
    , unfoldr
    )
import Data.Monoid
    ( Last (..)
    , Sum (..)
    )
import Data.Time
    ( UTCTime (..)
    , defaultTimeLocale
    , parseTimeOrError
    , pattern YearMonthDay
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )

type UTimed = Timed UTCTime (Sum Int)

t :: String -> UTCTime
t =
    parseTimeOrError False defaultTimeLocale "%Y-%m-%d %H:%M:%S"

mkTimed :: String -> Int -> UTimed
mkTimed s i = Timed (Last $ Just $ t s) (Sum i)

t0 :: UTimed
t0 = mkTimed "2021-01-01 00:00:00" 1

t1 :: UTimed
t1 = mkTimed "2021-01-02 00:00:00" 2

t2 :: UTimed
t2 = mkTimed "2021-02-01 00:00:00" 3

t3 :: UTimed
t3 = mkTimed "2021-02-02 00:00:00" 4

t4 :: UTimed
t4 = mkTimed "2022-02-03 00:00:00" 5

t5 :: UTimed
t5 = mkTimed "2022-02-03 12:00:00" 6

t6 :: UTimed
t6 = mkTimed "2022-03-03 12:00:00" 7

t7 :: UTimed
t7 = mkTimed "2022-03-05 12:00:00" 8

ts :: [UTimed]
ts = sort [t0, t1, t2, t3, t4, t5, t6, t7]

result
    :: [UTimed]
    -> Maybe UTimed
    -> (TimedSeq UTCTime (Sum Int), Maybe UTCTime)
result included next = (foldr (<|) mempty included, nextTime)
  where
    nextTime = do
        Timed x _ <- next
        getLast x

results :: [[UTimed]] -> [TimedSeq UTCTime (Sum Int)]
results = fmap (foldr (<|) mempty)

byYear :: UTCTime -> Integer
byYear (UTCTime (YearMonthDay y _ _) _) = y

byMonth :: UTCTime -> (Integer, Int)
byMonth (UTCTime (YearMonthDay y m _) _) = (y, m)

byDay :: UTCTime -> (Integer, Int, Int)
byDay (UTCTime (YearMonthDay y m d) _) = (y, m, d)

scroll
    :: (TimedSeq t a -> Maybe t)
    -> ( (t -> q)
         -> Maybe t
         -> Maybe Int
         -> TimedSeq t a
         -> (TimedSeq t a, Maybe t)
       )
    -> (t -> q)
    -> Int
    -> TimedSeq t a
    -> [TimedSeq t a]
scroll boot extract bucket count pager = unfoldr f $ boot pager
  where
    f Nothing = Nothing
    f (Just start) = case extract bucket (Just start) (Just count) pager of
        (m, Just next) -> Just (m, Just next)
        (m, Nothing) -> Just (m, Nothing)

spec :: Spec
spec = do
    describe "finger tree pager front" $ do
        it "can extract without start" $ do
            takeAfter
                byDay
                Nothing
                (Just 1)
                (fromList ts)
                `shouldBe` result [t0] (Just t1)
        it "can extract without count" $ do
            takeAfter
                byDay
                (Just $ t "2021-01-01 00:00:00")
                Nothing
                (fromList ts)
                `shouldBe` result [t0, t1, t2, t3, t4 <> t5, t6, t7] Nothing
        it "can extract 1 day" $ do
            takeAfter
                byDay
                (Just $ t "2021-01-01 00:00:00")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t0] (Just t1)
        it "can extract 2 days" $ do
            takeAfter
                byDay
                (Just $ t "2021-01-01 00:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result [t0, t1] (Just t2)
        it "can extract 5 days" $ do
            takeAfter
                byDay
                (Just $ t "2021-01-01 00:00:00")
                (Just 5)
                (fromList ts)
                `shouldBe` result [t0, t1, t2, t3, t4 <> t5] (Just t6)
        it "can extract 1 month" $ do
            takeAfter
                byMonth
                (Just $ t "2021-01-01 00:00:00")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t0 <> t1] (Just t2)

        it "can extract 2 months" $ do
            takeAfter
                byMonth
                (Just $ t "2021-01-01 00:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result [t0 <> t1, t2 <> t3] (Just t4)

        it "can extract 3 months" $ do
            takeAfter
                byMonth
                (Just $ t "2021-01-01 00:00:00")
                (Just 3)
                (fromList ts)
                `shouldBe` result [t0 <> t1, t2 <> t3, t4 <> t5] (Just t6)

        it "can extract 1 year" $ do
            takeAfter
                byYear
                (Just $ t "2021-01-01 00:00:00")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t0 <> t1 <> t2 <> t3] (Just t4)

        it "can extract 2 years" $ do
            takeAfter
                byYear
                (Just $ t "2021-01-01 00:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result [t0 <> t1 <> t2 <> t3, t4 <> t5 <> t6 <> t7] Nothing

        it "can extract 3 years" $ do
            takeAfter
                byYear
                (Just $ t "2021-01-01 00:00:00")
                (Just 3)
                (fromList ts)
                `shouldBe` result [t0 <> t1 <> t2 <> t3, t4 <> t5 <> t6 <> t7] Nothing

        it "can extract 1 day after t0" $ do
            takeAfter
                byDay
                (Just $ t "2021-01-01 00:00:01")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t1] (Just t2)

        it "can extract 1 month after t0" $ do
            takeAfter
                byMonth
                (Just $ t "2021-01-01 00:00:01")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t1] (Just t2)

        it "can extract 1 year after t0" $ do
            takeAfter
                byYear
                (Just $ t "2021-01-01 00:00:01")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t1 <> t2 <> t3] (Just t4)
    describe "finger tree pager back" $ do
        it "can extract without start" $ do
            takeBefore
                byDay
                Nothing
                (Just 1)
                (fromList ts)
                `shouldBe` result [t7] (Just t6)
        it "can extract without count" $ do
            takeBefore
                byDay
                (Just $ t "2022-03-05 12:00:00")
                Nothing
                (fromList ts)
                `shouldBe` result [t7, t6, t4 <> t5, t3, t2, t1, t0] Nothing
        it "can extract 1 day" $ do
            takeBefore
                byDay
                (Just $ t "2022-03-05 12:00:00")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t7] (Just t6)
        it "can extract 2 days" $ do
            takeBefore
                byDay
                (Just $ t "2022-03-05 12:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result [t7, t6] (Just t5)
        it "can extract 3 days" $ do
            takeBefore
                byDay
                (Just $ t "2022-03-05 12:00:00")
                (Just 3)
                (fromList ts)
                `shouldBe` result [t7, t6, t4 <> t5] (Just t3)
        it "can extract 1 month" $ do
            takeBefore
                byMonth
                (Just $ t "2022-03-05 12:00:00")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t6 <> t7] (Just t5)
        it "can extract 2 months" $ do
            takeBefore
                byMonth
                (Just $ t "2022-03-05 12:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result [t6 <> t7, t4 <> t5] (Just t3)
        it "can extract 2 years" $ do
            takeBefore
                byYear
                (Just $ t "2022-03-05 12:00:00")
                (Just 2)
                (fromList ts)
                `shouldBe` result
                    [t4 <> t5 <> t6 <> t7, t0 <> t1 <> t2 <> t3]
                    Nothing
        it "can extract 1 day before t7" $ do
            takeBefore
                byDay
                (Just $ t "2022-03-05 11:59:59")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t6] (Just t5)
        it "can extract 1 month before t6" $ do
            takeBefore
                byMonth
                (Just $ t "2022-03-03 11:59:59")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t4 <> t5] (Just t3)
        it "can extract 1 year before t4" $ do
            takeBefore
                byYear
                (Just $ t "2022-01-02 23:59:59")
                (Just 1)
                (fromList ts)
                `shouldBe` result [t0 <> t1 <> t2 <> t3] Nothing

    describe "finger tree pager scroll" $ do
        it "can consume scrolling forward by 1 day" $ do
            scroll minKey takeAfter byDay 1 (fromList ts)
                `shouldBe` results
                    [ [t0]
                    , [t1]
                    , [t2]
                    , [t3]
                    , [t4 <> t5]
                    , [t6]
                    , [t7]
                    ]
        it "can consume scrolling backward by 1 day" $ do
            scroll maxKey takeBefore byDay 1 (fromList ts)
                `shouldBe` results
                    [ [t7]
                    , [t6]
                    , [t4 <> t5]
                    , [t3]
                    , [t2]
                    , [t1]
                    , [t0]
                    ]
        it "can consume scrolling forward by 1 month" $ do
            scroll minKey takeAfter byMonth 1 (fromList ts)
                `shouldBe` results
                    [ [t0 <> t1]
                    , [t2 <> t3]
                    , [t4 <> t5]
                    , [t6 <> t7]
                    ]
        it "can consume scrolling backward by 1 month" $ do
            scroll maxKey takeBefore byMonth 1 (fromList ts)
                `shouldBe` results
                    [ [t6 <> t7]
                    , [t4 <> t5]
                    , [t2 <> t3]
                    , [t0 <> t1]
                    ]
        it "can consume scrolling forward by 1 year" $ do
            scroll minKey takeAfter byYear 1 (fromList ts)
                `shouldBe` results
                    [ [t0 <> t1 <> t2 <> t3]
                    , [t4 <> t5 <> t6 <> t7]
                    ]
        it "can consume scrolling backward by 1 year" $ do
            scroll maxKey takeBefore byYear 1 (fromList ts)
                `shouldBe` results
                    [ [t4 <> t5 <> t6 <> t7]
                    , [t0 <> t1 <> t2 <> t3]
                    ]
