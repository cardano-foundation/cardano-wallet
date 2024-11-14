{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.UI.Lib.DiscretizationSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.UI.Lib.Discretization
    ( Window (..)
    , discretizeTime
    , nextDiscretizedTime
    )
import Control.Monad.Cont
    ( cont
    , evalCont
    )
import Data.Time
    ( DayOfWeek (..)
    , UTCTime
    , addUTCTime
    , defaultTimeLocale
    , parseTimeOrError
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.Hspec.QuickCheck
    ( modifyMaxSuccess
    )
import Test.QuickCheck
    ( Gen
    , NonNegative (NonNegative)
    , Property
    , Testable
    , arbitrary
    , counterexample
    , elements
    , forAll
    , (===)
    )

readTime :: String -> POSIXTime
readTime =
    utcTimeToPOSIXSeconds
        . parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | We test after 2016-01-01 00:00:00 as mainnet was started in 2017.
-- In particular we include 2016 as it has a leap second which could create issues
-- in the commutative test below
timeG :: Gen UTCTime
timeG = do
    NonNegative diff <- arbitrary
    let posix = readTime "2016-01-01 00:00:00" + fromIntegral @Int diff
    return $ posixSecondsToUTCTime posix

dayOfWeekG :: Gen DayOfWeek
dayOfWeekG = elements [Monday, Sunday]

windowG :: Gen Window
windowG =
    elements
        [ Minute5
        , Minute10
        , Minute30
        , Hour1
        , Hour2
        , Hour4
        , Hour12
        , Day
        , Week
        , Year
        , Month
        ]

showCounterExample
    :: Testable property
    => UTCTime
    -> DayOfWeek
    -> Window
    -> property
    -> Property
showCounterExample t d w =
    counterexample
        ( "t: "
            <> show t
            <> "\n d: "
            <> show d
            <> "\n w: "
            <> show w
            <> "\n discretizeTime: "
            <> show (discretizeTime d w t)
            <> "\n nextDiscretizedTime: "
            <> show (nextDiscretizedTime d w t)
            <> "\n discretizeTime (nextDiscretizedTime): "
            <> show (discretizeTime d w (nextDiscretizedTime d w t))
            <> "\n nextDiscretizedTime (discretizeTime): "
            <> show (nextDiscretizedTime d w (discretizeTime d w t))
        )

spec :: Spec
spec = do
    describe
        "discretize"
        $ modifyMaxSuccess (const 100000)
        $ do
            it "after next is equivalent to next after discretize"
                $ evalCont
                $ do
                    t <- cont $ forAll timeG
                    d <- cont $ forAll dayOfWeekG
                    w <- cont $ forAll windowG
                    pure
                        $ showCounterExample t d w
                        $ nextDiscretizedTime
                            d
                            w
                            (discretizeTime d w t)
                            === discretizeTime
                                d
                                w
                                (nextDiscretizedTime d w t)
    describe "next"
        $ do
            it "is in the future" $ evalCont $ do
                t <- cont $ forAll timeG
                d <- cont $ forAll dayOfWeekG
                w <- cont $ forAll windowG
                pure
                    $ showCounterExample t d w
                    $ nextDiscretizedTime d w t > t
            it "can be reverted by discretizing from one seconds before"
                $ evalCont
                $ do
                    t <- cont $ forAll timeG
                    d <- cont $ forAll dayOfWeekG
                    w <- cont $ forAll windowG
                    pure
                        $ showCounterExample t d w
                        $ discretizeTime
                            d
                            w
                            ( addUTCTime (-1)
                                $ nextDiscretizedTime d w t
                            )
                            === discretizeTime d w t
