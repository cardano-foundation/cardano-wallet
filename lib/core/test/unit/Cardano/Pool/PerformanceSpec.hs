{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.PerformanceSpec
    ( spec
    ) where

import Prelude

import Cardano.Pool.Performance
    ( EpochStats (..), apparentPerformance )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..) )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , NonEmptyList (..)
    , Property
    , choose
    , classify
    , counterexample
    , property
    )

spec :: Spec
spec = do
    describe "apparentPerformance" $ do
        it "performances are non negative"
            $ property prop_performancesNonNegative

        it "more blocks produced means better perf"
            $ property
            $ prop_effectOnPerformance [PositiveEffect]
            $ \stat -> let p = poolProduction stat
                in stat { poolProduction = p + 1 }

        it "less blocks produced means lower (or equal) perf"
            $ property
            $ prop_effectOnPerformance [NegativeEffect, NoEffect]
            $ \stat -> let p = poolProduction stat
                in stat { poolProduction = if p >= 1 then p - 1 else p }

        describe "golden test cases" $ do
            performanceGoldens

-- | Performances are always positive numbers
prop_performancesNonNegative
    :: ActiveSlotCoefficient
    -> [EpochStats]
    -> Property
prop_performancesNonNegative (ActiveSlotCoefficient ε) stats =
    property (p >= 0.0)
    & counterexample ("p = " <> show p)
    & classify (p == 0)  ("p == 0")
    & classify (p >= 1)  ("p >= 1")
    & classify (p >= 10) ("p >= 10")
  where
    p = apparentPerformance ε stats

data EffectOnPerformance
    = PositiveEffect
    | NegativeEffect
    | NoEffect
    deriving (Show, Eq)

prop_effectOnPerformance
    :: [EffectOnPerformance]
    -> (EpochStats -> EpochStats)
    -> ActiveSlotCoefficient
    -> NonEmptyList EpochStats
    -> Property
prop_effectOnPerformance
  expectedEffects modifier (ActiveSlotCoefficient ε) (NonEmpty stats) =
    property $ case pAfter `compare` pBefore of
        GT -> PositiveEffect `elem` expectedEffects
        LT -> NegativeEffect `elem` expectedEffects
        EQ -> NoEffect `elem` expectedEffects
    & counterexample ("pBefore = " <> show pBefore)
    & counterexample ("pAfter  = " <> show pAfter)
  where
    pBefore = apparentPerformance ε stats
    pAfter  = apparentPerformance ε (modifier <$> stats)

performanceGoldens :: Spec
performanceGoldens = do
    it "50% stake, ε=1.0, producing 8/8 blocks => p=2.0" $ do
        flip shouldBe 2.0 $ apparentPerformance 1.0
            [ EpochStats
                { poolProduction = 8
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 8
                }
            ]

    it "50% stake, ε=1.0, producing 4/8 blocks => p=1.0" $ do
        flip shouldBe 1.0 $ apparentPerformance 1.0
            [ EpochStats
                { poolProduction = 4
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 8
                }
            ]

    it "50% stake, ε=1.0, producing 2/8 blocks => p=0.5" $ do
        flip shouldBe 0.5 $ apparentPerformance 1.0
            [ EpochStats
                { poolProduction = 2
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 8
                }
            ]

    it "50% stake, ε=1.0, producing 0/8 blocks => p=0.0" $ do
        flip shouldBe 0.0 $ apparentPerformance 1.0
            [ EpochStats
                { poolProduction = 0
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 8
                }
            ]

    it "100% stake, ε=0.1, producing 1/10 blocks => p=1.0" $ do
        flip shouldBe 1.0 $ apparentPerformance 0.1
            [ EpochStats
                { poolProduction = 1
                , poolStake = 100
                , totalStake = 100
                , epochHeight = 10
                }
            ]

    it "50% + 1/8, 50% + 2/4 => p=0.5" $ do
        flip shouldBe 0.5 $ apparentPerformance 1.0
            [ EpochStats
                { poolProduction = 1
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 8
                }
            , EpochStats
                { poolProduction = 2
                , poolStake = 50
                , totalStake = 100
                , epochHeight = 4
                }
            ]

{-------------------------------------------------------------------------------
                                 Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary ActiveSlotCoefficient where
    shrink (ActiveSlotCoefficient ε) =
        ActiveSlotCoefficient <$> shrinkRatio ε
    arbitrary =
        ActiveSlotCoefficient <$> genRatio

instance Arbitrary EpochStats where
    shrink _  = []
    arbitrary = do
        height <- fromIntegral <$> choose @Int (0, 1000)
        production <- fromIntegral <$> choose @Int (0, fromIntegral height)
        total <- fromIntegral <$> choose @Int (1, 1000)
        ratio <- genRatio
        pure EpochStats
            { poolProduction = production
            , poolStake = ceiling (fromIntegral total * ratio)
            , totalStake = total
            , epochHeight = height
            }

genRatio :: Gen Double
genRatio = choose (0.001, 1.0)

shrinkRatio :: Double -> [Double]
shrinkRatio ε = filter (\ε' -> ε' > 0.001 && ε' <= 1.0) (shrink ε)
