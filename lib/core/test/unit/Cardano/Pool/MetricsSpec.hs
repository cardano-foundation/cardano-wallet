{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Cardano.Pool.MetricsSpec (spec) where

import Prelude

import Cardano.Pool.Metrics
    ( Block (..), calculatePerformance, combineMetrics )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , Coin (..)
    , EpochLength (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , flatSlot
    , fromFlatSlot
    )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , checkCoverage
    , choose
    , classify
    , counterexample
    , cover
    , elements
    , frequency
    , property
    , vectorOf
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "combineMetrics" $ do
        it "pools with no entry for productions are included"
            $ property prop_combineDefaults

        it "it fails if a block-producer is not in the stake distr"
            $ checkCoverage
            $ property prop_combineIsLeftBiased

    describe "calculatePerformances" $ do
        it "performances are always between 0 and 1"
            $ property prop_performancesBounded01
        it "performance a single pool with 50% stake is as expected"
            $ property prop_performanceOfALonelyPool

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}

prop_combineDefaults
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Property
prop_combineDefaults mStake = do
    combineMetrics mStake Map.empty Map.empty
    ===
    (Right $ Map.map (, Quantity 0, 0) mStake)

-- | it fails if a block-producer or performance is not in the stake distr
prop_combineIsLeftBiased
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> Map PoolId Double
    -> Property
prop_combineIsLeftBiased mStake mProd mPerf =
    let
        shouldLeft = or
            [ not . Map.null $ Map.difference mProd mStake
            , not . Map.null $ Map.difference mPerf mStake
            ]
    in
    cover 10 shouldLeft "A pool without stake produced"
    $ cover 50 (not shouldLeft) "Successfully combined the maps"
    $ case combineMetrics mStake mProd mPerf of
        Left _ ->
            shouldLeft === True
        Right x ->
            Map.map (\(a,_,_) -> a) x === mStake
{-# HLINT ignore prop_combineIsLeftBiased "Use ||" #-}

-- | Performances are always positive numbers
prop_performancesBounded01
    :: Map PoolId (Quantity "lovelace" Word64)
    -> Map PoolId (Quantity "block" Word64)
    -> (NonNegative Int)
    -> Property
prop_performancesBounded01 mStake mProd (NonNegative emptySlots) =
    all (between 0 1) performances
    & counterexample (show performances)
    & classify (all (== 0) performances) "all null"
  where
    performances :: [Double]
    performances = Map.elems $ calculatePerformance slots mStake mProd

    slots :: Int
    slots = emptySlots +
        fromIntegral (Map.foldl (\y (Quantity x) -> (y + x)) 0 mProd)

    between :: Ord a => a -> a -> a -> Bool
    between inf sup x = x >= inf && x <= sup

prop_performanceOfALonelyPool
    :: NonNegative Int
    -> NonNegative Int
    -> Property
prop_performanceOfALonelyPool (NonNegative prod) (NonNegative totalSlots) =
    prod < totalSlots ==>
        let
            stake = mkStake
                [ (poolA, 1)
                , (poolB, 1)
                ]
            production = mkProduction
                [ (poolA, prod)
                , (poolB, 0)
                ]
            performances = calculatePerformance totalSlots stake production

            expected = Just $ min 1 $ (fromIntegral prod * 2) / (fromIntegral totalSlots)
            actual = (Map.lookup poolA performances)
        in
            counterexample debugMsg $
            counterexample "poolA has 50% stake" $
            --counterexample ("should have performance="++show ) $
            actual === expected

  where
    poolA = PoolId "athena"
    poolB = PoolId "nemesis"
    mkStake = Map.map Quantity . Map.fromList
    mkProduction = Map.map (Quantity . fromIntegral) . Map.fromList
    debugMsg =
        "poolA produced " ++ show prod ++ "/" ++ show totalSlots ++ " blocks"

{-------------------------------------------------------------------------------
                                 Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> arbitrary
    shrink sl = fromFlatSlot epochLength <$> shrink (flatSlot epochLength sl)

-- | Epoch length used to generate arbitrary @SlotId@
epochLength :: EpochLength
epochLength = EpochLength 50

instance Arbitrary (Hash tag) where
    shrink _  = []
    arbitrary = Hash . B8.pack
        <$> vectorOf 8 (elements (['a'..'f'] ++ ['0'..'9']))

instance Arbitrary Block where
   arbitrary = genericArbitrary
   shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)
    shrink (Quantity x) = map Quantity $ shrink x

instance Arbitrary (Quantity "block" Word64) where
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)
    shrink (Quantity x) = map Quantity $ shrink x

instance Arbitrary (Quantity "lovelace" Word64) where
    arbitrary = Quantity . fromIntegral . unLovelace <$> (arbitrary @Lovelace)
    shrink (Quantity x) = map Quantity $ shrink x

-- TODO: Move to a shared location for Arbitrary newtypes
newtype Lovelace = Lovelace { unLovelace :: Word64 }
instance Arbitrary Lovelace where
    shrink (Lovelace x) = map Lovelace $ shrink x
    arbitrary = do
        n <- choose (0, 100)
        Lovelace <$> frequency
            [ (50, return n)
            , (25, return $ minLovelace - n)
            , (25, choose (minLovelace, maxLovelace))
            ]
      where
        minLovelace = fromIntegral . getCoin $ minBound @Coin
        maxLovelace = fromIntegral . getCoin $ maxBound @Coin

instance Arbitrary PoolId where
    shrink _  = []
    arbitrary = PoolId . B8.pack
        <$> elements [ "ares", "athena", "hades", "hestia", "nemesis" ]
