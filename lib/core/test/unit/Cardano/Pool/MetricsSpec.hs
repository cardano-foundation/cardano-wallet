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
    ( Block (..), combineMetrics )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , flatSlot
    , fromFlatSlot
    )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , cover
    , elements
    , property
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "combineMetrics" $ do
        it "pools with no entry for productions are included" $
            property $ \stakeDistr -> do
                combineMetrics stakeDistr Map.empty Map.empty
                `shouldBe`
                (Right $ Map.map (, Quantity 0, 0) stakeDistr)

        it "it fails if a block-producer is not in the stake distr"
            $ checkCoverage
            $ property prop_combineIsLeftBiased

{-------------------------------------------------------------------------------
                                Properties
-------------------------------------------------------------------------------}

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
    cover 20 shouldLeft "A pool without stake produced"
    $ cover 1 (not shouldLeft) "Successfully combined the maps"
    $ case combineMetrics mStake mProd mPerf of
        Left _ ->
            shouldLeft === True
        Right x ->
            Map.map (\(a,_,_) -> a) x === mStake
{-# HLINT ignore prop_combineIsLeftBiased "Use ||" #-}


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
    arbitrary = Hash . getPoolId <$> arbitrary

instance Arbitrary Block where
     arbitrary = genericArbitrary
     shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)

instance Arbitrary (Quantity any Word64) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary PoolId where
    shrink _  = []
    arbitrary =
        PoolId . B8.pack <$> vectorOf 8 (elements (['a'..'f'] ++ ['0'..'9']))
