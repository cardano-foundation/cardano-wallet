{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , checkCoverage
    , cover
    , property
    , withMaxSuccess
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Data.ByteString as BS
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Counting how many blocks each pool produced" $ do
        describe "combineMetrics" $ do
            it "pools with no entry for productions are included" $
                property $ \stakeDistr -> do
                    combineMetrics stakeDistr Map.empty
                    `shouldBe`
                    (Right $ Map.map (, Quantity 0) stakeDistr)

            it "it fails if a block-producer is not in the stake distr" $ do
                checkCoverage
                . property
                . withMaxSuccess 1000
                $ \stakeDistr poolProd ->
                    let
                        aPoolWithoutStakeProduced =
                            not . Map.null $ Map.difference poolProd stakeDistr
                    in cover 20 aPoolWithoutStakeProduced
                        "A pool without stake produced" $
                       cover 1 (not aPoolWithoutStakeProduced)
                        "Successfully combined the maps" $
                        case combineMetrics stakeDistr poolProd of
                            Left _ ->
                                aPoolWithoutStakeProduced === True
                            Right x ->
                                Map.map fst x === stakeDistr

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
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = Hash $ BS.pack $ replicate 32 0

instance Arbitrary Block where
     arbitrary = genericArbitrary
     shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word)

instance Arbitrary (Quantity "lovelace" Word64) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary (Quantity "block" Natural) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = PoolId $ BS.pack $ replicate 32 0
