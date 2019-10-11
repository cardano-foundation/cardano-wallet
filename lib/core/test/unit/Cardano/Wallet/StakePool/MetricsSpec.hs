{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.StakePool.MetricsSpec (spec) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , flatSlot
    , fromFlatSlot
    )
import Cardano.Wallet.StakePool.Metrics
    ( State (..), applyBlock )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32 )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), InfiniteList (..), property, (===) )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )

import qualified Data.ByteString as BS
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Counting how many blocks each pool produced" $
        describe "State" $ do
            it "stores the last applied blockHeader"
                $ property $ \s b@(header,_) -> do
                tip (applyBlock b s) `shouldBe` header

            it "counts every block it applies (total activity increases by 1\
               \when a block is applied"
                $ property $ \s block -> do
                let s' = applyBlock block s
                let count = Map.foldl (\r l -> r + (length l)) 0 . activity
                count s' === (count s + 1)


instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary State where
    arbitrary = genericArbitrary
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

instance Arbitrary (Quantity "block" Word32) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word)

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = PoolId $ BS.pack $ replicate 32 0
