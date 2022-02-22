{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- HLINT ignore "Use camelCase" -}
module Cardano.Wallet.Primitive.BlockSummarySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Gen
    ( genBlockHeader, genSlot, genSlotNo )
import Cardano.Wallet.Primitive.BlockSummary
    ( BlockEvents (BlockEvents, slot)
    , ChainEvents
    , fromBlockEvents
    , mkChainEvents
    , toAscBlockEvents
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Slot, WithOrigin (..) )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , listOf1
    , property
    , resize
    , shuffle
    , (===)
    )

import Data.Map.Strict as Map

spec :: Spec
spec = do
    parallel $ describe "ChainEvents" $ do
        it "conversion to and from [BlockEvents]" $
            property prop_toFromBlocks

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
prop_toFromBlocks :: ChainEvents -> Gen Property
prop_toFromBlocks cs1 = do
    cs2 <- fromBlockEvents <$> shuffle (toAscBlockEvents cs1)
    pure $ cs1 === cs2

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
instance Arbitrary Slot where
    arbitrary = genSlot

instance Arbitrary ChainEvents where
    arbitrary = do
        bs <- listOf1 arbitrary
        pure . mkChainEvents $ Map.fromList [ (slot b, b) | b <- bs ]

instance Arbitrary BlockEvents where
    arbitrary = do
        sl <- genSlotNo
        ht <- blockHeight <$> genBlockHeader sl
        BlockEvents
            (At sl)
            ht
            <$> (zip [0..] <$> resize 2 (listOf1 genTx))
            <*> pure []
