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
    , Sublist
    , fromBlockEvents
    , mergeSublist
    , mkChainEvents
    , toAscBlockEvents
    , unsafeMkSublist
    , wholeList
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
    , forAll
    , listOf1
    , property
    , resize
    , shuffle
    , sublistOf
    , (===)
    )

import Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Sublist" $ do
        it "merging is idempotent" $
            property prop_idempotent
        it "merging has whole list as neutral element" $
            property prop_neutral_element

    parallel $ describe "ChainEvents" $ do
        it "conversion to and from [BlockEvents]" $
            property prop_toFromBlocks

        it "monoid is idemptotent" $
            property prop_idempotentChainEvents

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
prop_idempotent :: [Int] -> Property
prop_idempotent xs = forAll (genSublist xs) $ \s ->
    s `mergeSublist` s === s

prop_neutral_element :: [Int] -> Property
prop_neutral_element xs = forAll (genSublist xs) $ \s ->
    let whole = wholeList xs
    in  whole `mergeSublist` s === whole

prop_toFromBlocks :: ChainEvents -> Gen Property
prop_toFromBlocks cs1 = do
    cs2 <- fromBlockEvents <$> shuffle (toAscBlockEvents cs1)
    pure $ cs1 === cs2

prop_idempotentChainEvents :: ChainEvents -> Property
prop_idempotentChainEvents cs = cs <> cs === cs

{-------------------------------------------------------------------------------
    Generators
-------------------------------------------------------------------------------}
genSublist :: [a] -> Gen (Sublist a)
genSublist xs = unsafeMkSublist <$> sublistOf (zip [0..] xs)

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
            <$> (wholeList <$> resize 2 (listOf1 genTx))
            <*> pure (wholeList [])
