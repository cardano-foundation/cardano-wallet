{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.Primitive.Types.BlockSummarySpec
    ( spec
    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..)
    , WithOrigin (..)
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader (..)
    , Slot
    )
import Cardano.Wallet.Primitive.Types.Block.Gen
    ( genBlockHeader
    )
import Cardano.Wallet.Primitive.Types.BlockSummary
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
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx
    )
import Data.Foldable
    ( toList
    )
import Data.Word
    ( Word32
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , forAll
    , frequency
    , listOf1
    , property
    , resize
    , shuffle
    , sublistOf
    , (===)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Sublist" $ do
        it "merging denotes union on sets"
            $ property prop_merge_denotation
        it "merging is idempotent"
            $ property prop_merge_idempotent
        it "merging has whole list as absorbing element"
            $ property prop_merge_absorbing_element
        it "merging is commutative"
            $ property prop_merge_commutative

    describe "ChainEvents" $ do
        it "conversion to and from [BlockEvents]"
            $ property prop_toFromBlocks

        it "monoid is idemptotent"
            $ property prop_idempotentChainEvents

{-------------------------------------------------------------------------------
    Properties
-------------------------------------------------------------------------------}
prop_merge_denotation :: [Int] -> Property
prop_merge_denotation xs =
    forAll (genSublist xs) $ \a ->
        forAll (genSublist xs) $ \b ->
            toSet (a `mergeSublist` b) === toSet a `Set.union` toSet b
  where
    toSet = Set.fromList . toList

prop_merge_idempotent :: [Int] -> Property
prop_merge_idempotent xs = forAll (genSublist xs) $ \s ->
    s `mergeSublist` s === s

prop_merge_absorbing_element :: [Int] -> Property
prop_merge_absorbing_element xs = forAll (genSublist xs) $ \s ->
    let z = wholeList xs
    in  z `mergeSublist` s === z

prop_merge_commutative :: [Int] -> Property
prop_merge_commutative xs =
    forAll (genSublist xs) $ \a ->
        forAll (genSublist xs) $ \b ->
            a `mergeSublist` b === b `mergeSublist` a

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
genSublist xs = unsafeMkSublist <$> sublistOf (zip (map (,0) [0 ..]) xs)

instance Arbitrary Slot where
    arbitrary = genSlot

instance Arbitrary ChainEvents where
    arbitrary = do
        bs <- listOf1 arbitrary
        pure . mkChainEvents $ Map.fromList [(slot b, b) | b <- bs]

instance Arbitrary BlockEvents where
    arbitrary = do
        sl <- genSlotNo
        ht <- blockHeight <$> genBlockHeader sl
        BlockEvents
            (At sl)
            ht
            <$> (wholeList <$> resize 2 (listOf1 genTx))
            <*> pure (wholeList [])

genSlot :: Gen Slot
genSlot = frequency
    [ ( 1, pure Origin)
    , (40, At <$> genSlotNo)
    ]

-- | Don't generate /too/ large slots
genSlotNo :: Gen SlotNo
genSlotNo = SlotNo . fromIntegral <$> arbitrary @Word32
