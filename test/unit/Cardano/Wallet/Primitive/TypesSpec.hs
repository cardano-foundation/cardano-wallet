{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.PrimitiveSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Dom (..)
    , Hash (..)
    , SlotId (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , balance
    , excluding
    , isSubsetOf
    , isValidCoin
    , isValidSlotId
    , restrictedBy
    , restrictedTo
    , slotDiff
    , slotIncr
    , slotsPerEpoch
    , updatePending
    )
import Data.Set
    ( Set, (\\) )
import Data.Word
    ( Word64 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , choose
    , cover
    , expectFailure
    , oneof
    , property
    , scale
    , vectorOf
    , (===)
    , (==>)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary Coin" $ property isValidCoin
        it "Arbitrary SlotId" $ property isValidSlotId

    describe "Lemma 2.1 - Properties of UTxO operations" $ do
        it "2.1.1) ins⊲ u ⊆ u"
            (checkCoverage prop_2_1_1)
        it "2.1.2) ins⋪ u ⊆ u"
            (checkCoverage prop_2_1_2)
        it "2.1.3) u ⊳ outs ⊆ u"
            (checkCoverage prop_2_1_3)
        it "2.1.4) ins⊲ (u ⋃ v) = (ins⊲ u) ⋃ (ins⊲ v)"
            (checkCoverage prop_2_1_4)
        it "2.1.5) ins⋪ (u ⋃ v) = (ins⋪ u) ⋃ (ins⋪ v)"
            (checkCoverage prop_2_1_5)
        it "2.1.6) (dom u ⋂ ins) ⊲ u = ins⊲ u"
            (checkCoverage prop_2_1_6)
        it "2.1.7) (dom u ⋂ ins) ⋪ u = ins⋪ u"
            (checkCoverage prop_2_1_7)
        it "2.1.8) (dom u ⋃ ins) ⋪ (u ⋃ v) = (ins ⋃ dom u) ⋪ v"
            (checkCoverage prop_2_1_8)
        it "2.1.9) ins⋪ u = (dom u \\ ins)⊲ u"
            (checkCoverage prop_2_1_9)

    describe "Lemma 2.6 - Properties of balance" $ do
        it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v"
            (checkCoverage prop_2_6_1)
        it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)"
            (checkCoverage prop_2_6_2)

    describe "Lemma 3.3 - Updating the pending set" $ do
        it "3.3) updatePending b pending ⊆ pending"
            (checkCoverage prop_3_2)

    describe "Basic slot arithmetic" $ do
        let maxSlot = toEnum maxBound :: SlotId
        it "succ . pred = id"
            (property propNextSlotPrevSlot)
        it "succ always increments the SlotId"
            (property propNextIncrements)
        it ("succ on max slot (" <> show maxSlot <>") throws at runtime")
            (expectFailure prop_succSlotMaxIntFails)
        it "pred decrements the SlotId"
            (property propPrevDecrements)
        it "pred on initial slot throws at runtime"
            (expectFailure prop_predSlot0Fails)
        it "slotDiff results in correct difference"
            (property propAddSlotsDiff)
        it "slotIncr 0 == id"
            (property propAddSlotsId)



{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.1 - Properties of UTxO operations
-------------------------------------------------------------------------------}

prop_2_1_1 :: (Set TxIn, UTxO) -> Property
prop_2_1_1 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `restrictedBy` ins) `isSubsetOf` u

prop_2_1_2 :: (Set TxIn, UTxO) -> Property
prop_2_1_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) `isSubsetOf` u

prop_2_1_3 :: (Set TxOut, UTxO) -> Property
prop_2_1_3 (outs, u) =
    cover 50 cond "u ⋂ outs ≠ ∅" (property prop)
  where
    cond = not $ Set.null $
        Set.fromList (Map.elems (getUTxO u)) `Set.intersection` outs
    prop = (u `restrictedTo` outs) `isSubsetOf` u

prop_2_1_4 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_4 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `restrictedBy` ins)
            ===
        (u `restrictedBy` ins) <> (v `restrictedBy` ins)

prop_2_1_5 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_5 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `excluding` ins)
            ===
        (u `excluding` ins) <> (v `excluding` ins)

prop_2_1_6 :: (Set TxIn, UTxO) -> Property
prop_2_1_6 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `restrictedBy` (dom u `Set.intersection` ins))
            ===
        (u `restrictedBy` ins)

prop_2_1_7 :: (Set TxIn, UTxO) -> Property
prop_2_1_7 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `excluding` (dom u `Set.intersection` ins))
            ===
        (u `excluding` ins)

prop_2_1_8 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_8 (ins, u, v) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        ((u <> v) `excluding` (dom u <> ins))
            ===
        v `excluding` (ins <> dom u)

prop_2_1_9 :: (Set TxIn, UTxO) -> Property
prop_2_1_9 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) === u `restrictedBy` (dom u \\ ins)


{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.6 - Properties of Balance
-------------------------------------------------------------------------------}

prop_2_6_1 :: (UTxO, UTxO) -> Property
prop_2_6_1 (u, v) =
    cover 50 cond "u ≠ ∅ , v ≠ ∅" (property prop)
  where
    -- NOTE:
    -- A precondition (u ⋂ v = ∅ ) is hard to satisfy because our generators
    -- are built in order to not be 'too entropic'. So, we better just create
    -- a v' that has no overlap with u.
    v' = v `excluding` dom u
    cond = not (u `isSubsetOf` mempty || v' `isSubsetOf` mempty)
    prop = balance (u <> v') === balance u + balance v'

prop_2_6_2 :: (Set TxIn, UTxO) -> Property
prop_2_6_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        balance (u `excluding` ins)
            ===
        balance u - balance (u `restrictedBy` ins)


{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 3.3 - Update the Pending Set
-------------------------------------------------------------------------------}

prop_3_2 :: (Block, Set Tx) -> Property
prop_3_2 (b, pending) =
    cover 50 cond0 "pending ≠ ∅ " $
    cover 10 cond1 "transactions b ⋂  pending ≠ ∅ " $
        property prop
  where
    cond0 = not $ Set.null pending
    cond1 = not $ Set.null $ transactions b `Set.intersection` pending
    prop = updatePending b pending `Set.isSubsetOf` pending


{-------------------------------------------------------------------------------
                           Basic Slot Arithmetic
-------------------------------------------------------------------------------}

propNextSlotPrevSlot :: SlotId -> Property
propNextSlotPrevSlot sl = pred (succ sl) === sl

propNextIncrements :: SlotId -> Property
propNextIncrements sl = slotDiff (succ sl) sl === 1

propPrevDecrements :: SlotId -> Property
propPrevDecrements sl =
    sl > SlotId 0 0 ==> slotDiff sl (pred sl) === 1

prop_predSlot0Fails :: Property
prop_predSlot0Fails =
    property $ pred (SlotId 0 0) `seq` ()

prop_succSlotMaxIntFails :: Property
prop_succSlotMaxIntFails =
    property $ succ (toEnum (maxBound :: Int) :: SlotId) `seq` ()

propAddSlotsDiff :: (NumberOfSlots, SlotId) -> Property
propAddSlotsDiff (NumberOfSlots n, sl) =
    slotDiff (slotIncr n sl) sl === fromIntegral n

propAddSlotsId :: SlotId -> Property
propAddSlotsId sl = slotIncr 0 sl === sl


{-------------------------------------------------------------------------------
                            Arbitrary Instances

    Arbitrary instances define here aren't necessarily reflecting on real-life
    scenario, but they help test the property above by constructing data
    structures that don't have much entropy and therefore, allow us to even test
    something when checking for intersections and set restrictions!
-------------------------------------------------------------------------------}

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Hash "TXID01"
        , pure $ Hash "TXID02"
        , pure $ Hash "TXID03"
        ]

-- Same for addresses
instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 3)

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a crazy high indexes

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary Tx where
    shrink (Tx ins outs) =
        (flip Tx outs <$> shrink ins) <> (Tx ins <$> shrink outs)
    arbitrary = do
        ins <- choose (1, 3) >>= flip vectorOf arbitrary
        outs <- choose (1, 3) >>= flip vectorOf arbitrary
        return $ Tx ins outs

instance Arbitrary BlockHeader where
    -- No Shrinking
    arbitrary = BlockHeader
        <$> arbitrary
        <*> oneof
            [ pure $ Hash "BLOCK01"
            , pure $ Hash "BLOCK02"
            , pure $ Hash "BLOCK03"
            ]

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = toEnum <$> choose (0, maxBound `div` 2)

newtype NumberOfSlots = NumberOfSlots Word64 deriving Show

instance Arbitrary NumberOfSlots where
    shrink (NumberOfSlots 0) = []
    shrink (NumberOfSlots n) = NumberOfSlots <$> [0, n `div` 2, n - 1]
    arbitrary = NumberOfSlots . fromIntegral
        <$> choose (0 :: Int, 4 * (fromIntegral slotsPerEpoch))

instance Arbitrary Block where
    shrink (Block h txs) = Block h <$> shrink txs
    arbitrary = do
        txs <- choose (0, 500) >>= flip vectorOf arbitrary
        Block <$> arbitrary <*> pure (Set.fromList txs)
