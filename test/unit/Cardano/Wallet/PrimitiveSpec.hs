{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.PrimitiveSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Address (..)
    , Coin (..)
    , Dom (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    , excluding
    , isSubsetOf
    , restrictedBy
    , restrictedTo
    )
import Data.Set
    ( Set, (\\) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , checkCoverage
    , choose
    , cover
    , property
    , scale
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
    describe "Lemma 2.1 - Properties of UTxO operations" $ do
        it "2.1.1) ins⊲ u ⊆ u" (checkCoverage prop_2_1_1)
        it "2.1.2) ins⋪ u ⊆ u" (checkCoverage prop_2_1_2)
        it "2.1.3) u ⊳ outs ⊆ u" (checkCoverage prop_2_1_3)
        it "2.1.4) ins⊲ (u ⋃ v) = (ins⊲ u) ⋃ (ins⊲ v)" (checkCoverage prop_2_1_4)
        it "2.1.5) ins⋪ (u ⋃ v) = (ins⋪ u) ⋃ (ins⋪ v)" (checkCoverage prop_2_1_5)
        it "2.1.6) (dom u ⋂ ins) ⊲ u = ins⊲ u" (checkCoverage prop_2_1_6)
        it "2.1.7) (dom u ⋂ ins) ⋪ u = ins⋪ u" (checkCoverage prop_2_1_7)
        it "2.1.8) (dom u ⋃ ins) ⋪ (u ⋃ v) = (ins ⋃ dom u) ⋪ v" $ checkCoverage prop_2_1_8
        it "2.1.9) ins⋪ u = (dom u \\ ins)⊲ u" (checkCoverage prop_2_1_9)

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
    cond = not $ Set.null $ Set.fromList (Map.elems (getUTxO u)) `Set.intersection` outs
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
    prop = (u `restrictedBy` (dom u `Set.intersection` ins)) === (u `restrictedBy` ins)

prop_2_1_7 :: (Set TxIn, UTxO) -> Property
prop_2_1_7 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` (dom u `Set.intersection` ins)) === (u `excluding` ins)

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
    prop =
        (u `excluding` ins) === u `restrictedBy` (dom u \\ ins)


{-------------------------------------------------------------------------------
                            Arbitrary Instances

    Arbitrary instances define here aren't necessarily reflecting on real-life
    scenario, but they help test the property above by constructing data
    structures that don't have much entropy and therefore, allow us to even test
    something when checking for intersections and set restrictions!
-------------------------------------------------------------------------------}

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary =
        (Hash . ("txid-" <>) . BS.singleton) <$> choose (0, 3)

-- Same for addresses
instance Arbitrary Address where
    -- No Shrinking
    arbitrary =
        (Address . ("addr-" <>) . BS.singleton) <$> choose (0, 3)

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
