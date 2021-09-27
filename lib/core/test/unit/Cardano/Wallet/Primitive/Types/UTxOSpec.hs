{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.UTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..), addressParity, coarbitraryAddress )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( coarbitraryTxIn )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), dom, filterByAddress, filterByAddressM, isSubsetOf )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Data.Functor.Identity
    ( runIdentity )
import Data.Generics.Internal.VL.Lens
    ( view )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Property
    , Testable
    , checkCoverage
    , conjoin
    , cover
    , property
    , (===)
    )

import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOSpec" $ do

    parallel $ describe "filtering and partitioning" $ do

        it "prop_filter_disjoint" $
            property prop_filter_disjoint
        it "prop_filter_partition" $
            property prop_filter_partition
        it "prop_filter_toList" $
            property prop_filter_toList
        it "prop_partition_disjoint" $
            property prop_partition_disjoint
        it "prop_partition_mappend" $
            property prop_partition_mappend

    parallel $ describe "filtering by address" $ do

        it "matching everything gives us everything" $
            property prop_filterByAddress_matchAll
        it "matching nothing gives us nothing" $
            property prop_filterByAddress_matchNone
        it "matching some addresses gives us the appropriate subset" $
            property prop_filterByAddress_matchSome
        it "if there are no utxos, the result utxo should be empty" $
            property prop_filterByAddress_empty
        it "filterByAddress/filterByAddressM" $
            property prop_filterByAddress_filterByAddressM
        it "filterByAddress is always subset" $
            property prop_filterByAddress_isSubset

--------------------------------------------------------------------------------
-- Filtering and partitioning
--------------------------------------------------------------------------------

prop_filter_disjoint :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_disjoint f u =
    checkCoverage_filter_partition f u $
    UTxO.filter f u `UTxO.disjoint` UTxO.filter (not . f) u === True

prop_filter_partition :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_partition f u =
    checkCoverage_filter_partition f u $
    (UTxO.filter f u, UTxO.filter (not . f) u) === UTxO.partition f u

prop_filter_toList :: (TxIn -> Bool) -> UTxO -> Property
prop_filter_toList f u =
    checkCoverage_filter_partition f u $
    UTxO.toList (UTxO.filter f u) === L.filter (f . fst) (UTxO.toList u)

prop_partition_disjoint :: (TxIn -> Bool) -> UTxO -> Property
prop_partition_disjoint f u =
    checkCoverage_filter_partition f u $
    uncurry UTxO.disjoint (UTxO.partition f u) === True

prop_partition_mappend :: (TxIn -> Bool) -> UTxO -> Property
prop_partition_mappend f u =
    checkCoverage_filter_partition f u $
    uncurry (<>) (UTxO.partition f u) === u

checkCoverage_filter_partition
    :: Testable prop => (TxIn -> Bool) -> UTxO -> (prop -> Property)
checkCoverage_filter_partition f u
    = checkCoverage
    . cover 10
        (UTxO.filter f u `isNonEmptyProperSubsetOf` u)
        "UTxO.filter f u `isNonEmptyProperSubsetOf` u"
    . cover 10
        (UTxO.filter (not . f) u `isNonEmptyProperSubsetOf` u)
        "UTxO.filter (not . f) u `isNonEmptyProperSubsetOf` u"
    . cover 10
        (UTxO.size (UTxO.filter f u) > UTxO.size (UTxO.filter (not . f) u))
        "UTxO.size (UTxO.filter f u) > UTxO.size (UTxO.filter (not . f) u)"
    . cover 10
        (UTxO.size (UTxO.filter f u) < UTxO.size (UTxO.filter (not . f) u))
        "UTxO.size (UTxO.filter f u) < UTxO.size (UTxO.filter (not . f) u)"
  where
    u1 `isNonEmptyProperSubsetOf` u2 =
        not (UTxO.null u1)
        && u1 `UTxO.isSubsetOf` u2
        && u1 /= u2

--------------------------------------------------------------------------------
-- Filtering by address
--------------------------------------------------------------------------------

prop_filterByAddress_matchAll :: UTxO -> Property
prop_filterByAddress_matchAll u =
    checkCoverage $
    cover 2 (u == mempty) "empty" $
    cover 8 (u /= mempty) "non-empty" $
    filterByAddress (const True) u === u

prop_filterByAddress_matchNone :: UTxO -> Property
prop_filterByAddress_matchNone u =
    checkCoverage $
    cover 2 (u == mempty) "empty" $
    cover 8 (u /= mempty) "non-empty" $
    filterByAddress (const False) u === mempty

prop_filterByAddress_matchSome :: UTxO -> Property
prop_filterByAddress_matchSome utxo =
    checkCoverage $
    cover 10
        (domEven /= mempty && domEven `Set.isProperSubsetOf` dom utxo)
        "domEven /= mempty && domEven `Set.isProperSubsetOf` dom utxo" $
    cover 10
        (domOdd /= mempty && domOdd `Set.isProperSubsetOf` dom utxo)
        "domOdd /= mempty && domOdd `Set.isProperSubsetOf` dom utxo" $
    conjoin
        [ utxoEven <> utxoOdd == utxo
        , unUTxO utxoEven `Map.isSubmapOf` unUTxO utxo
        , unUTxO utxoOdd  `Map.isSubmapOf` unUTxO utxo
        , all ((== Even) . addressParity . view #address) (unUTxO utxoEven)
        , all ((==  Odd) . addressParity . view #address) (unUTxO utxoOdd)
        ]
  where
    domEven = dom utxoEven
    domOdd  = dom utxoOdd

    utxoEven = filterByAddress ((== Even) . addressParity) utxo
    utxoOdd  = filterByAddress ((==  Odd) . addressParity) utxo

prop_filterByAddress_empty :: (Address -> Bool) -> Property
prop_filterByAddress_empty f =
    filterByAddress f mempty === mempty

prop_filterByAddress_filterByAddressM :: UTxO -> (Address -> Bool) -> Property
prop_filterByAddress_filterByAddressM u f =
    checkCoverage $
    cover 10 isNonEmptyProperSubset "is non-empty proper subset" $
    filterByAddress f u === runIdentity (filterByAddressM (pure . f) u)
  where
    isNonEmptyProperSubset = (&&)
        (filterByAddress f u /= mempty)
        (dom (filterByAddress f u) `Set.isProperSubsetOf` dom u)

prop_filterByAddress_isSubset :: UTxO -> (Address -> Bool) -> Property
prop_filterByAddress_isSubset u f =
    checkCoverage $
    cover 10 isNonEmptyProperSubset "is non-empty proper subset" $
    property $ filterByAddress f u `isSubsetOf` u
  where
    isNonEmptyProperSubset = (&&)
        (filterByAddress f u /= mempty)
        (dom (filterByAddress f u) `Set.isProperSubsetOf` dom u)

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance CoArbitrary Address where
    coarbitrary = coarbitraryAddress

instance CoArbitrary TxIn where
    coarbitrary = coarbitraryTxIn

instance Arbitrary UTxO where
    arbitrary = genUTxO
    shrink = shrinkUTxO

--------------------------------------------------------------------------------
-- Show instances
--------------------------------------------------------------------------------

instance Show (Address -> Bool) where
    show = const "(Address -> Bool)"

instance Show (TxIn -> Bool) where
    show = const "(TxIn -> Bool)"
