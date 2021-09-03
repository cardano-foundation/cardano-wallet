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
import Test.QuickCheck
    ( Arbitrary (..)
    , CoArbitrary (..)
    , Property
    , checkCoverage
    , conjoin
    , cover
    , property
    , (===)
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOSpec" $ do

    describe "filterByAddress" $ do
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

instance CoArbitrary Address where
    coarbitrary = coarbitraryAddress

instance Show (Address -> Bool) where
    show = const "(Address -> Bool)"

instance Arbitrary UTxO where
    arbitrary = genUTxO
    shrink = shrinkUTxO
