{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.Types.UTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..), addressParity )
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
    filterByAddress (const True) u === u

prop_filterByAddress_matchNone :: UTxO -> Property
prop_filterByAddress_matchNone u =
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

prop_filterByAddress_empty :: Bool -> Property
prop_filterByAddress_empty b =
    let
        f = const b
    in
        filterByAddress f mempty === mempty

prop_filterByAddress_filterByAddressM :: UTxO -> Bool -> Property
prop_filterByAddress_filterByAddressM u b =
    let
        f = const b
    in
        filterByAddress f u === runIdentity (filterByAddressM (pure . f) u)

prop_filterByAddress_isSubset :: UTxO -> Bool -> Property
prop_filterByAddress_isSubset u b =
    let
        f = const b
    in
        property $ filterByAddress f u `isSubsetOf` u

instance Arbitrary UTxO where
    arbitrary = genUTxO
    shrink = shrinkUTxO
