module Cardano.Wallet.Primitive.Types.UTxOSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxO
    ( filterByAddress, filterByAddressM )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, shrinkUTxO )
import Data.Functor.Identity
    ( runIdentity )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Property, forAllShrink, property, (===) )

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.UTxOSpec" $ do

    describe "filterByAddress" $ do
        it "matching everything gives us everything" $
            property prop_filterByAddress_matchAll
        it "matching nothing gives us nothing" $
            property prop_filterByAddress_matchNone
        it "if there are no utxos, the result utxo should be empty" $
            property prop_filterByAddress_empty
        it "filterByAddress/filterByAddressM" $
            property prop_filterByAddress_filterByAddressM

prop_filterByAddress_matchAll :: Property
prop_filterByAddress_matchAll =
    forAllShrink genUTxO shrinkUTxO $ \u ->
        filterByAddress (const True) u === u

prop_filterByAddress_matchNone :: Property
prop_filterByAddress_matchNone =
    forAllShrink genUTxO shrinkUTxO $ \u ->
        filterByAddress (const False) u === mempty

prop_filterByAddress_empty :: Bool -> Property
prop_filterByAddress_empty b =
    let
        f = const b
    in
        filterByAddress f mempty === mempty

prop_filterByAddress_filterByAddressM :: Bool -> Property
prop_filterByAddress_filterByAddressM b =
    let
        f = const b
    in
        forAllShrink genUTxO shrinkUTxO $ \u ->
            filterByAddress f u === runIdentity (filterByAddressM (pure . f) u)
