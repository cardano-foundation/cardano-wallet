module Cardano.Wallet.Primitive.Types.AddressSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( Parity (..)
    , addressParity
    , genAddress
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , checkCoverage
    , cover
    , forAll
    , property
    )

spec :: Spec
spec =
    describe "Cardano.Wallet.Primitive.Types.AddressSpec" $ do

    describe "addressParity" $ do

        it "prop_addressParity_coverage" $
            property prop_addressParity_coverage

-- | Verifies that addresses are generated with both even and odd parity.
--
prop_addressParity_coverage :: Property
prop_addressParity_coverage =
    forAll genAddress $ \addr ->
    checkCoverage $
    cover 40 (addressParity addr == Even)
        "address parity is even" $
    cover 40 (addressParity addr == Odd)
        "address parity is odd" $
    property True
