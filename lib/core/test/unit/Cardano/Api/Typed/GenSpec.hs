module Cardano.Api.Typed.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..), CardanoEra (..) )
import Cardano.Api.Typed.Gen
    ( genTx )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldSatisfy, xit )
import Test.Hspec
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Blind (..)
    , Gen
    , InfiniteList (..)
    , NonEmptyList (..)
    , Property
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , arbitrarySizedFractional
    , checkCoverage
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , forAll
    , forAllBlind
    , label
    , liftArbitrary
    , liftShrink
    , liftShrink2
    , listOf1
    , oneof
    , property
    , scale
    , shrinkIntegral
    , shrinkList
    , sized
    , suchThat
    , vector
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( report )
import Test.QuickCheck.Hedgehog
    ( hedgehog )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Cardano.Api as Cardano
import qualified Hedgehog as H
import qualified Test.Hspec.Hedgehog as Hspec

spec :: Spec
spec =
    describe "Cardano.Api.Typed.Gen" $
        describe "Generator coverage" $
            -- it "genTx" (property genTxCoverage)
            it "genTx" (Hspec.hedgehog hedgehogCoverageTests)

hedgehogCoverageTests = do
    tx <- Hspec.forAll (genTx Cardano.ShelleyEra)
    let
        numWitnesses = length $ Cardano.getTxWitnesses tx
    Hspec.cover 2 "no witnesses" (numWitnesses == 0)
    Hspec.cover 10 "some witnesses" (numWitnesses > 0)

-- genTxCoverage :: Property
-- genTxCoverage =
--     forAll (genTx Cardano.ShelleyEra) $ \tx ->
--       let
--           numWitnesses = length $ Cardano.getTxWitnesses tx
--       in
--           checkCoverage
--           $ cover 2 (numWitnesses == 0) "no witnesses"
--           $ cover 1 (numWitnesses > 0) "some witnesses"
--           $ True
