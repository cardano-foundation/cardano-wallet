{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( AddressInEra (..)
    , AddressTypeInEra (..)
    , AnyCardanoEra (..)
    , AssetId (..)
    , AssetName (..)
    , BuildTx (..)
    , CardanoEra (..)
    , EpochNo (..)
    , ExecutionUnits (..)
    , KeyWitnessInCtx (..)
    , Lovelace
    , NetworkId (..)
    , NetworkMagic (..)
    , PaymentCredential (..)
    , Quantity (..)
    , ScriptData (..)
    , ScriptLanguageInEra (..)
    , ScriptValidity (..)
    , ScriptWitnessInCtx (..)
    , ShelleyWitnessSigningKey (..)
    , SimpleScript (..)
    , SimpleScriptVersion (..)
    , SlotNo (..)
    , StakeAddressPointer (..)
    , StakeAddressReference (..)
    , StakeCredential (..)
    , TxExtraKeyWitnesses (..)
    , TxFee (..)
    , TxFeesExplicitInEra (..)
    , TxFeesImplicitInEra (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxMintValue (..)
    , TxOut (..)
    , TxOutDatumHash (..)
    , TxOutValue (..)
    , TxScriptValidity (..)
    , TxValidityLowerBound (..)
    , TxValidityUpperBound (..)
    , Value (..)
    , WitCtxStake (..)
    , Witness (..)
    , extraKeyWitnessesSupportedInEra
    , multiAssetSupportedInEra
    , quantityToLovelace
    , txScriptValiditySupportedInCardanoEra
    , valueToList
    )
import Cardano.Api.Byron
    ( WitnessNetworkIdOrByronAddress (..) )
import Cardano.Api.Gen
import Cardano.Api.Shelley
    ( StakeCredential (..) )
import Cardano.Ledger.Credential
    ( Ix (..), Ptr (..) )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char
    ( isAlphaNum, isDigit, isLower, isUpper )
import Data.Foldable
    ( for_ )
import Data.Function
    ( (&) )
import Data.Int
    ( Int32, Int64 )
import Data.Traversable
    ( for )
import Data.Word
    ( Word16, Word32, Word64 )
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
    , sample
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
import qualified Data.Map as Map
import qualified Hedgehog as H
import qualified Test.Hspec.Hedgehog as Hspec

spec :: Spec
spec =
    describe "Cardano.Api.Gen" $
        describe "Generator coverage" $ do
            it "genLovelace" $
                property genLovelaceCoverage
            it "genNetworkMagic" $
                property genNetworkMagicCoverage
            it "genNetworkId" $
                property genNetworkIdCoverage
            it "genQuantity" $
                property genQuantityCoverage
            it "genAlphaNum" $
                property genAlphaNumCoverage
            it "genAssetName" $
                property genAssetNameCoverage
            it "genSlotNo" $
                property genSlotNoCoverage
            it "genEpochNo" $
                property genEpochNoCoverage
            it "genPaymentCredential" $
                property genPaymentCredentialCoverage
            it "genSimpleScriptV1" $
                property genSimpleScriptCoverageV1
            it "genSimpleScriptV2" $
                property genSimpleScriptCoverageV2
            it "genAssetId" $
                property genAssetIdCoverage
            it "genTxIx" $
                property genTxIxCoverage
            it "genTtl" $
                property genTtlCoverage
            it "genWitnessNetworkIdOrByronAddress" $
                property genWitnessNetworkIdOrByronAddressCoverage
            describe "genTxFeeCoverage" $ do
                it "genTxFee ByronEra" $
                    property genTxFeeCoverageByron
                it "genTxFee ShelleyEra" $
                    property genTxFeeCoverageShelley
                it "genTxFee AllegraEra" $
                    property genTxFeeCoverageAllegra
                it "genTxFee MaryEra" $
                    property genTxFeeCoverageMary
                it "genTxFee AlonzoEra" $
                    property genTxFeeCoverageAlonzo
            it "genTxIn" $
                property genTxInCoverage
            describe "genTxInsCollateral" $ do
                it "genTxInsCollateral ByronEra" $
                    property genTxInCollateralCoverageByron
                it "genTxInsCollateral ShelleyEra" $
                    property genTxInCollateralCoverageShelley
                it "genTxInsCollateral AllegraEra" $
                    property genTxInCollateralCoverageAllegra
                it "genTxInsCollateral MaryEra" $
                    property genTxInCollateralCoverageMary
                it "genTxInsCollateral AlonzoEra" $
                    property genTxInCollateralCoverageAlonzo
            it "genStakeCredential" $
                property genStakeCredentialCoverage
            it "genIx" $
                property (forAll genIx $ genIxCoverage)
            it "genPtr" $
                property genPtrCoverage
            -- it "genStakeAddressReference" $
            --     property genStakeAddressReferenceCoverage
            describe "genAddressInEra" $ do
                it "genAddressInEra ByronEra" $
                    property $ forAll (genAddressInEra ByronEra) $
                    genAddressInByronEraCoverage
                it "genAddressInEra ShelleyEra" $
                    property $ forAll (genAddressInEra ShelleyEra) $
                    genAddressInShelleyBasedEraCoverage
                it "genAddressInEra AllegraEra" $
                    property $ forAll (genAddressInEra AllegraEra) $
                    genAddressInShelleyBasedEraCoverage
                it "genAddressInEra MaryEra" $
                    property $ forAll (genAddressInEra MaryEra) $
                    genAddressInShelleyBasedEraCoverage
                it "genAddressInEra AlonzoEra" $
                    property $ forAll (genAddressInEra AlonzoEra) $
                    genAddressInShelleyBasedEraCoverage
            it "genUnsignedQuantity" $
                property $
                    forAll genUnsignedQuantity genUnsignedQuantityCoverage
            it "genValueForTxOutCoverage" $
                property $ forAll genValueForTxOut genValueForTxOutCoverage
            describe "genTxOutValue" $ do
                it "genTxOutValue ByronEra" $
                    property $ forAll (genTxOutValue ByronEra)
                        genTxOutValueCoverageAdaOnly
                it "genTxOutValue ShelleyEra" $
                    property $ forAll (genTxOutValue ShelleyEra)
                        genTxOutValueCoverageAdaOnly
                it "genTxOutValue AllegraEra" $
                    property $ forAll (genTxOutValue AllegraEra)
                        genTxOutValueCoverageAdaOnly
                it "genTxOutValue MaryEra" $
                    property $ forAll (genTxOutValue MaryEra)
                        genTxOutValueCoverageMultiAsset
                it "genTxOutValue AlonzoEra" $
                    property $ forAll (genTxOutValue AlonzoEra)
                        genTxOutValueCoverageMultiAsset
            describe "genTxOutDatumHash" $ do
                it "genTxOutDatumHash ByronEra" $
                    property $ forAll (genTxOutDatumHash ByronEra)
                        genTxOutDatumHashNoneCoverage
                it "genTxOutDatumHash ShelleyEra" $
                    property $ forAll (genTxOutDatumHash ShelleyEra)
                        genTxOutDatumHashNoneCoverage
                it "genTxOutDatumHash AllegraEra" $
                    property $ forAll (genTxOutDatumHash AllegraEra)
                        genTxOutDatumHashNoneCoverage
                it "genTxOutDatumHash MaryEra" $
                    property $ forAll (genTxOutDatumHash MaryEra)
                        genTxOutDatumHashNoneCoverage
                it "genTxOutDatumHash Alonzo" $
                    property $ forAll (genTxOutDatumHash AlonzoEra)
                        genTxOutDatumHashCoverage
            describe "genTxOut" $ do
                it "genTxOut ByronEra" $
                    property
                    $ forAll (genTxOut ByronEra) genTxOutCoverageByron
                it "genTxOut ShelleyEra" $
                    property
                    $ forAll (genTxOut ShelleyEra) genTxOutCoverageShelley
                it "genTxOut AllegraEra" $
                    property
                    $ forAll (genTxOut AllegraEra) genTxOutCoverageAllegra
                it "genTxOut MaryEra" $
                    property
                    $ forAll (genTxOut MaryEra) genTxOutCoverageMary
                it "genTxOut AlonzoEra" $
                    property
                    $ forAll (genTxOut AlonzoEra) genTxOutCoverageAlonzo
            describe "genTxValidityLowerBound" $ do
                it "genTxValidityLowerBound ByronEra" $
                    property
                    $ forAll (genTxValidityLowerBound ByronEra)
                    $ genTxValidityLowerBoundNotSupportedCoverage
                it "genTxValidityLowerBound ShelleyEra" $
                    property
                    $ forAll (genTxValidityLowerBound ShelleyEra)
                    $ genTxValidityLowerBoundNotSupportedCoverage
                it "genTxValidityLowerBound AllegraEra" $
                    property
                    $ forAll (genTxValidityLowerBound AllegraEra)
                    $ genTxValidityLowerBoundSupportedCoverage
                it "genTxValidityLowerBound MaryEra" $
                    property
                    $ forAll (genTxValidityLowerBound MaryEra)
                    $ genTxValidityLowerBoundSupportedCoverage
                it "genTxValidityLowerBound AlonzoEra" $
                    property
                    $ forAll (genTxValidityLowerBound AlonzoEra)
                    $ genTxValidityLowerBoundSupportedCoverage
            describe "genTxValidityUpperBound" $ do
                it "genTxValidityUpperBound ByronEra" $
                    property
                    $ forAll (genTxValidityUpperBound ByronEra)
                    $ genTxValidityNoUpperBoundCoverage
                it "genTxValidityUpperBound ShelleyEra" $
                    property
                    $ forAll (genTxValidityUpperBound ShelleyEra)
                    $ genTxValidityUpperBoundCoverage
                it "genTxValidityUpperBound AllegraEra" $
                    property
                    $ forAll (genTxValidityUpperBound AllegraEra)
                    $ genTxValidityUpperBoundCoverage
                it "genTxValidityUpperBound MaryEra" $
                    property
                    $ forAll (genTxValidityUpperBound MaryEra)
                    $ genTxValidityUpperBoundCoverage
                it "genTxValidityUpperBound AlonzoEra" $
                    property
                    $ forAll (genTxValidityUpperBound AlonzoEra)
                    $ genTxValidityUpperBoundCoverage
            describe "genTxValidityRange" $ do
                it "genTxValidityRange ByronEra" $
                    property
                    $ forAll (genTxValidityRange ByronEra)
                    $ genTxValidityRangeByronCoverage
                it "genTxValidityRange ShelleyEra" $
                    property
                    $ forAll (genTxValidityRange ShelleyEra)
                    $ genTxValidityRangeShelleyCoverage
                it "genTxValidityRange AllegraEra" $
                    property
                    $ forAll (genTxValidityRange AllegraEra)
                    $ genTxValidityRangeAllegraCoverage
                it "genTxValidityRange MaryEra" $
                    property
                    $ forAll (genTxValidityRange MaryEra)
                    $ genTxValidityRangeMaryCoverage
                it "genTxValidityRange AlonzoEra" $
                    property
                    $ forAll (genTxValidityRange AlonzoEra)
                    $ genTxValidityRangeAlonzoCoverage
            it "genScriptDataCoverage" $
                property genScriptDataCoverage
            it "genExecutionUnits" $
                property genExecutionUnitsCoverage
            it "genScriptValidity" $
                property genScriptValidityCoverage
            describe "genTxScriptValidity" $ do
                it "genTxScriptValidity ByronEra" $
                    property
                    $ forAll (genTxScriptValidity ByronEra)
                    $ genTxScriptValidityCoverage ByronEra
                it "genTxScriptValidity ShelleyEra" $
                    property
                    $ forAll (genTxScriptValidity ShelleyEra)
                    $ genTxScriptValidityCoverage ShelleyEra
                it "genTxScriptValidity AllegraEra" $
                    property
                    $ forAll (genTxScriptValidity AllegraEra)
                    $ genTxScriptValidityCoverage AllegraEra
                it "genTxScriptValidity MaryEra" $
                    property
                    $ forAll (genTxScriptValidity MaryEra)
                    $ genTxScriptValidityCoverage MaryEra
                it "genTxScriptValidity AlonzoEra" $
                    property
                    $ forAll (genTxScriptValidity AlonzoEra)
                    $ genTxScriptValidityCoverage AlonzoEra
            it "genShelleyWitnessSigningKey" $
                property genShelleyWitnessSigningKeyCoverage
            it "genValueForMinting" $
                property
                $ forAll genValueForMinting genValueForMintingCoverage
            describe "genTxMintValue" $ do
                it "genTxMintValue ByronEra" $
                    property
                    $ forAll (genTxMintValue ByronEra)
                    $ genTxMintValueCoverage ByronEra
                it "genTxMintValue ShelleyEra" $
                    property
                    $ forAll (genTxMintValue ShelleyEra)
                    $ genTxMintValueCoverage ShelleyEra
                it "genTxMintValue AllegraEra" $
                    property
                    $ forAll (genTxMintValue AllegraEra)
                    $ genTxMintValueCoverage AllegraEra
                it "genTxMintValue MaryEra" $
                    property
                    $ forAll (genTxMintValue MaryEra)
                    $ genTxMintValueCoverage MaryEra
                it "genTxMintValue AlonzoEra" $
                    property
                    $ forAll (genTxMintValue AlonzoEra)
                    $ genTxMintValueCoverage AlonzoEra
            describe "genExtraKeyWitnesses" $ do
                it "genExtraKeyWitnesses ByronEra" $
                    property
                    $ forAll (genExtraKeyWitnesses ByronEra)
                    $ genExtraKeyWitnessesCoverage ByronEra
                it "genExtraKeyWitnesses ShelleyEra" $
                    property
                    $ forAll (genExtraKeyWitnesses ShelleyEra)
                    $ genExtraKeyWitnessesCoverage ShelleyEra
                it "genExtraKeyWitnesses AllegraEra" $
                    property
                    $ forAll (genExtraKeyWitnesses AllegraEra)
                    $ genExtraKeyWitnessesCoverage AllegraEra
                it "genExtraKeyWitnesses MaryEra" $
                    property
                    $ forAll (genExtraKeyWitnesses MaryEra)
                    $ genExtraKeyWitnessesCoverage MaryEra
                it "genExtraKeyWitnesses AlonzoEra" $
                    property
                    $ forAll (genExtraKeyWitnesses AlonzoEra)
                    $ genExtraKeyWitnessesCoverage AlonzoEra
            it "genTxMetadataValue" $
                property genTxMetadataValueCoverage
            it "genTxMetadata" $
                property genTxMetadataCoverage

            -- describe "genWitnessStake" $ do
            --     it "genWitnessStake ByronEra" $
            --         property
            --         $ forAll (genWitnessStake ByronEra)
            --                  (genWitnessStakeCoverage ByronEra)
            --     it "genWitnessStake ShelleyEra" $
            --         property
            --         $ forAll (genWitnessStake ShelleyEra)
            --                  (genWitnessStakeCoverage ShelleyEra)

genLovelaceCoverage :: Lovelace -> Property
genLovelaceCoverage l = checkCoverage
    $ cover 1 (l == minLovelace)
        "lovelace is smallest allowable"
    $ cover 2 (l >= veryLargeLovelace)
        "lovelace is very large"
    $ cover 10 (l > minLovelace && l < veryLargeLovelace)
        "lovelace is between smallest and very large"
    $ label "no lovelace is negative" (l >= minLovelace)
      & counterexample "lovelace value was negative"
    where
        minLovelace :: Lovelace
        minLovelace =
            quantityToLovelace 0

        veryLargeLovelace :: Lovelace
        veryLargeLovelace =
            quantityToLovelace $ Quantity $ toInteger (maxBound :: Int)

instance Arbitrary Lovelace where
    arbitrary = genLovelace

genNetworkMagicCoverage :: NetworkMagic -> Property
genNetworkMagicCoverage (NetworkMagic n) = checkCoverage
    $ cover 1 (n == minNetworkMagic)
        "network magic is smallest allowable"
    $ cover 1 (n >= veryLargeNetworkMagic)
        "network magic is large"
    $ cover 10 (n > minNetworkMagic && n < veryLargeNetworkMagic)
        "network magic is between smallest and largest"
    $ label "no network magic is negative" (n >= minNetworkMagic)
      & counterexample "network magic was negative"
    where
        minNetworkMagic :: Word32
        minNetworkMagic = 0

        veryLargeNetworkMagic :: Word32
        veryLargeNetworkMagic = fromIntegral $ toInteger (maxBound :: Int32)

instance Arbitrary NetworkMagic where
    arbitrary = genNetworkMagic

genNetworkIdCoverage :: NetworkId -> Property
genNetworkIdCoverage n = checkCoverage
    $ cover 10 (isMainnet n)
        "network is mainnet"
    $ cover 10 (isTestnet n)
        "network is testnet"
    $ True
    where
        isMainnet = (== Mainnet)

        isTestnet = \case
            Mainnet   -> False
            Testnet _ -> True

instance Arbitrary NetworkId where
    arbitrary = genNetworkId

genQuantityCoverage :: Quantity -> Property
genQuantityCoverage qty = checkCoverage
    $ cover 1 (qty == 0)
        "quantity is zero"
    $ cover 1 (qty >= veryLargeQuantity)
        "quantity is large"
    $ cover 10 (qty > verySmallQuantity && qty < veryLargeQuantity)
        "quantity is between very small and very large"
    $ True

    where
        verySmallQuantity :: Quantity
        verySmallQuantity = fromInteger $ toInteger (minBound :: Int32)

        veryLargeQuantity :: Quantity
        veryLargeQuantity = fromInteger $ toInteger (maxBound :: Int32)

instance Arbitrary Quantity where
    arbitrary = genQuantity

genAlphaNumCoverage :: Property
genAlphaNumCoverage = forAll genAlphaNum $ \c ->
    checkCoverage
    $ cover 10 (isDigit c)
        "character is digit"
    $ cover 10 (isUpper c)
        "character is upper-case alphabetic"
    $ cover 10 (isLower c)
        "character is lower-case alphabetic"
    $ label "character is alphabetic or numeric" (isAlphaNum c)
      & counterexample "character wasn't alphabetic or numeric"

genAssetNameCoverage :: AssetName -> Property
genAssetNameCoverage n = checkCoverage
    $ cover 1 (assetNameLen == 0)
        "asset name is empty"
    $ cover 10 (assetNameLen == shortLength)
        "asset name is short"
    $ cover 5 (assetNameLen == longLength)
        "asset name is long"
    $ cover 5 (assetNameLen > shortLength && assetNameLen < longLength)
        "asset name is between short and long"
    $ label "is alphanumeric" (all isAlphaNum assetNameStr)
      & counterexample "character wasn't alphabetic or numeric"
    where
        assetNameStr = (\(AssetName n') -> B8.unpack n') $ n
        assetNameLen = (\(AssetName n') -> BS.length n') $ n
        shortLength = 1
        longLength = 32

instance Arbitrary AssetName where
    arbitrary = genAssetName

genSlotNoCoverage :: SlotNo -> Property
genSlotNoCoverage slotNo = checkCoverage
    $ cover 0.5 (slotNo == 0)
        "slot number is zero"
    $ cover 10 (slotNo > 0 && slotNo < veryLargeSlotNo)
        "slot number is between zero and very large"
    $ cover 5 (slotNo > veryLargeSlotNo)
        "slot number is greater than very large"
    $ label "slot number is non-negative" (slotNo >= 0)
      & counterexample "slot number was negative"

    where
        veryLargeSlotNo = fromIntegral (maxBound :: Word32)

instance Arbitrary SlotNo where
    arbitrary = genSlotNo

genEpochNoCoverage :: EpochNo -> Property
genEpochNoCoverage epochNo = checkCoverage
    $ cover 1 (epochNo == 0)
        "epoch number is zero"
    $ cover 10 (epochNo > 0 && epochNo < veryLargeEpochNo)
        "epoch number is between zero and very large"
    $ cover 5 (epochNo > veryLargeEpochNo)
        "epoch number is greater than very large"
    $ label "epoch number is non-negative" (epochNo >= 0)
      & counterexample "epoch number was negative"

    where
        veryLargeEpochNo = fromIntegral (maxBound :: Word32)

instance Arbitrary EpochNo where
    arbitrary = genEpochNo

genPaymentCredentialCoverage :: PaymentCredential -> Property
genPaymentCredentialCoverage paymentCred = checkCoverage
    $ cover 20 (isByKey paymentCred)
        "payment credential is provided by key"
    $ cover 20 (isByScript paymentCred)
        "payment credential is provided by script"
    $ True

    where
        isByKey = \case
            (PaymentCredentialByKey _) -> True
            (PaymentCredentialByScript _) -> False

        isByScript = \case
            (PaymentCredentialByKey _) -> False
            (PaymentCredentialByScript _) -> True

instance Arbitrary PaymentCredential where
    arbitrary = genPaymentCredential

genSimpleScriptCoverageV2 :: Property
genSimpleScriptCoverageV2 =
    forAll (genSimpleScript SimpleScriptV2) $ \script ->
    checkCoverage
    $ cover 10 (requiresSignature script)
       "script has \"require signature\""
    $ cover 10 (requiresAllOf script)
       "script has \"require all of\""
    $ cover 10 (requiresAnyOf script)
       "script has \"require any of\""
    $ cover 10 (requiresMOf script)
       "script has \"require M of\""
    $ cover 10 (requiresTimeBefore script)
       "script has \"time before\""
    $ cover 10 (requiresTimeAfter script)
       "script has \"time after\""
    $ cover 10 (hasOnlyNonRecursiveScriptPrimitive script)
       "script has only non-recursive script primitives (sig, timeBefore, timeAfter)"
    $ cover 10 (not (hasOnlyNonRecursiveScriptPrimitive script))
       "script has recursive script primitives (allOf, anyOf, mOf)"
    $ True

genSimpleScriptCoverageV1 :: Property
genSimpleScriptCoverageV1 =
    forAll (genSimpleScript SimpleScriptV1) $ \script ->
    checkCoverage
    $ cover 10 (requiresSignature script)
       "script has \"require signature\""
    $ cover 10 (requiresAllOf script)
       "script has \"require all of\""
    $ cover 10 (requiresAnyOf script)
       "script has \"require any of\""
    $ cover 10 (requiresMOf script)
       "script has \"require M of\""
    $ cover 10 (hasOnlyNonRecursiveScriptPrimitive script)
       "script has only non-recursive script primitives (sig, timeBefore, timeAfter)"
    $ cover 10 (not (hasOnlyNonRecursiveScriptPrimitive script))
       "script has recursive script primitives (allOf, anyOf, mOf)"
    $ True

hasOnlyNonRecursiveScriptPrimitive :: forall lang. SimpleScript lang -> Bool
hasOnlyNonRecursiveScriptPrimitive = \case
    (RequireSignature _)    -> True
    (RequireTimeBefore _ _) -> True
    (RequireTimeAfter _ _)  -> True
    (RequireAllOf _)        -> False
    (RequireAnyOf _)        -> False
    (RequireMOf _ _)        -> False

-- Returns true if any part of the script might require a signature.
requiresSignature :: forall lang. SimpleScript lang -> Bool
requiresSignature = \case
    (RequireSignature _)    -> True
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresSignature ss
    (RequireAnyOf ss)       -> any requiresSignature ss
    (RequireMOf _ ss)       -> any requiresSignature ss

-- Returns true if any part of the script requires "time before".
requiresTimeBefore :: forall lang. SimpleScript lang -> Bool
requiresTimeBefore = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> True
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresTimeBefore ss
    (RequireAnyOf ss)       -> any requiresTimeBefore ss
    (RequireMOf _ ss)       -> any requiresTimeBefore ss

-- Returns true if any part of the script requires "time after".
requiresTimeAfter :: forall lang. SimpleScript lang -> Bool
requiresTimeAfter = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> True
    (RequireAllOf ss)       -> any requiresTimeBefore ss
    (RequireAnyOf ss)       -> any requiresTimeBefore ss
    (RequireMOf _ ss)       -> any requiresTimeBefore ss

requiresAllOf :: forall lang. SimpleScript lang -> Bool
requiresAllOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> True
    (RequireAnyOf ss)       -> any requiresAllOf ss
    (RequireMOf _ ss)       -> any requiresAllOf ss

requiresAnyOf :: forall lang. SimpleScript lang -> Bool
requiresAnyOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresAnyOf ss
    (RequireAnyOf ss)       -> True
    (RequireMOf _ ss)       -> any requiresAnyOf ss

requiresMOf :: forall lang. SimpleScript lang -> Bool
requiresMOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresMOf ss
    (RequireAnyOf ss)       -> any requiresMOf ss
    (RequireMOf _ ss)       -> True

genAssetIdCoverage :: AssetId -> Property
genAssetIdCoverage assetId = checkCoverage
    $ cover 10 (isAdaAssetId assetId)
        "ADA asset id"
    $ cover 10 (isNonAdaAssetId assetId)
        "non-ADA asset id"
    $ True

    where
        isAdaAssetId = (== AdaAssetId)

        isNonAdaAssetId AdaAssetId    = False
        isNonAdaAssetId (AssetId _ _) = True

instance Arbitrary AssetId where
    arbitrary = genAssetId

genTxIxCoverage :: TxIx -> Property
genTxIxCoverage txIx = checkCoverage
    $ cover 1 (txIx == TxIx 0)
        "txIx is zero"
    $ cover 2 (txIx >= veryLargeTxIx)
        "txIx is very large"
    $ cover 10 (txIx > TxIx 0 && txIx < veryLargeTxIx)
        "txIx is between smallest and very large"
    $ label "no txIx is negative" (txIx >= TxIx 0)
      & counterexample "txIx was negative"
    where
        veryLargeTxIx :: TxIx
        veryLargeTxIx = TxIx $ fromInteger $ toInteger (maxBound :: Word32)

instance Arbitrary TxIx where
    arbitrary = genTxIndex

genTtlCoverage :: SlotNo -> Property
genTtlCoverage ttl = checkCoverage
    $ cover 1 (ttl == 0)
        "ttl is zero"
    $ cover 10 (ttl > 0 && ttl < veryLargeTTL)
        "ttl is between zero and very large"
    $ cover 5 (ttl > veryLargeTTL)
        "ttl is greater than very large"
    $ label "ttl is non-negative" (ttl >= 0)
      & counterexample "ttl was negative"

    where
        veryLargeTTL = fromIntegral (maxBound :: Word32)

genWitnessNetworkIdOrByronAddressCoverage
    :: WitnessNetworkIdOrByronAddress -> Property
genWitnessNetworkIdOrByronAddressCoverage witNetworkOrByron = checkCoverage
    $ cover 10 (isNetworkIdWit witNetworkOrByron)
        "is network id witness"
    $ cover 10 (isByronAddrWit witNetworkOrByron)
        "is byron address witness"
    $ True

    where
        isNetworkIdWit = \case
            WitnessNetworkId _    -> True
            WitnessByronAddress _ -> False

        isByronAddrWit = \case
            WitnessNetworkId _    -> False
            WitnessByronAddress _ -> True

instance Arbitrary WitnessNetworkIdOrByronAddress where
    arbitrary = genWitnessNetworkIdOrByronAddress

instance Show WitnessNetworkIdOrByronAddress where
    show (WitnessNetworkId n) = "WitnessNetworkId " ++ show n
    show (WitnessByronAddress addr) = "WitnessByronAddress " ++ show addr

genTxFeeCoverageByron :: Property
genTxFeeCoverageByron =
    forAll (genTxFee ByronEra) $ \fee ->
        label "fee in byron era is always implicit"
        $ fee == TxFeeImplicit (TxFeesImplicitInByronEra)
        & counterexample "fee wasn't implicit"

genTxFeeCoverageShelley :: Property
genTxFeeCoverageShelley =
    forAll (genTxFee ShelleyEra) $ \(TxFeeExplicit TxFeesExplicitInShelleyEra fee) ->
        genLovelaceCoverage fee

genTxFeeCoverageAllegra :: Property
genTxFeeCoverageAllegra =
    forAll (genTxFee AllegraEra) $ \(TxFeeExplicit TxFeesExplicitInAllegraEra fee) ->
        genLovelaceCoverage fee

genTxFeeCoverageMary :: Property
genTxFeeCoverageMary =
    forAll (genTxFee MaryEra) $ \(TxFeeExplicit TxFeesExplicitInMaryEra fee) ->
        genLovelaceCoverage fee

genTxFeeCoverageAlonzo :: Property
genTxFeeCoverageAlonzo =
    forAll (genTxFee AlonzoEra) $ \(TxFeeExplicit TxFeesExplicitInAlonzoEra fee) ->
        genLovelaceCoverage fee

genTxInCoverage :: TxIn -> Property
genTxInCoverage (TxIn _id ix) =
    -- We don't provide any coverage for genShelleyHash, and so we don't provide
    -- any coverage for txId either (as txId consists of a shelleyHash).
    genTxIxCoverage ix

instance Arbitrary TxIn where
    arbitrary = genTxIn

genTxInCollateralCoverageByron :: Property
genTxInCollateralCoverageByron =
    forAll (genTxInsCollateral ByronEra) $ \collateral ->
        label "collateral is never generated in Byron era" $
            collateral == TxInsCollateralNone
        & counterexample "collateral was generated in Byron era!"

genTxInCollateralCoverageShelley :: Property
genTxInCollateralCoverageShelley =
    forAll (genTxInsCollateral ShelleyEra) $ \collateral ->
        label "collateral is never generated in Shelley era" $
            collateral == TxInsCollateralNone
        & counterexample "collateral was generated in Shelley era!"

genTxInCollateralCoverageAllegra :: Property
genTxInCollateralCoverageAllegra =
    forAll (genTxInsCollateral AllegraEra) $ \collateral ->
        label "collateral is never generated in Allegra era" $
            collateral == TxInsCollateralNone
        & counterexample "collateral was generated in Allegra era!"

genTxInCollateralCoverageMary :: Property
genTxInCollateralCoverageMary =
    forAll (genTxInsCollateral MaryEra) $ \collateral ->
        label "collateral is never generated in Mary era" $
            collateral == TxInsCollateralNone
        & counterexample "collateral was generated in Mary era!"

genTxInCollateralCoverageAlonzo :: Property
genTxInCollateralCoverageAlonzo =
    forAll (genTxInsCollateral AlonzoEra) $ \collateral -> checkCoverage
        $ cover 10 (hasNoCollateral collateral)
            "no collateral"
        $ cover 10 (hasSomeCollateral collateral)
            "some collateral"
        $ cover 2 (collateralLength collateral == Just 0)
            "list of zero collateral"
        $ cover 10 (collateralLength collateral > Just 0)
            "list of more than zero collateral"
        $ cover 10 (collateralLength collateral > Just 3)
            "list of more than three collateral"
        $ True

    where
        hasNoCollateral = (== TxInsCollateralNone)
        hasSomeCollateral = not . hasNoCollateral
        collateralLength = \case
            TxInsCollateralNone  -> Nothing
            TxInsCollateral _ cs -> Just $ length cs

genStakeCredentialCoverage :: StakeCredential -> Property
genStakeCredentialCoverage sc = checkCoverage
    $ cover 10 (isByKey sc)
        "stake credential built from key"
    $ cover 10 (isByScript sc)
        "stake credential built from script"
    $ True

    where
        isByKey = \case
            StakeCredentialByKey _    -> True
            StakeCredentialByScript _ -> False

        isByScript = \case
            StakeCredentialByKey _    -> False
            StakeCredentialByScript _ -> True

instance Arbitrary StakeCredential where
    arbitrary = genStakeCredential

genIxCoverage :: Ix -> Property
genIxCoverage ix = checkCoverage
    -- NOTE: can't use Arbitrary here because Ix is a type synonym
    $ cover 1 (ix == 0)
        "ix is zero"
    $ cover 2 (ix >= veryLargeIx)
        "ix is very large"
    $ cover 10 (ix > 0 && ix < veryLargeIx)
        "ix is between smallest and very large"
    $ label "no ix is negative" (ix >= 0)
      & counterexample "ix was negative"
    where
        veryLargeIx :: Ix
        veryLargeIx = fromInteger $ toInteger (maxBound :: Word32)

genPtrCoverage :: Ptr -> Property
genPtrCoverage (Ptr slotNo ix1 ix2) = conjoin
    [ genSlotNoCoverage slotNo
    , genIxCoverage ix1
    , genIxCoverage ix2
    ]

instance Arbitrary Ptr where
    arbitrary = genPtr

genStakeAddressReferenceCoverage :: StakeAddressReference -> Property
genStakeAddressReferenceCoverage ref = checkCoverage
    $ cover 10 (byValue ref)
        "stake address reference created by value"
    $ cover 10 (byPointer ref)
        "stake address reference created by pointer"
    $ cover 10 (noStakeAddress ref)
        "no stake address"
    $ case ref of
        (StakeAddressByValue cred) ->
            genStakeCredentialCoverage cred
        (StakeAddressByPointer (StakeAddressPointer ptr)) ->
            genPtrCoverage ptr
        NoStakeAddress ->
            property True
    where
        byValue = \case
            StakeAddressByValue _   -> True
            StakeAddressByPointer _ -> False
            NoStakeAddress          -> False

        byPointer = \case
            StakeAddressByValue _   -> False
            StakeAddressByPointer _ -> True
            NoStakeAddress          -> False

        noStakeAddress = \case
            StakeAddressByValue _   -> False
            StakeAddressByPointer _ -> False
            NoStakeAddress          -> True

instance Arbitrary StakeAddressReference where
    arbitrary = genStakeAddressReference

genAddressInByronEraCoverage :: AddressInEra era -> Property
genAddressInByronEraCoverage addr =
    label "in Byron era, always generate byron addresses"
    $ case addr of
        AddressInEra ByronAddressInAnyEra _addr ->
            True
        _ ->
            False
    & counterexample "Non-Byron address was generated in Byron era"

genAddressInShelleyBasedEraCoverage :: AddressInEra era -> Property
genAddressInShelleyBasedEraCoverage addr = checkCoverage
    $ cover 10 (isByronAddress addr)
        "byron address"
    $ cover 10 (isShelleyAddress addr)
        "shelley address"
    $ True

    where
        isByronAddress = \case
            AddressInEra ByronAddressInAnyEra _addr -> True
            _ -> False
        isShelleyAddress = \case
            AddressInEra (ShelleyAddressInEra _era) _addr -> True
            _ -> False

genUnsignedQuantityCoverage :: Quantity -> Property
genUnsignedQuantityCoverage qty = checkCoverage
    $ cover 1 (qty == 0)
        "unsigned quantity is zero"
    $ cover 1 (qty >= veryLargeQuantity)
        "unsigned quantity is very large"
    $ cover 10 (qty > 0 && qty < veryLargeQuantity)
        "unsigned quantity is between zero and very large"
    $ label "unsigned quantity >= 0" (qty >= 0)
      & counterexample "unsigned quantity was signed!"

    where
        veryLargeQuantity :: Quantity
        veryLargeQuantity = fromInteger $ toInteger (maxBound :: Word32)

genValueForTxOutCoverage :: Value -> Property
genValueForTxOutCoverage val =
    let
        valList = valueToList val
    in
        checkCoverage
        $ cover 1 (null valList)
            "Value has no assets"
        $ cover 10 (length valList >= 1)
            "Value has some assets"
        $ cover 10 (length valList >= 3)
            "Value has more assets"
        $ True

genTxOutValueCoverageAdaOnly :: TxOutValue era -> Property
genTxOutValueCoverageAdaOnly val = conjoin
    [ label "ada only supported in era" (isAdaOnly val)
      & counterexample "era should only support ada"
    , case val of
          (TxOutAdaOnly _ l) -> genLovelaceCoverage l
          _                  -> property False
    ]

    where
        isAdaOnly = \case
            TxOutAdaOnly _ _ -> True
            _                -> False

genTxOutValueCoverageMultiAsset :: TxOutValue era -> Property
genTxOutValueCoverageMultiAsset val = conjoin
    [ label "multi-asset supported in era" (isMultiAsset val)
      & counterexample "multi-asset should be supported in era"
    , case val of
          (TxOutValue _ value) -> genValueForTxOutCoverage value
          _                    -> property False
    ]

    where
        isMultiAsset = \case
            TxOutValue _ _ -> True
            _              -> False

genTxOutDatumHashNoneCoverage :: TxOutDatumHash era -> Property
genTxOutDatumHashNoneCoverage datum =
    label "tx out datums not present in era" (datum == TxOutDatumHashNone)
    & counterexample "tx out datums shouldn't be present in era"

genTxOutDatumHashCoverage :: TxOutDatumHash era -> Property
genTxOutDatumHashCoverage datum = checkCoverage
    $ cover 10 (hasNoDatumHash datum)
        "no tx out datum hash"
    $ cover 10 (hasDatumHash datum)
        "tx out datum hash present"
    $ True

    where
        hasNoDatumHash = (== TxOutDatumHashNone)

        hasDatumHash = \case
            TxOutDatumHashNone   -> False
            (TxOutDatumHash _ _) -> True

genTxOutCoverageByron :: TxOut era -> Property
genTxOutCoverageByron (TxOut addr val datum) = conjoin
    [ genAddressInByronEraCoverage addr
    , genTxOutValueCoverageAdaOnly val
    , genTxOutDatumHashNoneCoverage datum
    ]

genTxOutCoverageShelley :: TxOut era -> Property
genTxOutCoverageShelley (TxOut addr val datum) = conjoin
    [ genAddressInShelleyBasedEraCoverage addr
    , genTxOutValueCoverageAdaOnly val
    , genTxOutDatumHashNoneCoverage datum
    ]

genTxOutCoverageAllegra :: TxOut era -> Property
genTxOutCoverageAllegra (TxOut addr val datum) = conjoin
    [ genAddressInShelleyBasedEraCoverage addr
    , genTxOutValueCoverageAdaOnly val
    , genTxOutDatumHashNoneCoverage datum
    ]

genTxOutCoverageMary :: TxOut era -> Property
genTxOutCoverageMary (TxOut addr val datum) = conjoin
    [ genAddressInShelleyBasedEraCoverage addr
    , genTxOutValueCoverageMultiAsset val
    , genTxOutDatumHashNoneCoverage datum
    ]

genTxOutCoverageAlonzo :: TxOut era -> Property
genTxOutCoverageAlonzo (TxOut addr val datum) = conjoin
    [ genAddressInShelleyBasedEraCoverage addr
    , genTxOutValueCoverageMultiAsset val
    , genTxOutDatumHashCoverage datum
    ]

genTxValidityLowerBoundNotSupportedCoverage :: TxValidityLowerBound era -> Property
genTxValidityLowerBoundNotSupportedCoverage validFrom =
    label "validity lower bound not supported" (validFrom == TxValidityNoLowerBound)
    & counterexample "validity lower bound shouldn't be supported"

genTxValidityLowerBoundSupportedCoverage :: TxValidityLowerBound era -> Property
genTxValidityLowerBoundSupportedCoverage = \case
    TxValidityNoLowerBound ->
        False
        & counterexample "validity lower bound supported in era, should have lower bound"
    TxValidityLowerBound _ ttl ->
        genTtlCoverage ttl

genTxValidityNoUpperBoundCoverage :: TxValidityUpperBound era -> Property
genTxValidityNoUpperBoundCoverage = \case
    (TxValidityUpperBound _ _) ->
        False & counterexample "upper bound should not be supported in era"
    (TxValidityNoUpperBound _) ->
        True & label "no upper bound in era"

genTxValidityUpperBoundCoverage :: TxValidityUpperBound era -> Property
genTxValidityUpperBoundCoverage = \case
    (TxValidityNoUpperBound _) ->
        False & counterexample "upper bound should be supported in era"
    (TxValidityUpperBound _ ttl) ->
        genTtlCoverage ttl

genTxValidityRangeByronCoverage :: (TxValidityLowerBound era, TxValidityUpperBound era) -> Property
genTxValidityRangeByronCoverage (lower, upper) = conjoin
    [ genTxValidityLowerBoundNotSupportedCoverage lower
    , genTxValidityNoUpperBoundCoverage upper
    ]

genTxValidityRangeShelleyCoverage :: (TxValidityLowerBound era, TxValidityUpperBound era) -> Property
genTxValidityRangeShelleyCoverage (lower, upper) = conjoin
    [ genTxValidityLowerBoundNotSupportedCoverage lower
    , genTxValidityUpperBoundCoverage upper
    ]

genTxValidityRangeAllegraCoverage :: (TxValidityLowerBound era, TxValidityUpperBound era) -> Property
genTxValidityRangeAllegraCoverage (lower, upper) = conjoin
    [ genTxValidityLowerBoundSupportedCoverage lower
    , genTxValidityUpperBoundCoverage upper
    ]

genTxValidityRangeMaryCoverage :: (TxValidityLowerBound era, TxValidityUpperBound era) -> Property
genTxValidityRangeMaryCoverage (lower, upper) = conjoin
    [ genTxValidityLowerBoundSupportedCoverage lower
    , genTxValidityUpperBoundCoverage upper
    ]

genTxValidityRangeAlonzoCoverage :: (TxValidityLowerBound era, TxValidityUpperBound era) -> Property
genTxValidityRangeAlonzoCoverage (lower, upper) = conjoin
    [ genTxValidityLowerBoundSupportedCoverage lower
    , genTxValidityUpperBoundCoverage upper
    ]

genScriptDataNumberCoverage :: Integer -> Property
genScriptDataNumberCoverage n = checkCoverage
    $ cover 1 (n == 0)
        "number is equal to 0"
    $ cover 2 (n >= veryLargeNumber)
        "number is very large"
    $ cover 2 (n <= verySmallNumber)
        "number is very small"
    $ cover 10 (n > verySmallNumber && n < veryLargeNumber)
        "number is between very small and very large"
    $ True

    where
        verySmallNumber = fromIntegral $ (minBound :: Int32)
        veryLargeNumber = fromIntegral $ (maxBound :: Int32)

genScriptDataBytesCoverage :: ByteString -> Property
genScriptDataBytesCoverage bs = checkCoverage
    $ cover 1 (BS.length bs == 0)
        "no bytes"
    $ cover 10 (BS.length bs > 0)
        "some bytes"
    $ cover 2 (BS.length bs > 32)
        "lots of bytes"
    $ True

genScriptDataListCoverage :: [ScriptData] -> Property
genScriptDataListCoverage ss = checkCoverage
    $ cover 1 (length ss == 0)
        "no scripts in list"
    $ cover 10 (length ss > 0)
        "some scripts in list"
    $ cover 10 (length ss > 32)
        "lots of scripts in list"
    $ conjoin
    $ fmap genScriptDataCoverage ss

genScriptDataMapCoverage :: [(ScriptData, ScriptData)] -> Property
genScriptDataMapCoverage ss = checkCoverage
    $ cover 1 (length ss == 0)
        "no scripts in map"
    $ cover 10 (length ss > 0)
        "some scripts in map"
    $ cover 10 (length ss > 32)
        "lots of scripts in map"
    $ conjoin
    $ fmap (\(k, v) ->
                conjoin [genScriptDataCoverage k, genScriptDataCoverage v]
           ) ss

genScriptDataConstructorCoverage :: (Integer, [ScriptData]) -> Property
genScriptDataConstructorCoverage (ix, ss) = checkCoverage
    $ cover 1 (length ss == 0)
        "no scripts in constr"
    $ cover 10 (length ss > 0)
        "some scripts in constr"
    $ cover 10 (length ss > 3)
        "lots of scripts in constr"
    $ cover 1 (ix == 0)
        "ix == 0"
    $ cover 10 (ix > 0)
        "ix > 0"
    $ cover 10 (ix > 3)
        "ix > 3"
    $ True

genScriptDataCoverage :: ScriptData -> Property
genScriptDataCoverage dat = conjoin
    [ checkCoverage
      $ case dat of
          ScriptDataNumber _        -> cover 10 True "is script data number"
          ScriptDataBytes _         -> cover 10 True "is script data bytes"
          ScriptDataList _          -> cover 10 True "is script data list"
          ScriptDataMap _           -> cover 10 True "is script data map"
          ScriptDataConstructor _ _ -> cover 10 True "is script data constructor"
      $ True
    , case dat of
          ScriptDataNumber n        ->
              genScriptDataNumberCoverage n
          ScriptDataBytes bs        ->
              genScriptDataBytesCoverage bs
          ScriptDataList ss          ->
              genScriptDataListCoverage ss
          ScriptDataMap ss           ->
              genScriptDataMapCoverage ss
          ScriptDataConstructor n ss ->
              genScriptDataConstructorCoverage (n, ss)
    ]

instance Arbitrary ScriptData where
    arbitrary = genScriptData

genExecutionUnitsCoverage :: ExecutionUnits -> Property
genExecutionUnitsCoverage (ExecutionUnits steps mem) = conjoin
    [ checkCoverage
      $ cover 1 (steps == 0)
        "execution steps is zero"
      $ cover 2 (steps >= veryLargeNumber)
        "execution steps is very large"
      $ cover 10 (steps > 0 && steps < veryLargeNumber)
        "execution steps is between smallest and very large"
      $ label "no execution steps is negative" (steps >= 0)
        & counterexample "execution steps was negative"
    , checkCoverage
      $ cover 1 (mem == 0)
        "execution mem is zero"
      $ cover 2 (mem >= veryLargeNumber)
        "execution mem is very large"
      $ cover 10 (mem > 0 && mem < veryLargeNumber)
        "execution mem is between smallest and very large"
      $ label "no execution mem is negative" (mem >= 0)
        & counterexample "execution mem was negative"
    ]

    where
        veryLargeNumber = fromIntegral (maxBound :: Word32)

instance Arbitrary ExecutionUnits where
    arbitrary = genExecutionUnits

-- genWitnessStakeCoverage :: CardanoEra era -> Witness WitCtxStake era -> Property
-- genWitnessStakeCoverage era wit =
--     case era of
--           ByronEra -> checkCoverage
--               $ cover 100 (wit == KeyWitness KeyWitnessForStakeAddr)
--                 "byron era always generates key witnesses"
--               $ True
--           ShelleyEra -> checkCoverage
--               $ cover 10 (isKeyWitness wit)
--                 "key witness in Shelley era"
--               $ cover 10 (isSimpleScriptWitness wit)
--                 "script witness in Shelley era"
--               $ cover 100 (isKeyWitness wit || isSimpleScriptWitness wit)
--                 "is appropriate key or script witness"
--               $ True
--           AllegraEra -> checkCoverage
--               $ cover 10 (isKeyWitness wit)
--                 "key witness in Allegra era"
--               $ cover 10 (isSimplewit)
--           MaryEra -> checkCoverage $ cover 100 True "" $ True
--           AlonzoEra -> checkCoverage $ cover 100 True "" $ True

--     where
--         isKeyWitness =
--             (KeyWitness KeyWitnessForStakeAddr ==)

--         isSimpleScriptWitness = \case
--             KeyWitness _                              -> False
--             ScriptWitness ScriptWitnessForStakeAddr _ -> True
--             ScriptWitness _ _                         -> False

genScriptValidityCoverage :: ScriptValidity -> Property
genScriptValidityCoverage scriptValidity = checkCoverage
    $ cover 10 (scriptValidity == ScriptInvalid)
        "script is invalid"
    $ cover 10 (scriptValidity == ScriptValid)
        "script is valid"
    $ True

instance Arbitrary ScriptValidity where
    arbitrary = genScriptValidity

genTxScriptValidityCoverage :: CardanoEra era -> TxScriptValidity era -> Property
genTxScriptValidityCoverage era txScriptValidity =
    case txScriptValiditySupportedInCardanoEra era of
        Nothing -> checkCoverage
            $ cover 100 (txScriptValidity == TxScriptValidityNone)
              "script validity is always none in eras it is not supported"
            $ True
        Just _  -> checkCoverage
            $ cover 100 (hasScriptValidity txScriptValidity)
              "script validity is always present in eras it is supported"
            $ case txScriptValidity of
                  TxScriptValidityNone ->
                      False & counterexample "era should have script validity"
                  TxScriptValidity _ scriptValidity ->
                      genScriptValidityCoverage scriptValidity
    where
        hasScriptValidity = \case
            TxScriptValidityNone -> False
            TxScriptValidity _ _ -> True

genShelleyWitnessSigningKeyCoverage :: ShelleyWitnessSigningKey -> Property
genShelleyWitnessSigningKeyCoverage sk = checkCoverage
    $ case sk of
          WitnessPaymentKey _ ->
              cover 10 True "is witness payment key"
          WitnessPaymentExtendedKey _ ->
              cover 10 True "is witness payment extended key"
          WitnessStakeKey _ ->
              cover 10 True "is witness stake key"
          WitnessStakeExtendedKey _ ->
              cover 10 True "is witness stake extended key"
          WitnessStakePoolKey _ ->
              cover 10 True "is witness stake pool key"
          WitnessGenesisDelegateKey _ ->
              cover 10 True "is witness genesis delegate key"
          WitnessGenesisUTxOKey _ ->
              cover 10 True "is witness genesis utxo key"
    $ True

instance Arbitrary ShelleyWitnessSigningKey where
    arbitrary = genShelleyWitnessSigningKey

instance Show ShelleyWitnessSigningKey where
    show (WitnessPaymentKey sk) = show sk
    show (WitnessPaymentExtendedKey sk) = show sk
    show (WitnessStakeKey sk) = show sk
    show (WitnessStakeExtendedKey sk) = show sk
    show (WitnessStakePoolKey sk) = show sk
    show (WitnessGenesisKey sk) = show sk
    show (WitnessGenesisExtendedKey sk) = show sk
    show (WitnessGenesisDelegateKey sk) = show sk
    show (WitnessGenesisDelegateExtendedKey sk) = show sk
    show (WitnessGenesisUTxOKey sk) = show sk

genValueForMintingCoverage :: Value -> Property
genValueForMintingCoverage val = checkCoverage
    $ cover 10 (hasMintingValue val)
      "minting assets"
    $ cover 10 (hasBurningValue val)
      "burning assets"
    $ conjoin
      [ label "no empty mint/burn" (not $ hasZeroValue val)
        & counterexample "shouldn't generate a zero mint/burn value"
      , label "is never ADA value (can't mint ADA)" (hasNoAdaValue val)
        & counterexample "generated ADA mint (you can't mint ADA!)"
      ]

    where
        hasNoAdaValue = all ((/= AdaAssetId) . fst) . valueToList

        hasMintingValue = any ((> 0) . snd) . valueToList

        hasBurningValue = any ((< 0) . snd) . valueToList

        hasZeroValue = any ((== 0) . snd) . valueToList

genTxMintValueCoverage :: CardanoEra era -> TxMintValue BuildTx era -> Property
genTxMintValueCoverage era val =
    case multiAssetSupportedInEra era of
        Left _ ->
            label "mint values are not generated in unsupported eras"
                (val == TxMintNone)
            & counterexample "a mint value was generated in an unsupported era"
        Right _ ->
            checkCoverage
            $ cover 10 (noMint val)
              "no mint"
            $ cover 10 (someMint val)
              "mint"
            $ case val of
                TxMintNone -> property True
                (TxMintValue _ value _) ->
                    genValueForMintingCoverage value

    where
        noMint = (== TxMintNone)

        someMint = \case
            TxMintNone -> False
            TxMintValue _ _ _ -> True

genExtraKeyWitnessesCoverage
    :: CardanoEra era -> TxExtraKeyWitnesses era -> Property
genExtraKeyWitnessesCoverage era ws =
    case extraKeyWitnessesSupportedInEra era of
        Nothing ->
            label "extra key witnesses are not generated in unsupported eras"
                (noWitnesses ws)
            & counterexample "key witnesses were generated in an unsupported era"
        Just _ -> checkCoverage
            $ cover 10 (noWitnesses ws)
                "no witnesses"
            $ cover 10 (witnesses ws)
                "witnesses"
            $ case ws of
                TxExtraKeyWitnessesNone -> property True
                (TxExtraKeyWitnesses _ wits) -> checkCoverage
                    $ cover 1 (length wits == 0)
                       "empty witneses"
                    $ cover 30 (length wits > 0)
                       "some witnesses"
                    $ cover 10 (length wits > 3)
                       "> 3 witnesses"
                    $ True

    where
        noWitnesses = (== TxExtraKeyWitnessesNone)

        witnesses = \case
            TxExtraKeyWitnessesNone -> False
            TxExtraKeyWitnesses _ _ -> True

genTxMetadataValueCoverage :: TxMetadataValue -> Property
genTxMetadataValueCoverage meta =
    checkCoverage
        $ cover 10 (isMetaNumber meta)
          "is TxMetaNumber"
        $ cover 10 (isMetaBytes meta)
          "is TxMetaBytes"
        $ cover 10 (isMetaText meta)
          "is TxMetaText"
        $ cover 10 (isMetaList meta)
          "is TxMetaList"
        $ cover 10 (isMetaMap meta)
          "is TxMetaMap"
        $ True

    where
        isMetaNumber = \case
            TxMetaNumber _ -> True
            _              -> False

        isMetaBytes = \case
            TxMetaBytes _ -> True
            _             -> False

        isMetaText = \case
            TxMetaText _ -> True
            _            -> False

        isMetaList = \case
            TxMetaList _ -> True
            _            -> False

        isMetaMap = \case
            TxMetaMap _  -> True
            _            -> False

instance Arbitrary TxMetadataValue where
    arbitrary = genTxMetadataValue

genTxMetadataCoverage :: TxMetadata -> Property
genTxMetadataCoverage (TxMetadata meta) =
    let
        metaMap = Map.toList meta
    in
        checkCoverage
            $ cover 1 (length metaMap == 0)
              "no metadata entries"
            $ cover 10 (length metaMap > 0)
              "some metadata entries"
            $ cover 10 (length metaMap > 10)
              "lots of metadata entries"
            $ conjoin $ fmap (metaNumberCoverage . fst) metaMap

    where
        metaNumberCoverage n = checkCoverage
            $ cover 1 (n == 0)
              "meta index == 0"
            $ cover 10 (n > 0)
              "meta index > 0"
            $ cover 10 (n >= veryLargeMetaIndex)
              "meta index is large"
            $ cover 10 (n > 0 && n < veryLargeMetaIndex)
              "meta index is between smallest and large"
            $ property (n >= 0)
              & counterexample "meta index was negative"

        veryLargeMetaIndex = fromIntegral (maxBound :: Word32)

instance Arbitrary TxMetadata where
    arbitrary = genTxMetadata

-- genPositiveNumberCoverage :: (Integral a, Bounded b) => Proxy b -> a -> Property
-- genPositiveNumberCoverage n = checkCoverage
--     $ cover 1 (n == 0)
--       "some are zero"
--     $
--     $ n >= 0
--       & counterexample "positive number was negative"

--     where
--         veryLargePositiveNumber :: a
--         veryLargePositiveNumber = fromIntegral (maxBound :: b)
