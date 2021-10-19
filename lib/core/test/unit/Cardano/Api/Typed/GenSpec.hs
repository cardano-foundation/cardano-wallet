{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Typed.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( AddressInEra (..)
    , AddressTypeInEra (..)
    , AnyCardanoEra (..)
    , AssetId (..)
    , AssetName (..)
    , CardanoEra (..)
    , EpochNo (..)
    , Lovelace
    , NetworkId (..)
    , NetworkMagic (..)
    , PaymentCredential (..)
    , Quantity (..)
    , ScriptData (..)
    , SimpleScript (..)
    , SimpleScriptVersion (..)
    , SlotNo (..)
    , StakeAddressPointer (..)
    , StakeAddressReference (..)
    , StakeCredential (..)
    , TxFee (..)
    , TxFeesExplicitInEra (..)
    , TxFeesImplicitInEra (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , TxOut (..)
    , TxOutDatumHash (..)
    , TxOutValue (..)
    , TxValidityLowerBound (..)
    , TxValidityUpperBound (..)
    , Value (..)
    , quantityToLovelace
    , valueToList
    )
import Cardano.Api.Byron
    ( WitnessNetworkIdOrByronAddress (..) )
import Cardano.Api.Shelley
    ( StakeCredential (..) )
import Cardano.Api.Typed.Gen.QuickCheck
import Cardano.Ledger.Credential
    ( Ix (..), Ptr (..) )
import Data.ByteString
    ( ByteString )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Char
    ( isAlphaNum, isDigit, isLower, isUpper )
import Data.Function
    ( (&) )
import Data.Int
    ( Int32, Int64 )
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
import qualified Hedgehog as H
import qualified Test.Hspec.Hedgehog as Hspec

spec :: Spec
spec =
    describe "Cardano.Api.Typed.Gen" $
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
    $ cover 10 (ix == 0)
        "ix == 0"
    $ cover 10 (ix > 0)
        "ix > 0"
    $ cover 10 (ix > 3)
        "ix > 3"
    $ True

genScriptDataCoverage :: ScriptData -> Property
genScriptDataCoverage dat = checkCoverage
    $ case dat of
        ScriptDataNumber _        -> cover 10 True "is script data number"
        ScriptDataBytes _         -> cover 10 True "is script data bytes"
        ScriptDataList _          -> cover 10 True "is script data list"
        ScriptDataMap _           -> cover 10 True "is script data map"
        ScriptDataConstructor _ _ -> cover 10 True "is script data constructor"
    $ True

instance Arbitrary ScriptData where
    arbitrary = genScriptData
