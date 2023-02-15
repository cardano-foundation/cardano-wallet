{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Cardano.Api.GenSpec (spec) where

import Prelude

import Cardano.Api
    ( AddressInEra (..)
    , AddressTypeInEra (..)
    , AnyCardanoEra (..)
    , AnyPlutusScriptVersion
    , AnyScriptLanguage (..)
    , AssetId (..)
    , AssetName (..)
    , BuildTx
    , BuildTxWith (..)
    , ByronEra
    , CardanoEra (..)
    , CardanoEraStyle (..)
    , CostModel (..)
    , EpochNo (..)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , KeyWitnessInCtx (..)
    , Lovelace
    , MIRTarget (..)
    , NetworkId (..)
    , NetworkMagic (..)
    , PaymentCredential (..)
    , Quantity (..)
    , ScriptValidity (..)
    , ShelleyWitnessSigningKey (..)
    , SimpleScript (..)
    , SimpleScriptVersion (..)
    , SlotNo (..)
    , StakeAddressReference (..)
    , TxAuxScripts (..)
    , TxCertificates (..)
    , TxExtraKeyWitnesses (..)
    , TxFee (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxIx (..)
    , TxMetadata (..)
    , TxMetadataInEra (..)
    , TxMetadataValue (..)
    , TxMintValue (..)
    , TxOut (..)
    , TxOutDatum (..)
    , TxOutValue (..)
    , TxScriptValidity (..)
    , TxUpdateProposal (..)
    , TxValidityLowerBound (..)
    , TxValidityUpperBound (..)
    , TxWithdrawals (..)
    , UpdateProposal (..)
    , Value
    , WitCtxStake
    , Witness (..)
    , auxScriptsSupportedInEra
    , cardanoEraStyle
    , certificatesSupportedInEra
    , collateralSupportedInEra
    , extraKeyWitnessesSupportedInEra
    , multiAssetSupportedInEra
    , scriptDataSupportedInEra
    , scriptLanguageSupportedInEra
    , txFeesExplicitInEra
    , txMetadataSupportedInEra
    , txScriptValiditySupportedInCardanoEra
    , updateProposalSupportedInEra
    , validityLowerBoundSupportedInEra
    , validityUpperBoundSupportedInEra
    , valueToList
    , withdrawalsSupportedInEra
    )
import Cardano.Api.Byron
    ( KeyWitness (ByronKeyWitness), WitnessNetworkIdOrByronAddress (..) )
import Cardano.Api.Gen
    ( genAddressInEra
    , genAlphaNum
    , genAssetName
    , genByronKeyWitness
    , genCertIx
    , genCostModel
    , genCostModels
    , genEpochNo
    , genExecutionUnitPrices
    , genExecutionUnits
    , genExtraKeyWitnesses
    , genLovelace
    , genMIRPot
    , genMIRTarget
    , genNat
    , genNetworkId
    , genNetworkMagic
    , genPaymentCredential
    , genPtr
    , genRational
    , genRationalInt64
    , genScriptValidity
    , genShelleyWitnessSigningKey
    , genSignedQuantity
    , genSimpleScript
    , genSlotNo
    , genSlotNo32
    , genStakeAddressReference
    , genStakeCredential
    , genTxAuxScripts
    , genTxBody
    , genTxCertificate
    , genTxCertificates
    , genTxFee
    , genTxIn
    , genTxIndex
    , genTxInsCollateral
    , genTxIx
    , genTxMetadata
    , genTxMetadataInEra
    , genTxMetadataValue
    , genTxMintValue
    , genTxOut
    , genTxOutDatum
    , genTxOutValue
    , genTxScriptValidity
    , genTxValidityLowerBound
    , genTxValidityRange
    , genTxValidityUpperBound
    , genTxWithdrawals
    , genUnsignedQuantity
    , genUpdateProposal
    , genValueForMinting
    , genValueForTxOut
    , genWitnessNetworkIdOrByronAddress
    , genWitnessStake
    , genWitnesses
    )
import Cardano.Api.Shelley
    ( Certificate (..)
    , ReferenceScript (..)
    , StakeCredential (..)
    , refInsScriptsAndInlineDatsSupportedInEra
    )
import Cardano.Chain.UTxO
    ( TxInWitness (..) )
import Cardano.Ledger.Credential.Safe
    ( Ptr, SlotNo32, safePtr, safeUnwrapPtr )
import Cardano.Ledger.Shelley.API
    ( MIRPot (..) )
import Data.Char
    ( isAlphaNum, isDigit, isLower, isUpper )
import Data.Foldable
    ( traverse_ )
import Data.Function
    ( (&) )
import Data.Int
    ( Int32 )
import Data.List
    ( (\\) )
import Data.Map.Strict
    ( Map )
import Data.Ratio
    ( denominator, numerator )
import Data.Word
    ( Word32, Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary
    , Property
    , arbitrary
    , checkCoverage
    , conjoin
    , counterexample
    , cover
    , forAll
    , label
    , property
    , (===)
    )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
    ( CertIx (..), TxIx (..) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Plutus

spec :: Spec
spec =
    describe "Cardano.Api.Gen" $
        describe "Generator coverage" $ do
            it "genTxIx" $
                property genTxIxCoverage
            it "genTxIn" $
                property genTxInCoverage
            describe "genTxInsCollateral" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxInsCollateral era)
                        $ genTxInCollateralCoverage era
            it "genSlotNo" $
                property genSlotNoCoverage
            it "genLovelace" $
                property genLovelaceCoverage
            describe "genTxFee" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxFee era)
                        $ genTxFeeCoverage era
            it "genTtl" $
                property genTtlCoverage
            describe "genTxValidityLowerBound" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxValidityLowerBound era)
                        $ genTxValidityLowerBoundCoverage era
            describe "genTxValidityUpperBound" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxValidityUpperBound era)
                        $ genTxValidityUpperBoundCoverage era
            describe "genTxValidityRangeBound" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxValidityRange era)
                        $ genTxValidityRangeCoverage era
            it "genScriptValidity" $
                property genScriptValidityCoverage
            describe "genTxScriptValidity" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxScriptValidity era)
                        $ genTxScriptValidityCoverage era
            describe "genExtraKeyWitnesses" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genExtraKeyWitnesses era)
                        $ genExtraKeyWitnessesCoverage era
            it "genSimpleScriptV1" $
                property genSimpleScriptCoverageV1
            it "genSimpleScriptV2" $
                property genSimpleScriptCoverageV2
            it "genAssetName" $
                property genAssetNameCoverage
            it "genAlphaNum" $
                property genAlphaNumCoverage
            it "genValueForMinting" $
                property
                $ forAll genValueForMinting genValueForMintingCoverage
            it "genSignedQuantity" $
                property genSignedQuantityCoverage
            describe "genTxMintValue" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxMintValue era)
                        $ genTxMintValueCoverage era
            it "genNetworkMagic" $
                property genNetworkMagicCoverage
            it "genNetworkId" $
                property genNetworkIdCoverage
            it "genStakeCredential" $
                property genStakeCredentialCoverage
            -- it "genScriptDataCoverage" $
            --     property genScriptDataCoverage
            it "genExecutionUnits" $
                property genExecutionUnitsCoverage
            describe "genWitnessStake" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genWitnessStake era)
                        $ genWitnessStakeCoverage era
            describe "genTxWithdrawals" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxWithdrawals era)
                        $ genTxWithdrawalsCoverage era
            describe "genTxAuxScripts" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxAuxScripts era)
                        $ genTxAuxScriptsCoverage era
            it "genTxMetadataValue" $
                property genTxMetadataValueCoverage
            it "genTxMetadata" $
                property genTxMetadataCoverage
            describe "genTxMetadataInEra" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxMetadataInEra era)
                        $ genTxMetadataInEraCoverage era
            it "genTxIx" $
                -- NOTE: can't use Arbitrary here because Ix is a type synonym
                property (forAll genTxIx genTxIxCoverage')
            it "genPtrCoverage" $
                property genPtrCoverage
            it "prop_safePtr_safeUnwrapPtr" $
                property prop_safePtr_safeUnwrapPtr
            it "genStakeAddressReference" $
                property genStakeAddressReferenceCoverage
            it "genPaymentCredential" $
                property genPaymentCredentialCoverage
            describe "genAddressInEra" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genAddressInEra era)
                            (genAddressInEraCoverage era)
            it "genUnsignedQuantity" $
                property $
                    forAll genUnsignedQuantity genUnsignedQuantityCoverage
            it "genValueForTxOutCoverage" $
                property $ forAll genValueForTxOut genValueForTxOutCoverage
            describe "genTxOutDatum" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genTxOutDatum era)
                            (genTxOutDatumCoverage era)
            describe "genTxOutValue" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genTxOutValue era)
                            (genTxOutValueCoverage era)
            describe "genTxOut" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genTxOut era)
                            (genTxOutCoverage era)
            it "genWitnessNetworkIdOrByronAddress" $
                property genWitnessNetworkIdOrByronAddressCoverage
            it "genByronKeyWitness" $
                property $ forAll genByronKeyWitness genByronKeyWitnessCoverage
            it "genShelleyWitnessSigningKey" $
                property genShelleyWitnessSigningKeyCoverage
            describe "genWitnesses" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property
                        $ forAll (genTxBody era)
                        $ \body -> forAll (genWitnesses era body)
                        $ genWitnessesCoverage era
            -- describe "genTxBodyContent" $ do
            --     it "ByronEra" $
            --         property
            --         $ forAll (genTxBodyContent ByronEra)
            --         $ genTxBodyContentCoverage ByronEra
            --     it "ShelleyEra" $
            --         property
            --         $ forAll (genTxBodyContent ShelleyEra)
            --         $ genTxBodyContentCoverage ShelleyEra
            --     it "AllegraEra" $
            --         property
            --         $ forAll (genTxBodyContent AllegraEra)
            --         $ genTxBodyContentCoverage AllegraEra
            --     it "MaryEra" $
            --         property
            --         $ forAll (genTxBodyContent MaryEra)
            --         $ genTxBodyContentCoverage MaryEra
            --     it "AlonzoEra" $
            --         property
            --         $ forAll (genTxBodyContent AlonzoEra)
            --         $ genTxBodyContentCoverage AlonzoEra
            -- describe "genTx" $ do
            --     it "ByronEra" $
            --         property
            --         $ forAll (genTx ByronEra)
            --         $ genTxCoverage ByronEra
            --     it "ShelleyEra" $
            --         property
            --         $ forAll (genTx ShelleyEra)
            --         $ genTxCoverage ShelleyEra
            --     it "AllegraEra" $
            --         property
            --         $ forAll (genTx AllegraEra)
            --         $ genTxCoverage AllegraEra
            --     it "MaryEra" $
            --         property
            --         $ forAll (genTx MaryEra)
            --         $ genTxCoverage MaryEra
            --     it "AlonzoEra" $
            --         property
            --         $ forAll (genTx AlonzoEra)
            --         $ genTxCoverage AlonzoEra
            it "genNat" $
                property genNatCoverage
            it "genRationalCoverage" $
                property $ forAll genRational genRationalCoverage
            it "genRationalInt64Coverage" $
                property $ forAll genRationalInt64 genRationalInt64Coverage
            it "genEpochNo" $
                property genEpochNoCoverage
            it "genCostModel" $
                property genCostModelCoverage
            it "genCostModels" $
                property $ forAll genCostModels genCostModelsCoverage
            it "genExecutionUnitPrices" $
                property genExecutionUnitPricesCoverage
            -- it "genProtocolParameters" $
            --     property genProtocolParametersCoverage
            it "genMIRPot" $
                property genMIRPotCoverage
            it "genMIRTarget" $
                property genMIRTargetCoverage
            it "genTxCertificate" $
                property genTxCertificateCoverage
            describe "genTxCertificates" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genTxCertificates era)
                            (genTxCertificatesCoverage era)
            describe "genUpdateProposal" $
                forAllEras $ \(AnyCardanoEra era) ->
                    it (show era) $
                        property $ forAll
                            (genUpdateProposal era)
                            (genUpdateProposalCoverage era)

genTxIxCoverage :: TxIx -> Property
genTxIxCoverage (TxIx ix) = unsignedCoverage (6 * maxBound @Word8) "txIx" ix

instance Arbitrary TxIx where
    arbitrary = genTxIndex

genTxIxCoverage' :: Ledger.TxIx -> Property
genTxIxCoverage' (Ledger.TxIx ix) =
    unsignedCoverage (maxBound @Word32 - 1024) "txIx" ix

instance Arbitrary Ledger.TxIx where
    arbitrary = genTxIx

genCertIxCoverage :: Ledger.CertIx -> Property
genCertIxCoverage (Ledger.CertIx ix) =
    unsignedCoverage (maxBound @Word32 - 1024) "certIx" ix

instance Arbitrary Ledger.CertIx where
    arbitrary = genCertIx

genTxInCoverage :: TxIn -> Property
genTxInCoverage (TxIn _id ix) =
    -- We don't provide any coverage for genShelleyHash, and so we don't provide
    -- any coverage for txId either (as txId consists of a shelleyHash).
    genTxIxCoverage ix

instance Arbitrary TxIn where
    arbitrary = genTxIn

genTxInCollateralCoverage :: CardanoEra era -> TxInsCollateral era -> Property
genTxInCollateralCoverage era collateral =
    case collateralSupportedInEra era of
        Nothing ->
            collateral == TxInsCollateralNone
            & label ("collateral is never generated in " <> show era)
            & counterexample ("collateral was generated in " <> show era)
        Just _ ->
            checkCoverage
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
              True

    where
        hasNoCollateral = (== TxInsCollateralNone)

        hasSomeCollateral = \case
            TxInsCollateralNone -> False
            TxInsCollateral _ _ -> True

        collateralLength = \case
            TxInsCollateralNone  -> Nothing
            TxInsCollateral _ cs -> Just $ length cs

genSlotNoCoverage :: SlotNo -> Property
genSlotNoCoverage = unsignedCoverage (maxBound @Word64 - 1000) "slot number"

genSlotNo32Coverage :: SlotNo32 -> Property
genSlotNo32Coverage =
    unsignedCoverage (maxBound @Word32 - 10000) "slot number (32-bit)"

instance Arbitrary SlotNo where
    arbitrary = genSlotNo

genLovelaceCoverage :: Lovelace -> Property
genLovelaceCoverage = unsignedCoverage (maxBound @Word32) "lovelace"

instance Arbitrary Lovelace where
    arbitrary = genLovelace

genTxFeeCoverage :: CardanoEra era -> TxFee era -> Property
genTxFeeCoverage era fee =
    case txFeesExplicitInEra era of
        Left implicit ->
            fee == TxFeeImplicit implicit
            & label ("fee in " <> show era <> " is always implicit")
            & counterexample ( "fee in era "
                              <> show era
                              <> " wasn't implicit but should be"
                             )
        Right _ ->
            case fee of
                TxFeeImplicit _ ->
                    error "fees are explicit in era but received implicit fee"
                TxFeeExplicit _ l ->
                    genLovelaceCoverage l

genTtlCoverage :: SlotNo -> Property
genTtlCoverage = genSlotNoCoverage

genTxValidityLowerBoundCoverage
    :: CardanoEra era -> TxValidityLowerBound era -> Property
genTxValidityLowerBoundCoverage era validFrom =
    case validityLowerBoundSupportedInEra era of
        Nothing ->
            validFrom == TxValidityNoLowerBound
            & label ("validity lower bound not supported in " <> show era)
            & counterexample ( "validity lower bound shouldn't be supported in "
                              <> show era
                             )
        Just _ ->
            case validFrom of
                TxValidityNoLowerBound ->
                    False
                    & counterexample ( "validity lower bound supported in "
                                       <> show era
                                       <> ", should have lower bound")
                TxValidityLowerBound _ ttl ->
                    genTtlCoverage ttl

genTxValidityUpperBoundCoverage
    :: CardanoEra era -> TxValidityUpperBound era -> Property
genTxValidityUpperBoundCoverage era validFrom =
    case validityUpperBoundSupportedInEra era of
        Nothing ->
            case validFrom of
                (TxValidityNoUpperBound _) ->
                    True
                    & label ( "validity upper bound not supported in "
                              <> show era
                            )
                    & counterexample
                        ( "validity upper bound shouldn't be supported in "
                          <> show era
                        )
                (TxValidityUpperBound _ _) ->
                    error ( "validity upper bound not supported in "
                           <> show era
                           <> ", no upper bound should be generated."
                          )
        Just _ ->
            case validFrom of
                TxValidityNoUpperBound _ ->
                    False
                    & counterexample ( "validity upper bound supported in "
                                       <> show era
                                       <> ", should have upper bound")
                TxValidityUpperBound _ ttl ->
                    genTtlCoverage ttl

genTxValidityRangeCoverage
    :: CardanoEra era
    -> (TxValidityLowerBound era, TxValidityUpperBound era)
    -> Property
genTxValidityRangeCoverage era (lower, upper) = checkCoverage $ conjoin
    [ genTxValidityLowerBoundCoverage era lower
    , genTxValidityUpperBoundCoverage era upper
    ]

genScriptValidityCoverage :: ScriptValidity -> Property
genScriptValidityCoverage scriptValidity = checkCoverage
    $ cover 40 (scriptValidity == ScriptInvalid)
        "script is invalid"
    $ cover 40 (scriptValidity == ScriptValid)
        "script is valid"
        True

instance Arbitrary ScriptValidity where
    arbitrary = genScriptValidity

genTxScriptValidityCoverage :: CardanoEra era -> TxScriptValidity era -> Property
genTxScriptValidityCoverage era scriptValidity =
    case txScriptValiditySupportedInCardanoEra era of
        Nothing ->
            scriptValidity == TxScriptValidityNone
            & label "script validity is always none in eras it is not supported"
            & counterexample
                ("script validity was generated in unsupported " <> show era)
        Just _  ->
            case scriptValidity of
                TxScriptValidityNone ->
                    False
                    & counterexample
                        (show era <> "era should have script validity")
                TxScriptValidity _ validity ->
                    genScriptValidityCoverage validity

genExtraKeyWitnessesCoverage
    :: CardanoEra era -> TxExtraKeyWitnesses era -> Property
genExtraKeyWitnessesCoverage era ws =
    case extraKeyWitnessesSupportedInEra era of
        Nothing ->
            label "extra key witnesses are not generated in unsupported eras"
                (noWitnesses ws)
            & counterexample ( "key witnesses were generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ cover 10 (noWitnesses ws) "no witnesses"
            $ cover 10 (witnesses ws) "witnesses"
            $ case ws of
                TxExtraKeyWitnessesNone -> property True
                (TxExtraKeyWitnesses _ wits) -> checkCoverage
                    $ cover 1 (null wits) "empty witneses"
                    $ cover 30 (not (null wits)) "some witnesses"
                    $ cover 10 (length wits > 3) "> 3 witnesses" True

    where
        noWitnesses = (== TxExtraKeyWitnessesNone)

        witnesses = \case
            TxExtraKeyWitnessesNone -> False
            TxExtraKeyWitnesses _ _ -> True

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
        True

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
        True

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
    (RequireAllOf _)        -> True
    (RequireAnyOf ss)       -> any requiresAllOf ss
    (RequireMOf _ ss)       -> any requiresAllOf ss

requiresAnyOf :: forall lang. SimpleScript lang -> Bool
requiresAnyOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresAnyOf ss
    (RequireAnyOf _)        -> True
    (RequireMOf _ ss)       -> any requiresAnyOf ss

requiresMOf :: forall lang. SimpleScript lang -> Bool
requiresMOf = \case
    (RequireSignature _)    -> False
    (RequireTimeBefore _ _) -> False
    (RequireTimeAfter _ _)  -> False
    (RequireAllOf ss)       -> any requiresMOf ss
    (RequireAnyOf ss)       -> any requiresMOf ss
    (RequireMOf _ _)        -> True

genAssetNameCoverage :: AssetName -> Property
genAssetNameCoverage n = checkCoverage
    $ cover 1 (assetNameLen == 0)
        "asset name is empty"
    $ cover 0.1 (assetNameLen == shortLength)
        "asset name is short"
    $ cover 5 (assetNameLen == longLength)
        "asset name is long"
    $ cover 5 (assetNameLen > shortLength && assetNameLen < longLength)
        "asset name is between short and long"
    $ label "is alphanumeric" (all isAlphaNum assetNameStr)
      & counterexample "character wasn't alphabetic or numeric"
    where
        assetNameStr = (\(AssetName n') -> B8.unpack n') n
        assetNameLen = (\(AssetName n') -> BS.length n') n
        shortLength = 1
        longLength = 32

instance Arbitrary AssetName where
    arbitrary = genAssetName

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

genSignedQuantityCoverage :: Quantity -> Property
genSignedQuantityCoverage = signedCoverage "quantity"

instance Arbitrary Quantity where
    arbitrary = genSignedQuantity

genTxMintValueCoverage :: CardanoEra era -> TxMintValue build era -> Property
genTxMintValueCoverage era val =
    case multiAssetSupportedInEra era of
        Left _ ->
            label "mint values are not generated in unsupported eras"
                (val == TxMintNone)
            & counterexample ( "a mint value was generated in an unsupported "
                              <> show era
                             )
        Right _ ->
            checkCoverage
            $ cover 40 (noMint val)
              "no mint"
            $ cover 40 (someMint val)
              "mint"
              True

    where
        noMint = (== TxMintNone)

        someMint = \case
            TxMintNone -> False
            TxMintValue {} -> True

genNetworkMagicCoverage :: NetworkMagic -> Property
genNetworkMagicCoverage (NetworkMagic n) =
    unsignedCoverage (maxBound :: Int32) "network magic" n

instance Arbitrary NetworkMagic where
    arbitrary = genNetworkMagic

genNetworkIdCoverage :: NetworkId -> Property
genNetworkIdCoverage n = checkCoverage
    $ cover 10 (isMainnet n)
        "network is mainnet"
    $ cover 1 (isTestnet n)
        "network is testnet"
        True
    where
        isMainnet = (== Mainnet)

        isTestnet = \case
            Mainnet   -> False
            Testnet _ -> True

instance Arbitrary NetworkId where
    arbitrary = genNetworkId

genStakeCredentialCoverage :: StakeCredential -> Property
genStakeCredentialCoverage sc = checkCoverage
    $ cover 10 (isByKey sc)
        "stake credential built from key"
    $ cover 10 (isByScript sc)
        "stake credential built from script"
        True

    where
        isByKey = \case
            StakeCredentialByKey _    -> True
            StakeCredentialByScript _ -> False

        isByScript = \case
            StakeCredentialByKey _    -> False
            StakeCredentialByScript _ -> True

instance Arbitrary StakeCredential where
    arbitrary = genStakeCredential

-- genScriptDataNumberCoverage :: Integer -> Property
-- genScriptDataNumberCoverage n = checkCoverage
--     $ cover 1 (n == 0)
--         "number is equal to 0"
--     $ cover 2 (n >= veryLargeNumber)
--         "number is very large"
--     $ cover 2 (n <= verySmallNumber)
--         "number is very small"
--     $ cover 10 (n > verySmallNumber && n < veryLargeNumber)
--         "number is between very small and very large"
--         True

--     where
--         verySmallNumber = fromIntegral (minBound :: Int32)
--         veryLargeNumber = fromIntegral (maxBound :: Int32)

-- genScriptDataBytesCoverage :: ByteString -> Property
-- genScriptDataBytesCoverage bs = checkCoverage
--     $ cover 1 (BS.length bs == 0)
--         "no bytes"
--     $ cover 10 (BS.length bs > 0)
--         "some bytes"
--     $ cover 2 (BS.length bs > 32)
--         "lots of bytes"
--         True

-- genScriptDataListCoverage :: [ScriptData] -> Property
-- genScriptDataListCoverage ss = checkCoverage
--     $ cover 1 (null ss)
--         "no scripts in list"
--     $ cover 10 (not $ null ss)
--         "some scripts in list"
--     $ cover 10 (length ss > 32)
--         "lots of scripts in list"
--     $ conjoin
--     $ fmap genScriptDataCoverage ss

-- genScriptDataMapCoverage :: [(ScriptData, ScriptData)] -> Property
-- genScriptDataMapCoverage ss = checkCoverage
--     $ cover 1 (null ss)
--         "no scripts in map"
--     $ cover 10 (not $ null ss)
--         "some scripts in map"
--     $ cover 10 (length ss > 32)
--         "lots of scripts in map"
--     $ conjoin
--     $ fmap (\(k, v) ->
--                 conjoin [genScriptDataCoverage k, genScriptDataCoverage v]
--            ) ss

-- genScriptDataConstructorCoverage :: (Integer, [ScriptData]) -> Property
-- genScriptDataConstructorCoverage (ix, ss) = checkCoverage
--     $ cover 1 (null ss)
--         "no scripts in constr"
--     $ cover 10 (not $ null ss)
--         "some scripts in constr"
--     $ cover 10 (length ss > 3)
--         "lots of scripts in constr"
--     $ cover 1 (ix == 0)
--         "ix == 0"
--     $ cover 10 (ix > 0)
--         "ix > 0"
--     $ cover 10 (ix > 3)
--         "ix > 3"
--         True

-- genScriptDataCoverage :: ScriptData -> Property
-- genScriptDataCoverage dat = checkCoverage $ conjoin
--     [ checkCoverage
--       $ case dat of
--           ScriptDataNumber _        -> cover 5 True "is script data number"
--           ScriptDataBytes _         -> cover 5 True "is script data bytes"
--           ScriptDataList _          -> cover 5 True "is script data list"
--           ScriptDataMap _           -> cover 5 True "is script data map"
--           ScriptDataConstructor _ _ -> cover 5 True "is script data constructor"
--       $ True
--     , case dat of
--           ScriptDataNumber n        ->
--               genScriptDataNumberCoverage n
--           ScriptDataBytes bs        ->
--               genScriptDataBytesCoverage bs
--           ScriptDataList ss          ->
--               genScriptDataListCoverage ss
--           ScriptDataMap ss           ->
--               genScriptDataMapCoverage ss
--           ScriptDataConstructor n ss ->
--               genScriptDataConstructorCoverage (n, ss)
--     ]

-- instance Arbitrary ScriptData where
--     arbitrary = genScriptData

genExecutionUnitsCoverage :: ExecutionUnits -> Property
genExecutionUnitsCoverage (ExecutionUnits steps mem) = checkCoverage $ conjoin
    [ unsignedCoverage (maxBound @Word32) "execution steps" steps
    , unsignedCoverage (maxBound @Word32) "execution mem" mem
    ]

instance Arbitrary ExecutionUnits where
    arbitrary = genExecutionUnits

genWitnessStakeCoverage
    :: CardanoEra era
    -> Witness WitCtxStake era
    -> Property
genWitnessStakeCoverage era wit =
    let
        supportedLangs = [ ()
                         | AnyScriptLanguage lang <- [minBound..maxBound]
                         , Just _langInEra <-
                               [scriptLanguageSupportedInEra era lang]
                         ]
    in
        if null supportedLangs
        then
            wit == KeyWitness KeyWitnessForStakeAddr
            & label ("only key witnesses are generated in " <> show era)
            & counterexample ( "a script witness was generated in "
                              <> show era
                              <> " but only key witnesses are supported in this"
                              <> " era."
                             )
        else
            checkCoverage
            $ cover 20 (wit == KeyWitness KeyWitnessForStakeAddr)
                "key witnesses in era"
            $ cover 20 (isScriptWitness wit)
                "script witnesses in era"
                True

    where
        isScriptWitness = \case
            (KeyWitness _)      -> False
            (ScriptWitness _ _) -> True

genTxWithdrawalsCoverage
    :: CardanoEra era -> TxWithdrawals build era -> Property
genTxWithdrawalsCoverage era ws =
    case withdrawalsSupportedInEra era of
        Nothing ->
            ws == TxWithdrawalsNone
            & label "withdrawals not generated in unsupported eras"
            & counterexample ( "withdrawals were generated in unsupported era "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case ws of
                TxWithdrawalsNone -> cover 10 True "no withdrawals" True
                TxWithdrawals _ xs ->
                    cover 1 (null xs) "empty withdrawals"
                    $ cover 10 (not $ null xs) "some withdrawals"
                    $ cover 10 (length xs > 3) "more withdrawals"
                      True

genTxAuxScriptsCoverage :: CardanoEra era -> TxAuxScripts era -> Property
genTxAuxScriptsCoverage era aux =
    case auxScriptsSupportedInEra era of
        Nothing ->
            aux == TxAuxScriptsNone
            & label "aux scripts not generated in unsupported era"
            & counterexample ( "aux scripts were generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case aux of
                TxAuxScriptsNone  -> cover 10 True "no aux scripts" True
                TxAuxScripts _ ss ->
                    cover 1 (null ss) "empty aux scripts"
                    $ cover 50 (not $ null ss) "non-empty aux scripts"
                    $ cover 30 (length ss > 3) "some aux scripts"
                      True

genTxMetadataValueCoverage :: TxMetadataValue -> Property
genTxMetadataValueCoverage meta =
    checkCoverage
        $ cover 8 (isMetaNumber meta) "is TxMetaNumber"
        $ cover 8 (isMetaBytes meta) "is TxMetaBytes"
        $ cover 8 (isMetaText meta) "is TxMetaText"
        $ cover 8 (isMetaList meta) "is TxMetaList"
        $ cover 8 (isMetaMap meta) "is TxMetaMap" True

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
            $ cover 1 (null metaMap)
              "no metadata entries"
            $ cover 10 (not $ null metaMap)
              "some metadata entries"
            $ cover 10 (length metaMap > 10)
              "lots of metadata entries"
            $ conjoin $ fmap (metaNumberCoverage . fst) metaMap

    where
        metaNumberCoverage n = checkCoverage
            $ cover 0.1 (n == 0)
              "meta index == 0"
            $ cover 10 (n > 0)
              "meta index > 0"
            $ cover 10 (n >= veryLargeMetaIndex)
              "meta index is large"
            $ cover 10 (n > 0 && n < veryLargeMetaIndex)
              "meta index is between smallest and large"
            $ property (n >= 0)
              & counterexample "meta index was negative"

        veryLargeMetaIndex = fromIntegral (maxBound @Word32)

instance Arbitrary TxMetadata where
    arbitrary = genTxMetadata

genTxMetadataInEraCoverage :: CardanoEra era -> TxMetadataInEra era -> Property
genTxMetadataInEraCoverage era meta =
    case txMetadataSupportedInEra era of
        Nothing ->
            meta == TxMetadataNone
            & label "metadata not generated in unsupported era"
            & counterexample ( "metadata was generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ case meta of
                TxMetadataNone -> cover 10 True "no metadata" True
                TxMetadataInEra _ _ -> cover 40 True "some metadata" True

genPtrCoverage :: Ptr -> Property
genPtrCoverage (safeUnwrapPtr -> (slotNo, txIx, certIx)) =
    checkCoverage $ conjoin
        [ genSlotNo32Coverage slotNo
        , genTxIxCoverage' txIx
        , genCertIxCoverage certIx
        ]

prop_safePtr_safeUnwrapPtr
    :: SlotNo32 -> Ledger.TxIx -> Ledger.CertIx -> Property
prop_safePtr_safeUnwrapPtr s t c =
    safeUnwrapPtr (safePtr s t c) === (s, t, c)

instance Arbitrary Ptr where
    arbitrary = genPtr

instance Arbitrary SlotNo32 where
    arbitrary = genSlotNo32

genStakeAddressReferenceCoverage :: StakeAddressReference -> Property
genStakeAddressReferenceCoverage ref = checkCoverage
    $ cover 10 (byValue ref)
        "stake address reference created by value"
    $ cover 10 (byPointer ref)
        "stake address reference created by pointer"
    $ cover 10 (noStakeAddress ref)
        "no stake address"
        True
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

genPaymentCredentialCoverage :: PaymentCredential -> Property
genPaymentCredentialCoverage paymentCred = checkCoverage
    $ cover 20 (isByKey paymentCred)
        "payment credential is provided by key"
    $ cover 20 (isByScript paymentCred)
        "payment credential is provided by script"
        True

    where
        isByKey = \case
            (PaymentCredentialByKey _) -> True
            (PaymentCredentialByScript _) -> False

        isByScript = \case
            (PaymentCredentialByKey _) -> False
            (PaymentCredentialByScript _) -> True

instance Arbitrary PaymentCredential where
    arbitrary = genPaymentCredential

genAddressInEraCoverage
    :: CardanoEra era
    -> AddressInEra era
    -> Property
genAddressInEraCoverage era addr =
    case cardanoEraStyle era of
        LegacyByronEra ->
            if isByronAddress addr
            then True
                 & label "in Byron era, always generate byron addresses"
            else False
                 & counterexample "Non-Byron address was generated in Byron era"
        ShelleyBasedEra _era -> checkCoverage
            $ cover 10 (isByronAddress addr)
                "byron address"
            $ cover 10 (isShelleyAddress addr)
                "shelley address"
                True

    where
        isByronAddress = \case
            AddressInEra ByronAddressInAnyEra _addr -> True
            _ -> False
        isShelleyAddress = \case
            AddressInEra (ShelleyAddressInEra _era) _addr -> True
            _ -> False

genUnsignedQuantityCoverage :: Quantity -> Property
genUnsignedQuantityCoverage =
    unsignedCoverage (maxBound @Word32) "unsigned quantity"

genValueForTxOutCoverage :: Value -> Property
genValueForTxOutCoverage val =
    let
        valList = map fst $ valueToList val -- includes ada
        otherAssetList = valList \\ [AdaAssetId] -- doesn't include ada
    in
        checkCoverage
        $ cover 0.5 (null valList)
            "Value has no assets nor ada"
        $ cover 10 (null otherAssetList)
            "Value has no assets other than ada"
        $ cover 10 (not $ null valList)
            "Value has some assets"
        $ cover 10 (length valList >= 3)
            "Value has more assets"
            True

genTxOutDatumCoverage
    :: CardanoEra era -> TxOutDatum ctx era -> Property
genTxOutDatumCoverage era datum =
    case scriptDataSupportedInEra era of
        Nothing ->
            (datum == TxOutDatumNone)
            & label "tx out datums not generated in unsupported era"
            & counterexample ( "tx out datums were generated in unsupported "
                               <> show era
                             )
        Just _ -> checkCoverage
            $ cover 30 (hasNoDatumHash datum)
                "no tx out datum hash"
            $ cover 30 (hasDatumHash datum)
                "tx out datum hash present"
                True
    where
        hasNoDatumHash = (== TxOutDatumNone)

        hasDatumHash = \case
            TxOutDatumHash _ _ -> True
            _ -> False

genTxOutReferenceScriptCoverage
    :: CardanoEra era -> ReferenceScript era -> Property
genTxOutReferenceScriptCoverage era refScript =
    case refInsScriptsAndInlineDatsSupportedInEra era of
        Nothing ->
            (refScript == ReferenceScriptNone)
            & label "reference scripts not generated in unsupported era"
            & counterexample
                ( "reference scripts were generated in unsupported "
                    <> show era
                )
        Just _ -> checkCoverage
            $ cover 30 (hasNoRefScript refScript)
                "no reference script"
            $ cover 30 (hasRefScript refScript)
                "reference script present"
                True
  where
    hasNoRefScript = (== ReferenceScriptNone)

    hasRefScript = \case
        ReferenceScript _ _ -> True
        _ -> False

genTxOutValueCoverage :: CardanoEra era -> TxOutValue era -> Property
genTxOutValueCoverage era val =
    case multiAssetSupportedInEra era of
        Left _ ->
            case val of
                (TxOutAdaOnly _ l) ->
                    genLovelaceCoverage l
                    & label ("ADA only supported in " <> show era)
                _                  ->
                    property False
                    & counterexample (show era <> " should only support ADA")
        Right _ ->
            case val of
                (TxOutValue _ value) ->
                    genValueForTxOutCoverage value
                    & label ("Multi-asset supported in " <> show era)
                _ ->
                    property False
                    & counterexample (show era <> " should support multi-asset")

genTxOutCoverage :: CardanoEra era -> TxOut ctx era -> Property
genTxOutCoverage era (TxOut addr val datum refScript) = checkCoverage $ conjoin
    [ genAddressInEraCoverage era addr
    , genTxOutValueCoverage era val
    , genTxOutDatumCoverage era datum
    , genTxOutReferenceScriptCoverage era refScript
    ]

genWitnessNetworkIdOrByronAddressCoverage
    :: WitnessNetworkIdOrByronAddress -> Property
genWitnessNetworkIdOrByronAddressCoverage witNetworkOrByron = checkCoverage
    $ cover 30 (isNetworkIdWit witNetworkOrByron)
        "is network id witness"
    $ cover 30 (isByronAddrWit witNetworkOrByron)
        "is byron address witness"
        True

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

genByronKeyWitnessCoverage :: KeyWitness ByronEra -> Property
genByronKeyWitnessCoverage (ByronKeyWitness wit) =
    if isVKWitness wit
    then True
         & label "Key witness in Byron era is VKWitness"
    else False
         & counterexample "Key witness in Byron era MUST be a VKWitness"

    where
        isVKWitness = \case
            VKWitness _ _     -> True
            RedeemWitness _ _ -> False
genByronKeyWitnessCoverage wit =
    error $ "expected ByronKeyWitness, got: " <> show wit

genShelleyWitnessSigningKeyCoverage :: ShelleyWitnessSigningKey -> Property
genShelleyWitnessSigningKeyCoverage sk = checkCoverage
    $ case sk of
          WitnessPaymentKey _ ->
              cover 5 True "is witness payment key"
          WitnessPaymentExtendedKey _ ->
              cover 5 True "is witness payment extended key"
          WitnessStakeKey _ ->
              cover 5 True "is witness stake key"
          WitnessStakeExtendedKey _ ->
              cover 5 True "is witness stake extended key"
          WitnessStakePoolKey _ ->
              cover 5 True "is witness stake pool key"
          WitnessGenesisKey _ ->
              cover 5 True "is witness genesis key"
          WitnessGenesisExtendedKey _ ->
              cover 5 True "is witness genesis extended key"
          WitnessGenesisDelegateKey _ ->
              cover 5 True "is witness genesis delegate key"
          WitnessGenesisDelegateExtendedKey _ ->
              cover 5 True "is witness genesis delegate extended key"
          WitnessGenesisUTxOKey _ ->
              cover 5 True "is witness genesis utxo key"
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

genWitnessesCoverage :: CardanoEra era -> [KeyWitness era] -> Property
genWitnessesCoverage era wits = checkCoverage
    $ case cardanoEraStyle era of
          LegacyByronEra -> conjoin
              [ conjoin $ genByronKeyWitnessCoverage <$> wits
              ,  not (null wits)
                 & label "witness list is not empty"
                 & counterexample "genWitnesses generated empty witness list"
              ]
          ShelleyBasedEra _ -> property True

-- genTxBodyContentCoverage
--     :: CardanoEra era -> TxBodyContent build era -> Property
-- genTxBodyContentCoverage era txBody =
--     let
--         ins = (\(txIn, _) -> txIn) <$> Api.txIns txBody
--         outs = Api.txOuts txBody
--     in
--         checkCoverage $ conjoin $ genTxOutCoverage era <$> outs
--         -- checkCoverage $ conjoin
--         --     [ not (null ins)
--         --       & label "always have at least one tx in"
--         --       & counterexample "TxBodyContent generated without a tx in!"
--         --     , conjoin $ genTxInCoverage <$> ins
--         --     , genTxInCollateralCoverage era $ Api.txInsCollateral txBody
--         --     , not (null outs)
--         --       & label "always have at least one tx out"
--         --       & counterexample "TxBodyContent generated without a tx out!"

--         --     -- , conjoin $ genTxOutCoverage era <$> outs
--         --     -- , genTxFeeCoverage era $ Api.txFee txBody
--         --     -- , genTxValidityRangeCoverage era $ Api.txValidityRange txBody

--         --     , genTxMetadataInEraCoverage era $ Api.txMetadata txBody
--         --     , genTxAuxScriptsCoverage era $ Api.txAuxScripts txBody
--         --     -- TODO: Protocol params coverage
--         --     -- , genProtocolParamsCoverage era $ Api.protocolParams txBody
--         --     , genTxWithdrawalsCoverage era $ Api.txWithdrawals txBody
--         --     -- TODO: Certificates coverage
--         --     -- , genTxCertificatesCoverage era $ Api.txCertificates txBody
--         --     -- TODO: Update proposal coverage
--         --     -- , genTxUpdateProposalCoverage era $ Api.txUpdateProposal txBody
--         --     , genTxMintValueCoverage era $ Api.txMintValue txBody
--         --     , genTxScriptValidityCoverage era $ Api.txScriptValidity txBody
--         --     -- TODO: Extra script data coverage
--         --     -- , genExtraScriptDataCoverage era $ Api.txExtraScriptData txBody
--         --     , genExtraKeyWitnessesCoverage era $ Api.txExtraKeyWits txBody
--         --     ]

-- instance Show (TxBodyContent build era) where
--     show txBody =
--         show $ "TxBody "
--                <> "( " <> show (Api.txIns txBody) <> " ) "
--                <> "( " <> show (Api.txInsCollateral txBody) <> " ) "
--                <> "( " <> show (Api.txOuts txBody) <> " ) "
--                <> "( " <> show (Api.txFee txBody) <> " ) "
--                <> "( " <> show (Api.txValidityRange txBody) <> " ) "
--                <> "( " <> show (Api.txMetadata txBody) <> " ) "
--                <> "( " <> show (Api.txAuxScripts txBody) <> " ) "
--                <> "( " <> show (Api.txProtocolParams txBody) <> " ) "
--                <> "( " <> show (Api.txWithdrawals txBody) <> " ) "
--                <> "( " <> show (Api.txCertificates txBody) <> " ) "
--                <> "( " <> show (Api.txUpdateProposal txBody) <> " ) "
--                <> "( " <> show (Api.txMintValue txBody) <> " ) "
--                <> "( " <> show (Api.txScriptValidity txBody) <> " ) "
--                <> "( " <> show (Api.txExtraScriptData txBody) <> " ) "
--                <> "( " <> show (Api.txExtraKeyWits txBody) <> " ) "

-- genTxCoverage
--     :: CardanoEra era -> Tx era -> Property
-- genTxCoverage era (Tx (TxBody body) wits) = checkCoverage $ conjoin
--     [ -- genTxBodyContentCoverage era body
--       genWitnessesCoverage era wits
--     ]

genNatCoverage :: Natural -> Property
genNatCoverage = unsignedCoverage (maxBound @Word32) "natural"

instance Arbitrary Natural where
    arbitrary = genNat

genRationalCoverage :: Rational -> Property
genRationalCoverage = rationalCoverage (maxBound @Word32)

genRationalInt64Coverage :: Rational -> Property
genRationalInt64Coverage = rationalCoverage (maxBound @Int32)

genEpochNoCoverage :: EpochNo -> Property
genEpochNoCoverage = unsignedCoverage (maxBound @Word32) "epoch num"

instance Arbitrary EpochNo where
    arbitrary = genEpochNo

genCostModelCoverage :: CostModel -> Property
genCostModelCoverage (CostModel costModel) = checkCoverage $ conjoin
    [ Map.size costModel == Map.size (Ledger.getCostModelParams Plutus.testingCostModelV1)
      & label "Generated cost model must have same size as default cost model"
      & counterexample "Generated cost model did not have same size as default cost model"
    , checkCoverage
        $ cover 1 (elem 0 $ Map.elems costModel)
            "model param is zero"
        $ cover 30 (any (> 0) $ Map.elems costModel)
            "model param is greater than zero"
            True
    ]

instance Arbitrary CostModel where
    arbitrary = genCostModel

genCostModelsCoverage :: Map AnyPlutusScriptVersion CostModel -> Property
genCostModelsCoverage m = checkCoverage $ conjoin $
    [ cover 5 (Map.size m == 0)
          "cost models size == 0"
      $ cover 30 (Map.size m > 0)
          "cost models size > 0"
          True
    ] <> (genCostModelCoverage <$> (Map.elems m))

genExecutionUnitPricesCoverage :: ExecutionUnitPrices -> Property
genExecutionUnitPricesCoverage (ExecutionUnitPrices p1 p2) =
    checkCoverage $ conjoin
        [ genRationalCoverage p1
        , genRationalCoverage p2
        ]

instance Arbitrary ExecutionUnitPrices where
    arbitrary = genExecutionUnitPrices

-- genProtocolParametersCoverage :: ProtocolParameters -> Property
-- genProtocolParametersCoverage pparams = checkCoverage $ conjoin
--     [ genNatCoverage . fst . Api.protocolParamProtocolVersion $ pparams
--     , genNatCoverage . snd . Api.protocolParamProtocolVersion $ pparams
--     , genRationalCoverage . Api.protocolParamDecentralization $ pparams
--     ]

-- instance Arbitrary ProtocolParameters where
--     arbitrary = genProtocolParameters

genMIRPotCoverage :: MIRPot -> Property
genMIRPotCoverage pot = checkCoverage
    $ True
      & cover 30 (pot == ReservesMIR) "is reserves pot"
      & cover 30 (pot == TreasuryMIR) "is treasury pot"

instance Arbitrary MIRPot where
    arbitrary = genMIRPot

genMIRTargetCoverage :: MIRTarget -> Property
genMIRTargetCoverage target = checkCoverage
    $ case target of
        StakeAddressesMIR ss ->
            True
            & cover 25 True "is stake address MIR target"
            & cover 1 (null ss) "no stake addresses"
            & cover 10 (not $ null ss) "some stake addresses"
        SendToReservesMIR _ ->
            cover 25 True "is reserves MIR target" True
        SendToTreasuryMIR _ ->
            cover 25 True "is treasury MIR target" True
        _ ->
            error "uncovered case"

instance Arbitrary MIRTarget where
    arbitrary = genMIRTarget

genTxCertificateCoverage :: Certificate -> Property
genTxCertificateCoverage cert = checkCoverage
    $ case cert of
        StakeAddressRegistrationCertificate _ ->
            cover 10 True "is stake address registration cert" True
        StakeAddressDeregistrationCertificate _ ->
            cover 10 True "is stake address deregistration cert" True
        StakeAddressDelegationCertificate _ _ ->
            cover 10 True "is stake address delegation cert" True
        StakePoolRegistrationCertificate _ ->
            cover 10 True "is stake pool registration cert" True
        StakePoolRetirementCertificate _ _ ->
            cover 10 True "is stake pool retirement cert" True
        GenesisKeyDelegationCertificate {} ->
            cover 10 True "is genesis key delegation cert" True
        MIRCertificate _ _ ->
            cover 10 True "is MIR cert" True
        _ ->
            error "uncovered case"

instance Arbitrary Certificate where
    arbitrary = genTxCertificate

genTxCertificatesCoverage
    :: CardanoEra era -> TxCertificates BuildTx era -> Property
genTxCertificatesCoverage era certs = checkCoverage
    $ case certificatesSupportedInEra era of
        Nothing ->
            certs == TxCertificatesNone
            & label "tx certificates aren't generated in unsupported era"
            & counterexample ( "tx certificate was generated in unsupported "
                               <> show era
                             )
        Just _ ->
            case certs of
                TxCertificatesNone ->
                    cover 5 True "no tx certificates" True
                TxCertificates _ cs (BuildTxWith m) ->
                    True
                    & cover 30 True
                        "some tx certificates"
                    & cover 0.4 (null cs)
                        "empty tx certificates"
                    & cover 0.4 (null m)
                        "empty stake credential/witnesses map"
                    & cover 10 (not $ null cs)
                        "some tx certificates"
                    & cover 10 (not $ null m)
                        "non-empty stake credential/witnesses map"
                _ ->
                    error "uncovered case"

genUpdateProposalCoverage
    :: CardanoEra era -> TxUpdateProposal era -> Property
genUpdateProposalCoverage era proposal = checkCoverage
    $ case updateProposalSupportedInEra era of
        Nothing ->
            proposal == TxUpdateProposalNone
            & label "tx update proposal aren't generated in unsupported era"
            & counterexample ( "tx update proposal was generated in unsupported "
                               <> show era
                             )
        Just _ ->
            case proposal of
                TxUpdateProposalNone ->
                    cover 5 True "no update proposal" True
                TxUpdateProposal _ (UpdateProposal m _epoch) ->
                    True
                    & cover 30 True
                        "update proposal"
                    & cover 0.4 (null m)
                        "empty protocol updates"
                    & cover 10 (not $ null m)
                        "non-empty protocol updates"
                _ ->
                    error "uncovered case"

rationalCoverage
    :: Integral b
    => b
    -> Rational
    -> Property
rationalCoverage b x = checkCoverage $ conjoin
    [ numerator x == 1
      & label "rational numerator is always 1"
      & counterexample "rational numerator must always be 1"
    , denominator x /= 0
      & label "rational denominator never zero"
      & counterexample "rational denominator was zero - divide by zero"
    , denominator x >= 1
      & label "rational denominator is non-negative"
      & counterexample "rational denominator was negative"
    , checkCoverage
        $ cover 2 (denominator x == 1)
            "rational denominator is 1"
        $ cover 30 (denominator x > 1 && denominator x < veryLarge)
            "rational denominator is between 1 and very large"
        $ cover 5 (denominator x > veryLarge)
            "rational denominator is very large"
            True
    ]

    where
        veryLarge = fromIntegral b

-- | Provide coverage for an unsigned number.
unsignedCoverage
    :: ( Num a
       , Ord a
       , Integral b
       )
    => b
    -- ^ Numbers greater than this maxBound are considered "large"
    -> String
    -- ^ The name of the entity we are providing coverage for
    -> a
    -- ^ The value of the entity
    -> Property
unsignedCoverage b name x = checkCoverage
    $ cover 1 (x == 0)
        (name <> " is zero")
    $ cover 30 (x > 0 && x < veryLarge)
        (name <> " is between zero and very large")
    $ cover 1 (x > veryLarge)
        (name <> " is greater than very large")
    $ label (name <> " is non-negative") (x >= 0)
      & counterexample (name <> " was negative")

    where
        veryLarge = fromIntegral b

signedCoverage
  :: ( Num a
     , Ord a
     )
  => String
  -> a
  -> Property
signedCoverage name x = checkCoverage
    $ cover 1 (x == 0)
        (name <> " is zero")
    $ cover 30 (x > verySmall && x < veryLarge)
        (name <> " is between very small and very large")
    $ cover 5 (x > veryLarge)
        (name <> " is greater than very large")
    $ cover 5 (x < verySmall)
        (name <> " is less than very small")
        True

    where
        veryLarge = fromIntegral (maxBound @Word32)
        verySmall = -fromIntegral (maxBound @Word32)

forAllEras :: Applicative f => (AnyCardanoEra -> f a) -> f ()
forAllEras f =
    f (AnyCardanoEra ByronEra)
    *> forAllShelleyBasedEras f

forAllShelleyBasedEras :: Applicative f => (AnyCardanoEra -> f a) -> f ()
forAllShelleyBasedEras f =
    traverse_ f [ AnyCardanoEra ShelleyEra
                , AnyCardanoEra AllegraEra
                , AnyCardanoEra MaryEra
                , AnyCardanoEra AlonzoEra
                , AnyCardanoEra BabbageEra
                ]
