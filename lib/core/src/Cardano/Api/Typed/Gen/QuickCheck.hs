{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Typed.Gen.QuickCheck
  ( genLovelace
  , genNetworkMagic
  , genScriptExecutionUnits
  , genNetworkId
  , genQuantity
  , genAssetName
  , genAlphaNum
  , genSlotNo
  , genEpochNo
  , genPaymentCredential
  , genSimpleScript
  , genAssetId
  , genTxIndex
  , genTtl
  , genWitnessNetworkIdOrByronAddress
  , genTxFee
  , genTxIn
  , genTxInsCollateral
  , genStakeCredential
  , genIx
  , genPtr
  , genStakeAddressReference
  , genAddressInEra
  , genUnsignedQuantity
  , genValueForTxOut
  , genTxOutValue
  , genTxOutDatumHash
  , genTxOut
  , genTxValidityLowerBound
  , genTxValidityUpperBound
  , genTxValidityRange
  , genScriptData
  , genWitnessStake
  , genScriptValidity
  , genTxScriptValidity
  , genShelleyWitnessSigningKey
  , genValueForMinting
  , genTxMintValue
  , genExtraKeyWitnesses

  -- * Coverage doesn't make much sense rn
  , genPolicyId
  , genSeed
  , genSigningKey
  , genVerificationKey
  , genVerificationKeyHash
  , genAddressByron
  , genScript
  , genScriptHash
  , genPlutusScript
  , genShelleyHash
  , genTxId
  , genAddressShelley
  , genStakeAddress
  , genHashScriptData
  ) where

import Prelude

import Cardano.Api hiding
    ( txIns )
import Cardano.Api.Byron
    ( KeyWitness (ByronKeyWitness)
    , Lovelace (Lovelace)
    , WitnessNetworkIdOrByronAddress (..)
    )
import Cardano.Api.Metadata.Gen
    ( genTxMetadata )
import Cardano.Api.Shelley
    ( Hash (ScriptDataHash)
    , KESPeriod (KESPeriod)
    , OperationalCertificateIssueCounter (OperationalCertificateIssueCounter)
    , PlutusScript (PlutusScriptSerialised)
    , ProtocolParameters (ProtocolParameters)
    , StakeCredential (..)
    , StakePoolKey
    )
import Cardano.Ledger.Credential
    ( Ix (..), Ptr (..) )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
import Data.Int
    ( Int64 )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes, maybeToList )
import Data.Ratio
    ( Ratio, (%) )
import Data.String
import Data.Traversable
    ( for )
import Data.Word
    ( Word64, Word8 )
import Hedgehog
    ( Gen, Range )
import Numeric.Natural
    ( Natural )
import Test.Cardano.Chain.UTxO.Gen
    ( genVKWitness )
import Test.Cardano.Crypto.Gen
    ( genProtocolMagicId )
import Test.QuickCheck
    ( Arbitrary (arbitrary), Large (..), NonNegative (..) )
import Test.QuickCheck.Hedgehog
    ( hedgehog )


import qualified Cardano.Api as Api
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Hash.Class as CRYPTO
import qualified Cardano.Crypto.Seed as Crypto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Shelley.Spec.Ledger.TxBody as Ledger
    ( EraIndependentTxBody )
import qualified Test.QuickCheck as QuickCheck

genAddressByron :: QuickCheck.Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: QuickCheck.Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: CardanoEra era -> QuickCheck.Gen (AddressInEra era)
genAddressInEra era =
  case cardanoEraStyle era of
    LegacyByronEra ->
      byronAddressInEra <$> genAddressByron

    ShelleyBasedEra _ ->
      QuickCheck.oneof
        [ byronAddressInEra   <$> genAddressByron
        , shelleyAddressInEra <$> genAddressShelley
        ]

genLovelace :: QuickCheck.Gen Lovelace
genLovelace = do
    (Large (n :: Word64)) <- QuickCheck.arbitrary
    pure $ quantityToLovelace $ Quantity $ toInteger n

-- ----------------------------------------------------------------------------
-- -- SimpleScript generators
-- --

genScript :: ScriptLanguage lang -> QuickCheck.Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

genSimpleScript :: SimpleScriptVersion lang -> QuickCheck.Gen (SimpleScript lang)
genSimpleScript lang =
    QuickCheck.sized genTerm
  where
    genTerm 0 = QuickCheck.oneof nonRecursive
    genTerm n = QuickCheck.frequency
        [ (3, QuickCheck.oneof (recursive n))
        , (1, QuickCheck.oneof nonRecursive)
        ]

    -- Non-recursive generators
    nonRecursive =
        (RequireSignature . verificationKeyHash <$>
            genVerificationKey AsPaymentKey)

      : [ RequireTimeBefore supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

     ++ [ RequireTimeAfter supported <$> genSlotNo
        | supported <- maybeToList (timeLocksSupported lang) ]

    -- Recursive generators
    recursive n =
        [ RequireAllOf <$> (QuickCheck.scale (`mod` 10) $ QuickCheck.listOf $ recurse n)

        , RequireAnyOf <$> (QuickCheck.scale (`mod` 10) $ QuickCheck.listOf $ recurse n)

        , do ts <- QuickCheck.scale (`mod` 10) $ QuickCheck.listOf $ recurse n
             m  <- QuickCheck.choose (0, length ts)
             return (RequireMOf m ts)
        ]

    recurse n = do
        (QuickCheck.Positive m) <- QuickCheck.arbitrary
        genTerm (n `div` (m + 3))

genScriptData :: QuickCheck.Gen ScriptData
genScriptData =
    QuickCheck.sized genTerm

    where
        genTerm 0 = QuickCheck.oneof nonRecursive
        genTerm n = QuickCheck.frequency
            [ (3, QuickCheck.oneof (recursive n))
            , (1, QuickCheck.oneof nonRecursive)
            ]

        -- Non-recursive generators
        nonRecursive =
            [ do
                 (Large (n :: Int64)) <- arbitrary
                 pure $ ScriptDataNumber $ fromIntegral n
            , do
                 (Large (n :: Word8)) <- arbitrary
                 (ScriptDataBytes . BS.pack) <$> QuickCheck.vector (fromIntegral n)
            ]

        -- Recursive generators
        recursive n =
            [ ScriptDataList <$> QuickCheck.listOf (recurse n)
            , ScriptDataMap <$> QuickCheck.listOf ((,) <$> recurse n <*> recurse n)
            , ScriptDataConstructor <$> arbitrary <*> QuickCheck.listOf (recurse n)
            ]

        recurse n = do
            (QuickCheck.Positive m) <- QuickCheck.arbitrary
            genTerm (n `div` (m + 3))

genPlutusScript :: PlutusScriptVersion lang -> QuickCheck.Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> QuickCheck.arbitrary

-- -- ----------------------------------------------------------------------------
-- -- Script generators for any language, or any language valid in a specific era
-- --

genScriptInAnyLang :: QuickCheck.Gen ScriptInAnyLang
genScriptInAnyLang =
    QuickCheck.oneof
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> QuickCheck.Gen (ScriptInEra era)
genScriptInEra era =
    QuickCheck.oneof
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: QuickCheck.Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)


----------------------------------------------------------------------------
-- Multi-asset generators
--
genAssetName :: QuickCheck.Gen AssetName
genAssetName =
  QuickCheck.frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, QuickCheck.elements ["", "a", "b", "c"])
    , (1, AssetName <$> fromString <$> (QuickCheck.vectorOf 32 genAlphaNum))
    , (1, AssetName <$> fromString <$> (
              QuickCheck.scale (\n -> (n `mod` 31) + 1)
                  (QuickCheck.listOf genAlphaNum)
              )
      )
    ]

genAlphaNum :: QuickCheck.Gen Char
genAlphaNum = QuickCheck.elements
    "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genPolicyId :: QuickCheck.Gen PolicyId
genPolicyId =
  QuickCheck.frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, QuickCheck.elements [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: QuickCheck.Gen AssetId
genAssetId = QuickCheck.oneof
    [ AssetId <$> genPolicyId <*> genAssetName
    , return AdaAssetId
    ]

genQuantity :: QuickCheck.Gen Quantity
genQuantity = do
    (Large (n :: Int64)) <- QuickCheck.arbitrary
    pure $ fromIntegral n

-- | Generate a positive or negative quantity.
genSignedQuantity :: QuickCheck.Gen Quantity
genSignedQuantity = genQuantity

genUnsignedQuantity :: QuickCheck.Gen Quantity
genUnsignedQuantity = do
    (Large (n :: Word64)) <- QuickCheck.arbitrary
    pure $ fromIntegral n

genValue :: QuickCheck.Gen AssetId -> QuickCheck.Gen Quantity -> QuickCheck.Gen Value
genValue genAId genQuant =
  valueFromList <$>
    QuickCheck.listOf ((,) <$> genAId <*> genQuant)


-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: QuickCheck.Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedQuantity
  where
    genAssetIdNoAda :: QuickCheck.Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: QuickCheck.Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity

genNetworkId :: QuickCheck.Gen NetworkId
genNetworkId =
    QuickCheck.oneof
        [ pure Mainnet
        , Testnet <$> genNetworkMagic
        ]

genNetworkMagic :: QuickCheck.Gen NetworkMagic
genNetworkMagic = do
    (Large n) <- QuickCheck.arbitrary
    pure $ NetworkMagic n

genPaymentCredential :: QuickCheck.Gen PaymentCredential
genPaymentCredential =
    QuickCheck.oneof
        [ byKey
        , byScript
        ]
    where
        byKey :: QuickCheck.Gen PaymentCredential
        byKey = do
            vKey <- genVerificationKey AsPaymentKey
            return . PaymentCredentialByKey $ verificationKeyHash vKey

        byScript :: QuickCheck.Gen PaymentCredential
        byScript = PaymentCredentialByScript <$> genScriptHash

genSigningKey :: Key keyrole => AsType keyrole -> QuickCheck.Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddressReference :: QuickCheck.Gen StakeAddressReference
genStakeAddressReference =
  QuickCheck.oneof
    [ StakeAddressByValue <$> genStakeCredential
    , (StakeAddressByPointer . StakeAddressPointer) <$> genPtr
    , return NoStakeAddress
    ]

genPtr :: QuickCheck.Gen Ptr
genPtr = Ptr <$> genSlotNo <*> genIx <*> genIx

genIx :: QuickCheck.Gen Ix
genIx = do
    (Large (n :: Word64)) <- QuickCheck.arbitrary
    pure n

genStakeCredential :: QuickCheck.Gen StakeCredential
genStakeCredential =
  QuickCheck.oneof
    [ byKey
    , byScript
    ]

  where
      byKey = do
          vKey <- genVerificationKey AsStakeKey
          return . StakeCredentialByKey $ verificationKeyHash vKey

      byScript = StakeCredentialByScript <$> genScriptHash

genStakeAddress :: QuickCheck.Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

genShelleyHash :: QuickCheck.Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: QuickCheck.Gen SlotNo
genSlotNo = SlotNo <$> QuickCheck.arbitrary

genTxIn :: QuickCheck.Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: QuickCheck.Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: QuickCheck.Gen TxIx
genTxIndex = do
    (Large (n :: Word)) <- QuickCheck.arbitrary
    pure $ TxIx n

genTxOutValue :: CardanoEra era -> QuickCheck.Gen (TxOutValue era)
genTxOutValue era =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> TxOutAdaOnly adaOnlyInEra <$> genLovelace
    Right multiAssetInEra -> TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOut :: CardanoEra era -> QuickCheck.Gen (TxOut era)
genTxOut era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHash era

genTtl :: QuickCheck.Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> QuickCheck.Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> QuickCheck.Gen (TxValidityUpperBound era)
genTxValidityUpperBound era =
  case (validityUpperBoundSupportedInEra era,
       validityNoUpperBoundSupportedInEra era) of
    (Just supported, _) ->
      TxValidityUpperBound supported <$> genTtl

    (Nothing, Just supported) ->
      pure (TxValidityNoUpperBound supported)

    (Nothing, Nothing) ->
      error "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: CardanoEra era
  -> QuickCheck.Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

-- genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
-- genTxMetadataInEra era =
--   case txMetadataSupportedInEra era of
--     Nothing -> pure TxMetadataNone
--     Just supported ->
--       Gen.choice
--         [ pure TxMetadataNone
--         , TxMetadataInEra supported <$> genTxMetadata
--         ]

genTxAuxScripts :: CardanoEra era -> QuickCheck.Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        QuickCheck.scale (`mod` 3) (QuickCheck.listOf (genScriptInEra era))

genTxWithdrawals :: CardanoEra era -> QuickCheck.Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing ->
        pure TxWithdrawalsNone
    Just supported -> do
        QuickCheck.frequency
          [ ( 1 , pure TxWithdrawalsNone )
          , ( 3 , TxWithdrawals supported
                  <$> QuickCheck.listOf (genWithdrawalInfo era) )
          ]

genWithdrawalInfo
    :: CardanoEra era
    -> QuickCheck.Gen ( StakeAddress
                      , Lovelace
                      , BuildTxWith BuildTx (Witness WitCtxStake era)
                      )
genWithdrawalInfo era = do
    stakeAddr <- genStakeAddress
    amt <- genLovelace
    wit <- BuildTxWith <$> genWitnessStake era
    pure (stakeAddr, amt, wit)

genWitnessStake :: CardanoEra era -> QuickCheck.Gen (Witness WitCtxStake era)
genWitnessStake era = QuickCheck.oneof $
    [ pure $ KeyWitness KeyWitnessForStakeAddr ]
    <> forEachLanguage (\lang ->
        case scriptLanguageSupportedInEra era lang of
            Nothing ->
                []
            Just langEra ->
                [ ScriptWitness ScriptWitnessForStakeAddr
                  <$> genScriptWitnessStake langEra
                ]
    )

forEachLanguage
    :: Semigroup a
    => (forall lang. ScriptLanguage lang -> a)
    -> a
forEachLanguage f =
    f (SimpleScriptLanguage SimpleScriptV1)
    <> f (SimpleScriptLanguage SimpleScriptV2)
    <> f (PlutusScriptLanguage PlutusScriptV1)

genScriptWitnessStake
    :: ScriptLanguageInEra lang era
    -> QuickCheck.Gen (ScriptWitness WitCtxStake era)
genScriptWitnessStake langEra =
    case languageOfScriptLanguageInEra langEra of
        (SimpleScriptLanguage ver) ->
            SimpleScriptWitness langEra ver <$> genSimpleScript ver
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScript ver
            <*> pure NoScriptDatumForStake
            <*> genScriptData
            <*> genScriptExecutionUnits

genScriptExecutionUnits :: QuickCheck.Gen ExecutionUnits
genScriptExecutionUnits = do
    (Large executionSteps) <- arbitrary
    (Large executionMemory) <- arbitrary
    pure $ ExecutionUnits executionSteps executionMemory

-- genTxCertificates :: CardanoEra era -> QuickCheck.Gen (TxCertificates BuildTx era)
-- genTxCertificates era =
--   case certificatesSupportedInEra era of
--     Nothing -> pure TxCertificatesNone
--     Just supported ->
--       QuickCheck.oneof
--         [ pure TxCertificatesNone
--         , pure (TxCertificates supported mempty $ BuildTxWith mempty)
--           -- TODO: Generate certificates
--         ]

-- genTxUpdateProposal :: CardanoEra era -> QuickCheck.Gen (TxUpdateProposal era)
-- genTxUpdateProposal era =
--   case updateProposalSupportedInEra era of
--     Nothing -> pure TxUpdateProposalNone
--     Just supported ->
--       QuickCheck.oneof
--         [ pure TxUpdateProposalNone
--         , TxUpdateProposal supported <$> genUpdateProposal
--         ]

genTxMintValue :: CardanoEra era -> QuickCheck.Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      QuickCheck.oneof
        [ pure TxMintNone
        -- TODO gen policy IDs
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]

genExtraKeyWitnesses :: CardanoEra era -> QuickCheck.Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses era =
    case extraKeyWitnessesSupportedInEra era of
        Nothing -> pure TxExtraKeyWitnessesNone
        Just supported  -> QuickCheck.oneof
            [ pure TxExtraKeyWitnessesNone
            , TxExtraKeyWitnesses supported
              <$> QuickCheck.listOf (genVerificationKeyHash AsPaymentKey)
            ]

-- genTxBodyContent :: CardanoEra era -> QuickCheck.Gen (TxBodyContent BuildTx era)
-- genTxBodyContent era = do
--   txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> QuickCheck.listOf1 genTxIn
--   txInsCollateral <- genTxInsCollateral era
--   txOuts <- QuickCheck.listOf (genTxOut era)
--   txFee <- genTxFee era
--   txValidityRange <- genTxValidityRange era
--   txMetadata <- hedgehog $ genTxMetadataInEra era
--   txAuxScripts <- genTxAuxScripts era
--   let txExtraScriptData = BuildTxWith TxExtraScriptDataNone --TODO: Alonzo era: Generate extra script data
--   let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
--   txProtocolParams <- BuildTxWith <$> QuickCheck.liftArbitrary genProtocolParameters
--   txWithdrawals <- genTxWithdrawals era
--   txCertificates <- genTxCertificates era
--   txUpdateProposal <- genTxUpdateProposal era
--   txMintValue <- genTxMintValue era
--   txScriptValidity <- genTxScriptValidity era

--   pure $ TxBodyContent
--     { Api.txIns
--     , Api.txInsCollateral
--     , Api.txOuts
--     , Api.txFee
--     , Api.txValidityRange
--     , Api.txMetadata
--     , Api.txAuxScripts
--     , Api.txExtraScriptData
--     , Api.txExtraKeyWits
--     , Api.txProtocolParams
--     , Api.txWithdrawals
--     , Api.txCertificates
--     , Api.txUpdateProposal
--     , Api.txMintValue
--     , Api.txScriptValidity
--     }

genTxInsCollateral :: CardanoEra era -> QuickCheck.Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> QuickCheck.oneof
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> QuickCheck.listOf genTxIn
                          ]

genTxFee :: CardanoEra era -> QuickCheck.Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> genLovelace

-- genTxBody :: IsCardanoEra era => CardanoEra era -> QuickCheck.Gen (TxBody era)
-- genTxBody era = do
--   res <- makeTransactionBody <$> genTxBodyContent era
--   case res of
--     Left err -> error (displayError err)
--     Right txBody -> pure txBody

genTxScriptValidity :: CardanoEra era -> QuickCheck.Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: QuickCheck.Gen ScriptValidity
genScriptValidity = QuickCheck.elements [ScriptInvalid, ScriptValid]

-- genTx :: forall era. IsCardanoEra era => CardanoEra era -> QuickCheck.Gen (Tx era)
-- genTx era =
--   makeSignedTransaction
--     <$> genWitnesses era
--     <*> genTxBody era

-- genWitnesses :: CardanoEra era -> QuickCheck.Gen [KeyWitness era]
-- genWitnesses era =
--     case cardanoEraStyle era of
--         LegacyByronEra    -> QuickCheck.scale (`mod` 10) $ QuickCheck.listOf1 (hedgehog genByronKeyWitness)
--         ShelleyBasedEra _ -> do
--           bsWits  <- QuickCheck.frequency
--               [ (3, QuickCheck.scale (`mod` 10) $ QuickCheck.listOf1 (hedgehog $ genShelleyBootstrapWitness era))
--               , (1, pure [])
--               ]
--           keyWits <- QuickCheck.frequency
--               [ (3, QuickCheck.scale (`mod` 10) $ QuickCheck.listOf1 (hedgehog $ genShelleyKeyWitness era))
--               , (1, pure [])
--               ]
--           return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> QuickCheck.Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> QuickCheck.Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genWitnessNetworkIdOrByronAddress :: QuickCheck.Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  QuickCheck.oneof
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

-- genShelleyBootstrapWitness
--   :: IsShelleyBasedEra era
--   => CardanoEra era
--   -> Gen (KeyWitness era)
-- genShelleyBootstrapWitness era =
--  makeShelleyBootstrapWitness
--    <$> genWitnessNetworkIdOrByronAddress
--    <*> genTxBody era
--    <*> genSigningKey AsByronKey

-- genShelleyKeyWitness
--   :: IsShelleyBasedEra era
--   => CardanoEra era
--   -> Gen (KeyWitness era)
-- genShelleyKeyWitness era =
--   makeShelleyKeyWitness
--     <$> genTxBody era
--     <*> genShelleyWitnessSigningKey

-- genShelleyWitness
--   :: IsShelleyBasedEra era
--   => CardanoEra era
--   -> Gen (KeyWitness era)
-- genShelleyWitness era =
--   Gen.choice
--    [ genShelleyKeyWitness era
--    , genShelleyBootstrapWitness era
--    ]

genShelleyWitnessSigningKey :: QuickCheck.Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  QuickCheck.oneof
      [ WitnessPaymentKey
        <$> genSigningKey AsPaymentKey
      , WitnessPaymentExtendedKey
        <$> genSigningKey AsPaymentExtendedKey
      , WitnessStakeKey
        <$> genSigningKey AsStakeKey
      , WitnessStakeExtendedKey
        <$> genSigningKey AsStakeExtendedKey
      , WitnessStakePoolKey
        <$> genSigningKey AsStakePoolKey
      , WitnessGenesisDelegateKey
        <$> genSigningKey AsGenesisDelegateKey
      , WitnessGenesisUTxOKey
        <$> genSigningKey AsGenesisUTxOKey
      ]

genSeed :: Int -> QuickCheck.Gen Crypto.Seed
genSeed n = (Crypto.mkSeedFromBytes . BS.pack) <$> QuickCheck.vector n

-- genNat :: QuickCheck.Gen Natural
-- genNat = (fromIntegral . QuickCheck.getPositive)
--     <$> (QuickCheck.arbitrary :: QuickCheck.Gen (QuickCheck.Positive Integer))

-- genRational :: QuickCheck.Gen Rational
-- genRational =
--     (\d -> ratioToRational (1 % d)) <$> genDenominator
--   where
--     genDenominator :: QuickCheck.Gen Word64
--     genDenominator = QuickCheck.chooseBoundedIntegral (1, maxBound)

--     ratioToRational :: Ratio Word64 -> Rational
--     ratioToRational = toRational

-- -- TODO: consolidate this back to just genRational once this is merged:
-- -- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
-- genRationalInt64 :: QuickCheck.Gen Rational
-- genRationalInt64 =
--     (\d -> ratioToRational (1 % d)) <$> genDenominator
--   where
--     genDenominator :: QuickCheck.Gen Int64
--     genDenominator = QuickCheck.chooseBoundedIntegral (1, maxBound)

--     ratioToRational :: Ratio Int64 -> Rational
--     ratioToRational = toRational

genEpochNo :: QuickCheck.Gen EpochNo
genEpochNo = EpochNo <$> QuickCheck.arbitrary

-- genPraosNonce :: QuickCheck.Gen PraosNonce
-- genPraosNonce = makePraosNonce <$> QuickCheck.arbitrary

-- genMaybePraosNonce :: QuickCheck.Gen (Maybe PraosNonce)
-- genMaybePraosNonce = QuickCheck.liftArbitrary genPraosNonce

-- genProtocolParameters :: QuickCheck.Gen ProtocolParameters
-- genProtocolParameters =
--   ProtocolParameters
--     <$> ((,) <$> genNat <*> genNat)
--     <*> genRational
--     <*> genMaybePraosNonce
--     <*> genNat
--     <*> genNat
--     <*> genNat
--     <*> genNat
--     <*> genNat
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> genLovelace
--     <*> genLovelace
--     <*> genLovelace
--     <*> genEpochNo
--     <*> genNat
--     <*> genRationalInt64
--     <*> genRational
--     <*> genRational
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> genCostModels
--     <*> QuickCheck.liftArbitrary genExecutionUnitPrices
--     <*> QuickCheck.liftArbitrary genExecutionUnits
--     <*> QuickCheck.liftArbitrary genExecutionUnits
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat

-- genProtocolParametersUpdate :: QuickCheck.Gen ProtocolParametersUpdate
-- genProtocolParametersUpdate =
--   ProtocolParametersUpdate
--     <$> QuickCheck.liftArbitrary ((,) <$> genNat <*> genNat)
--     <*> QuickCheck.liftArbitrary genRational
--     <*> QuickCheck.liftArbitrary genMaybePraosNonce
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> QuickCheck.liftArbitrary genEpochNo
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genRationalInt64
--     <*> QuickCheck.liftArbitrary genRational
--     <*> QuickCheck.liftArbitrary genRational
--     <*> QuickCheck.liftArbitrary genLovelace
--     <*> genCostModels
--     <*> QuickCheck.liftArbitrary genExecutionUnitPrices
--     <*> QuickCheck.liftArbitrary genExecutionUnits
--     <*> QuickCheck.liftArbitrary genExecutionUnits
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat
--     <*> QuickCheck.liftArbitrary genNat

-- genUpdateProposal :: QuickCheck.Gen UpdateProposal
-- genUpdateProposal =
--   UpdateProposal
--     <$> Map.fromList <$> QuickCheck.listOf
--                 ((,) <$> genVerificationKeyHash AsGenesisKey
--                      <*> genProtocolParametersUpdate)
--     <*> genEpochNo

-- genCostModel :: QuickCheck.Gen CostModel
-- genCostModel = case Plutus.defaultCostModelParams of
--   Nothing -> error "Plutus defaultCostModelParams is broken."
--   Just dcm ->
--       CostModel
--     -- TODO This needs to be the cost model struct for whichever
--     -- Plutus version we're using, once we support multiple Plutus versions.
--     <$> mapM (const $ QuickCheck.scale (`mod` 5000) QuickCheck.arbitrary) dcm

-- genCostModels :: QuickCheck.Gen (Map AnyPlutusScriptVersion CostModel)
-- genCostModels = QuickCheck.scale (`mod` (length plutusScriptVersions)) $
--     Map.fromList <$> QuickCheck.listOf
--             ((,) <$> QuickCheck.elements plutusScriptVersions
--                  <*> genCostModel)
--   where
--     plutusScriptVersions :: [AnyPlutusScriptVersion]
--     plutusScriptVersions = [minBound..maxBound]

-- genExecutionUnitPrices :: QuickCheck.Gen ExecutionUnitPrices
-- genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHash :: CardanoEra era -> QuickCheck.Gen (TxOutDatumHash era)
genTxOutDatumHash era = case era of
    ByronEra -> pure TxOutDatumHashNone
    ShelleyEra -> pure TxOutDatumHashNone
    AllegraEra -> pure TxOutDatumHashNone
    MaryEra -> pure TxOutDatumHashNone
    AlonzoEra -> QuickCheck.oneof
      [ pure TxOutDatumHashNone
      , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
      ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: QuickCheck.Gen (Cardano.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> (QuickCheck.scale (`mod` 10) $ QuickCheck.arbitrary)
