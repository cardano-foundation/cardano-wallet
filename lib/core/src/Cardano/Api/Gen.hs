{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Gen
  ( genLovelace
  , genNetworkMagic
  , genExecutionUnits
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
  , genTxMetadataValue
  , genTxMetadata
  , genNat

  -- * No coverage yet
  , genScriptInEra
  , genTxMetadataInEra
  , genTxAuxScripts
  , genTxWithdrawals
  , genWithdrawalInfo
  , genTxCertificates
  , genTx
  , genTxBodyContent
  , genTxUpdateProposal
  , genByronKeyWitness
  , genShelleyKeyWitness
  , genShelleyWitness

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
    ( KeyWitness (ByronKeyWitness), WitnessNetworkIdOrByronAddress (..) )
import Cardano.Api.Shelley
    ( Hash (ScriptDataHash)
    , PlutusScript (PlutusScriptSerialised)
    , ProtocolParameters (ProtocolParameters)
    , StakeCredential (..)
    )
import Cardano.Ledger.Credential
    ( Ix, Ptr (..) )
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
    ( maybeToList )
import Data.Ratio
    ( Ratio, (%) )
import Data.String
import Data.Text
    ( Text )
import Data.Word
    ( Word64, Word8 )
import Numeric.Natural
    ( Natural )
import Test.Cardano.Chain.UTxO.Gen
    ( genVKWitness )
import Test.Cardano.Crypto.Gen
    ( genProtocolMagicId )
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , Large (..)
    , Positive (..)
    , choose
    , chooseBoundedIntegral
    , chooseInt
    , elements
    , frequency
    , liftArbitrary
    , listOf
    , listOf1
    , oneof
    , scale
    , sized
    , vector
    , vectorOf
    )
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
import qualified Data.Text as Text
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Shelley.Spec.Ledger.TxBody as Ledger
    ( EraIndependentTxBody )

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = makeByronAddress <$> genNetworkId
                                   <*> genVerificationKey AsByronKey

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = makeShelleyAddress <$> genNetworkId
                                       <*> genPaymentCredential
                                       <*> genStakeAddressReference

genAddressInEra :: CardanoEra era -> Gen (AddressInEra era)
genAddressInEra era =
  case cardanoEraStyle era of
    LegacyByronEra ->
      byronAddressInEra <$> genAddressByron

    ShelleyBasedEra _ ->
      oneof
        [ byronAddressInEra   <$> genAddressByron
        , shelleyAddressInEra <$> genAddressShelley
        ]

genLovelace :: Gen Lovelace
genLovelace = do
    (Large (n :: Word64)) <- arbitrary
    pure $ quantityToLovelace $ Quantity $ toInteger n

-- ----------------------------------------------------------------------------
-- -- SimpleScript generators
-- --

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

genSimpleScript :: SimpleScriptVersion lang -> Gen (SimpleScript lang)
genSimpleScript lang =
    sized genTerm
  where
    genTerm 0 = oneof nonRecursive
    genTerm n = frequency
        [ (3, oneof (recursive n))
        , (1, oneof nonRecursive)
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
        [ RequireAllOf <$> (scale (`mod` 10) $ listOf $ recurse n)

        , RequireAnyOf <$> (scale (`mod` 10) $ listOf $ recurse n)

        , do ts <- scale (`mod` 10) $ listOf $ recurse n
             m  <- choose (0, length ts)
             return (RequireMOf m ts)
        ]

    recurse n = do
        (Positive m) <- arbitrary
        genTerm (n `div` (m + 3))

genScriptData :: Gen ScriptData
genScriptData =
    sized genTerm

    where
        genTerm 0 = oneof nonRecursive
        genTerm n = frequency
            [ (3, oneof (recursive n))
            , (1, oneof nonRecursive)
            ]

        -- Non-recursive generators
        nonRecursive =
            [ do
                 (Large (n :: Int64)) <- arbitrary
                 pure $ ScriptDataNumber $ fromIntegral n
            , do
                 (Large (n :: Word8)) <- arbitrary
                 (ScriptDataBytes . BS.pack) <$> vector (fromIntegral n)
            ]

        -- Recursive generators
        recursive n =
            [ ScriptDataList <$> listOf (recurse n)
            , ScriptDataMap <$> listOf ((,) <$> recurse n <*> recurse n)
            , ScriptDataConstructor <$> arbitrary <*> listOf (recurse n)
            ]

        recurse n = do
            (Positive m) <- arbitrary
            genTerm (n `div` (m + 3))

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> arbitrary

-- -- ----------------------------------------------------------------------------
-- -- Script generators for any language, or any language valid in a specific era
-- --

genScriptInAnyLang :: Gen ScriptInAnyLang
genScriptInAnyLang =
    oneof
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound] ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    oneof
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- [scriptLanguageSupportedInEra era lang] ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang
    return (hashScript script)

genTxMetadata :: Gen TxMetadata
genTxMetadata =
    sized $ \sz ->
      fmap (TxMetadata . Map.fromList) $ do
          n <- chooseInt (0, fromIntegral sz)
          vectorOf n
               ((,) <$> (getLarge <$> arbitrary)
                    <*> genTxMetadataValue)

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    sized $ \sz ->
        frequency
            [ (1, TxMetaNumber <$> genTxMetaNumber)
            , (1, TxMetaBytes  <$> genTxMetaBytes)
            , (1, TxMetaText   <$> genTxMetaText)
            , (fromIntegral (signum sz),
                  TxMetaList <$> scale (`div` 2) genTxMetaList)
            , (fromIntegral (signum sz),
                  TxMetaMap <$> scale (`div` 2) genTxMetaMap)
            ]
    where
        genTxMetaNumber :: Gen Integer
        genTxMetaNumber = do
            (Large (n :: Int64)) <- arbitrary
            pure (fromIntegral n)

        genTxMetaBytes :: Gen ByteString
        genTxMetaBytes = do
            n <- chooseInt (0, 64)
            BS.pack <$> vector n

        genTxMetaText :: Gen Text
        genTxMetaText = do
            n <- chooseInt (0, 64)
            Text.pack <$> vectorOf n genAlphaNum

        genTxMetaList :: Gen [TxMetadataValue]
        genTxMetaList = sized $ \sz -> do
            n <- chooseInt (0, sz)
            vectorOf n genTxMetadataValue

        genTxMetaMap :: Gen [(TxMetadataValue, TxMetadataValue)]
        genTxMetaMap = sized $ \sz -> do
            n <- chooseInt (0, sz)
            vectorOf n
                ((,) <$> genTxMetadataValue <*> genTxMetadataValue)

----------------------------------------------------------------------------
-- Multi-asset generators
--
genAssetName :: Gen AssetName
genAssetName =
  frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, elements ["", "a", "b", "c"])
    , (1, AssetName <$> fromString <$> (vectorOf 32 genAlphaNum))
    , (1, AssetName <$> fromString <$> (
              scale (\n -> (n `mod` 31) + 1)
                  (listOf genAlphaNum)
              )
      )
    ]

genAlphaNum :: Gen Char
genAlphaNum = elements
    "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genPolicyId :: Gen PolicyId
genPolicyId =
  frequency
      -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, elements [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])

       -- and some from the full range of the type
    , (1, PolicyId <$> genScriptHash)
    ]

genAssetId :: Gen AssetId
genAssetId = oneof
    [ AssetId <$> genPolicyId <*> genAssetName
    , return AdaAssetId
    ]

genQuantity :: Gen Quantity
genQuantity = do
    (Large (n :: Int64)) <- arbitrary
    pure $ fromIntegral n

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = genQuantity

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = do
    (Large (n :: Word64)) <- arbitrary
    pure $ fromIntegral n

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    listOf ((,) <$> genAId <*> genQuant)


-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity

genNetworkId :: Gen NetworkId
genNetworkId =
    oneof
        [ pure Mainnet
        , Testnet <$> genNetworkMagic
        ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = do
    (Large n) <- arbitrary
    pure $ NetworkMagic n

genPaymentCredential :: Gen PaymentCredential
genPaymentCredential =
    oneof
        [ byKey
        , byScript
        ]
    where
        byKey :: Gen PaymentCredential
        byKey = do
            vKey <- genVerificationKey AsPaymentKey
            return . PaymentCredentialByKey $ verificationKeyHash vKey

        byScript :: Gen PaymentCredential
        byScript = PaymentCredentialByScript <$> genScriptHash

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk
  where
    seedSize :: Word
    seedSize = deterministicSigningKeySeedSize roletoken

genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  oneof
    [ StakeAddressByValue <$> genStakeCredential
    , (StakeAddressByPointer . StakeAddressPointer) <$> genPtr
    , return NoStakeAddress
    ]

genPtr :: Gen Ptr
genPtr = Ptr <$> genSlotNo <*> genIx <*> genIx

genIx :: Gen Ix
genIx = do
    (Large (n :: Word64)) <- arbitrary
    pure n

genStakeCredential :: Gen StakeCredential
genStakeCredential =
  oneof
    [ byKey
    , byScript
    ]

  where
      byKey = do
          vKey <- genVerificationKey AsStakeKey
          return . StakeCredentialByKey $ verificationKeyHash vKey

      byScript = StakeCredentialByScript <$> genScriptHash

genStakeAddress :: Gen StakeAddress
genStakeAddress = makeStakeAddress <$> genNetworkId <*> genStakeCredential

genShelleyHash :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> arbitrary

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = do
    (Large (n :: Word)) <- arbitrary
    pure $ TxIx n

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era =
  case multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> TxOutAdaOnly adaOnlyInEra <$> genLovelace
    Right multiAssetInEra -> TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOut :: CardanoEra era -> Gen (TxOut era)
genTxOut era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatumHash era

genTtl :: Gen SlotNo
genTtl = genSlotNo

-- TODO: Accept a range for generating ttl.
genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

-- TODO: Accept a range for generating ttl.
genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
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
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
        oneof
            [ pure TxMetadataNone
            , TxMetadataInEra supported <$> genTxMetadata
            ]

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
      TxAuxScripts supported <$>
        scale (`mod` 3) (listOf (genScriptInEra era))

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing ->
        pure TxWithdrawalsNone
    Just supported -> do
        frequency
          [ ( 1 , pure TxWithdrawalsNone )
          , ( 3 , TxWithdrawals supported
                  <$> listOf (genWithdrawalInfo era) )
          ]

genWithdrawalInfo
    :: CardanoEra era
    -> Gen ( StakeAddress
                      , Lovelace
                      , BuildTxWith BuildTx (Witness WitCtxStake era)
                      )
genWithdrawalInfo era = do
    stakeAddr <- genStakeAddress
    amt <- genLovelace
    wit <- BuildTxWith <$> genWitnessStake era
    pure (stakeAddr, amt, wit)

genWitnessStake :: CardanoEra era -> Gen (Witness WitCtxStake era)
genWitnessStake era = oneof $
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
    -> Gen (ScriptWitness WitCtxStake era)
genScriptWitnessStake langEra =
    case languageOfScriptLanguageInEra langEra of
        (SimpleScriptLanguage ver) ->
            SimpleScriptWitness langEra ver <$> genSimpleScript ver
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScript ver
            <*> pure NoScriptDatumForStake
            <*> genScriptData
            <*> genExecutionUnits

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = do
    (Large executionSteps) <- arbitrary
    (Large executionMemory) <- arbitrary
    pure $ ExecutionUnits executionSteps executionMemory

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
  case certificatesSupportedInEra era of
    Nothing -> pure TxCertificatesNone
    Just supported ->
      oneof
        [ pure TxCertificatesNone
        , pure (TxCertificates supported mempty $ BuildTxWith mempty)
          -- TODO: Generate certificates
        ]

genTxUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genTxUpdateProposal era =
  case updateProposalSupportedInEra era of
    Nothing -> pure TxUpdateProposalNone
    Just supported ->
      oneof
        [ pure TxUpdateProposalNone
        , TxUpdateProposal supported <$> genUpdateProposal
        ]

genTxMintValue :: CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported ->
      oneof
        [ pure TxMintNone
        -- TODO gen policy IDs
        , TxMintValue supported <$> genValueForMinting <*> return (BuildTxWith mempty)
        ]

genExtraKeyWitnesses :: CardanoEra era -> Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses era =
    case extraKeyWitnessesSupportedInEra era of
        Nothing -> pure TxExtraKeyWitnessesNone
        Just supported  -> oneof
            [ pure TxExtraKeyWitnessesNone
            , TxExtraKeyWitnesses supported
              <$> listOf (genVerificationKeyHash AsPaymentKey)
            ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
  txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending)) <$> listOf1 genTxIn
  txInsCollateral <- genTxInsCollateral era
  txOuts <- listOf (genTxOut era)
  txFee <- genTxFee era
  txValidityRange <- genTxValidityRange era
  txMetadata <- genTxMetadataInEra era
  txAuxScripts <- genTxAuxScripts era
  let txExtraScriptData = BuildTxWith TxExtraScriptDataNone --TODO: Alonzo era: Generate extra script data
  let txExtraKeyWits = TxExtraKeyWitnessesNone --TODO: Alonzo era: Generate witness key hashes
  txProtocolParams <- BuildTxWith <$> liftArbitrary genProtocolParameters
  txWithdrawals <- genTxWithdrawals era
  txCertificates <- genTxCertificates era
  txUpdateProposal <- genTxUpdateProposal era
  txMintValue <- genTxMintValue era
  txScriptValidity <- genTxScriptValidity era

  pure $ TxBodyContent
    { Api.txIns
    , Api.txInsCollateral
    , Api.txOuts
    , Api.txFee
    , Api.txValidityRange
    , Api.txMetadata
    , Api.txAuxScripts
    , Api.txExtraScriptData
    , Api.txExtraKeyWits
    , Api.txProtocolParams
    , Api.txWithdrawals
    , Api.txCertificates
    , Api.txUpdateProposal
    , Api.txMintValue
    , Api.txScriptValidity
    }

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> oneof
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported <$> listOf genTxIn
                          ]

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  supported -> pure (TxFeeImplicit supported)
    Right supported -> TxFeeExplicit supported <$> genLovelace

genTxBody :: IsCardanoEra era => CardanoEra era -> Gen (TxBody era)
genTxBody era = do
  res <- makeTransactionBody <$> genTxBodyContent era
  case res of
    Left err -> error (displayError err)
    Right txBody -> pure txBody

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = elements [ScriptInvalid, ScriptValid]

genTx :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTx era =
  makeSignedTransaction
    <$> genWitnesses era
    <*> genTxBody era

genByronKeyWitness :: Gen (KeyWitness ByronEra)
genByronKeyWitness = do
  pmId <- hedgehog $ genProtocolMagicId
  txinWitness <- hedgehog $ genVKWitness pmId
  return $ ByronKeyWitness txinWitness

genWitnesses :: CardanoEra era -> Gen [KeyWitness era]
genWitnesses era =
    case cardanoEraStyle era of
        LegacyByronEra    -> scale (`mod` 10) $ listOf1 genByronKeyWitness
        ShelleyBasedEra _ -> do
          bsWits  <- frequency
              [ (3, scale (`mod` 10) $ listOf1 (genShelleyBootstrapWitness era))
              , (1, pure [])
              ]
          keyWits <- frequency
              [ (3, scale (`mod` 10) $ listOf1 (genShelleyKeyWitness era))
              , (1, pure [])
              ]
          return $ bsWits ++ keyWits

genVerificationKey :: Key keyrole => AsType keyrole -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genWitnessNetworkIdOrByronAddress :: Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  oneof
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

genShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyBootstrapWitness era =
 makeShelleyBootstrapWitness
   <$> genWitnessNetworkIdOrByronAddress
   <*> genTxBody era
   <*> genSigningKey AsByronKey

genShelleyKeyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyKeyWitness era =
  makeShelleyKeyWitness
    <$> genTxBody era
    <*> genShelleyWitnessSigningKey

genShelleyWitness
  :: IsShelleyBasedEra era
  => CardanoEra era
  -> Gen (KeyWitness era)
genShelleyWitness era =
    oneof
        [ genShelleyKeyWitness era
        , genShelleyBootstrapWitness era
        ]

genShelleyWitnessSigningKey :: Gen ShelleyWitnessSigningKey
genShelleyWitnessSigningKey =
  oneof
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

genSeed :: Int -> Gen Crypto.Seed
genSeed n = (Crypto.mkSeedFromBytes . BS.pack) <$> vector n

genNat :: Gen Natural
genNat = do
    Large (n :: Word64) <- arbitrary
    pure $ fromIntegral n

genRational :: Gen Rational
genRational =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Word64
    genDenominator = chooseBoundedIntegral (1, maxBound)

    ratioToRational :: Ratio Word64 -> Rational
    ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = chooseBoundedIntegral (1, maxBound)

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> arbitrary

genPraosNonce :: Gen PraosNonce
genPraosNonce = makePraosNonce <$> arbitrary

genMaybePraosNonce :: Gen (Maybe PraosNonce)
genMaybePraosNonce = liftArbitrary genPraosNonce

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> genRational
    <*> genMaybePraosNonce
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> liftArbitrary genLovelace
    <*> genLovelace
    <*> genLovelace
    <*> genLovelace
    <*> genEpochNo
    <*> genNat
    <*> genRationalInt64
    <*> genRational
    <*> genRational
    <*> liftArbitrary genLovelace
    <*> genCostModels
    <*> liftArbitrary genExecutionUnitPrices
    <*> liftArbitrary genExecutionUnits
    <*> liftArbitrary genExecutionUnits
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate =
  ProtocolParametersUpdate
    <$> liftArbitrary ((,) <$> genNat <*> genNat)
    <*> liftArbitrary genRational
    <*> liftArbitrary genMaybePraosNonce
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genLovelace
    <*> liftArbitrary genLovelace
    <*> liftArbitrary genLovelace
    <*> liftArbitrary genLovelace
    <*> liftArbitrary genEpochNo
    <*> liftArbitrary genNat
    <*> liftArbitrary genRationalInt64
    <*> liftArbitrary genRational
    <*> liftArbitrary genRational
    <*> liftArbitrary genLovelace
    <*> genCostModels
    <*> liftArbitrary genExecutionUnitPrices
    <*> liftArbitrary genExecutionUnits
    <*> liftArbitrary genExecutionUnits
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat
    <*> liftArbitrary genNat

genUpdateProposal :: Gen UpdateProposal
genUpdateProposal =
  UpdateProposal
    <$> Map.fromList <$> listOf
                ((,) <$> genVerificationKeyHash AsGenesisKey
                     <*> genProtocolParametersUpdate)
    <*> genEpochNo

genCostModel :: Gen CostModel
genCostModel = case Plutus.defaultCostModelParams of
  Nothing -> error "Plutus defaultCostModelParams is broken."
  Just dcm ->
      CostModel
    -- TODO This needs to be the cost model struct for whichever
    -- Plutus version we're using, once we support multiple Plutus versions.
    <$> mapM (const $ scale (`mod` 5000) arbitrary) dcm

genCostModels :: Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels = scale (`mod` (length plutusScriptVersions)) $
    Map.fromList <$> listOf
            ((,) <$> elements plutusScriptVersions
                 <*> genCostModel)
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genTxOutDatumHash :: CardanoEra era -> Gen (TxOutDatumHash era)
genTxOutDatumHash era = case era of
    ByronEra -> pure TxOutDatumHashNone
    ShelleyEra -> pure TxOutDatumHashNone
    AllegraEra -> pure TxOutDatumHashNone
    MaryEra -> pure TxOutDatumHashNone
    AlonzoEra -> oneof
      [ pure TxOutDatumHashNone
      , TxOutDatumHash ScriptDataInAlonzoEra <$> genHashScriptData
      ]

mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData = ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> (scale (`mod` 10) $ arbitrary)
