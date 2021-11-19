{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Api.Gen
  ( genTxIn
  , genTxId
  , genTxIndex
  , genShelleyHash
  , genTxInsCollateral
  , genSlotNo
  , genLovelace
  , genTxFee
  , genTtl
  , genTxValidityLowerBound
  , genTxValidityUpperBound
  , genTxValidityRange
  , genTxScriptValidity
  , genScriptValidity
  , genSeed
  , genSigningKey
  , genVerificationKey
  , genVerificationKeyHash
  , genExtraKeyWitnesses
  , genSimpleScript
  , genPlutusScript
  , genScript
  , genScriptInAnyLang
  , genScriptInEra
  , genScriptHash
  , genAssetName
  , genAlphaNum
  , genPolicyId
  , genAssetId
  , genValue
  , genValueForMinting
  , genSignedQuantity
  , genTxMintValue
  , genNetworkMagic
  , genNetworkId
  , genStakeCredential
  , genStakeAddress
  , genScriptData
  , genExecutionUnits
  , genTxWithdrawals
  , genWithdrawalInfo
  , genWitnessStake
  , genScriptWitnessStake
  , genTxAuxScripts
  , genTxMetadataInEra
  , genTxMetadata
  , genTxMetadataValue
  , genIx
  , genPtr
  , genStakeAddressReference
  , genPaymentCredential
  , genAddressByron
  , genAddressShelley
  , genAddressInEra
  , genUnsignedQuantity
  , genValueForTxOut
  , genTxOutValue
  , genTxOut
  , genTxOutDatumHash
  , genWitnessNetworkIdOrByronAddress
  , genByronKeyWitness
  , genShelleyWitnessSigningKey
  , genWitnesses
  , genTxBody
  , genTxBodyContent
  , genTx
  , genTxInEra
  , genNat
  , genRational
  , genRationalInt64
  , genEpochNo
  , genCostModel
  , genCostModels
  , genExecutionUnitPrices
  , genProtocolParameters
  , genPoolId
  , genMIRPot
  , genMIRTarget
  , genTxCertificate
  , genTxCertificates
  , genStakePoolMetadata
  , genStakePoolMetadataReference
  , genStakePoolRelay
  , genStakePoolParameters
  , genProtocolParametersUpdate
  , genUpdateProposal
  , genExtraScriptData
  , genWitness
  ) where

import Prelude

import Cardano.Api hiding
    ( txIns )
import Cardano.Api.Byron
    ( KeyWitness (ByronKeyWitness), WitnessNetworkIdOrByronAddress (..) )
import Cardano.Api.Shelley
    ( Certificate (..)
    , Hash (..)
    , PlutusScript (..)
    , PoolId
    , ProtocolParameters (..)
    , StakeCredential (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataReference (..)
    , StakePoolParameters (..)
    , StakePoolRelay (..)
    )
import Cardano.Ledger.Credential
    ( Ix, Ptr (..) )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Data.Aeson
    ( ToJSON (..), (.=) )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Int
    ( Int64 )
import Data.Map
    ( Map )
import Data.Maybe
    ( maybeToList )
import Data.Maybe.Strict
    ( strictMaybeToMaybe )
import Data.Ratio
    ( Ratio, (%) )
import Data.Set
    ( Set )
import Data.String
    ( fromString )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64, Word8 )
import Network.Socket
    ( PortNumber )
import Numeric.Natural
    ( Natural )
import Shelley.Spec.Ledger.API
    ( MIRPot (..) )
import Test.Cardano.Chain.UTxO.Gen
    ( genVKWitness )
import Test.Cardano.Crypto.Gen
    ( genProtocolMagicId )
import Test.QuickCheck
    ( Gen
    , Large (..)
    , Positive (..)
    , arbitrary
    , choose
    , chooseInt
    , chooseInteger
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
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.BaseTypes as Ledger
    ( Port, dnsToText )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Shelley.Spec.Ledger.API as Ledger
    ( StakePoolRelay (..), portToWord16 )
import qualified Shelley.Spec.Ledger.TxBody as Ledger
    ( EraIndependentTxBody )
import Test.QuickCheck.Gen
    ( resize )
import qualified Test.Shelley.Spec.Ledger.Serialisation.Generators.Genesis as Ledger
    ( genStakePoolRelay )

genShelleyHash
    :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = do
    (Large (n :: Word)) <- arbitrary
    pure $ TxIx n

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era =
    case collateralSupportedInEra era of
      Nothing        -> pure TxInsCollateralNone
      Just supported -> oneof
                          [ pure TxInsCollateralNone
                          , TxInsCollateral supported
                            <$> resize 3 (listOf genTxIn)
                          ]

genSlotNo :: Gen SlotNo
genSlotNo = SlotNo <$> arbitrary

genLovelace :: Gen Lovelace
genLovelace = do
    (Large (n :: Word32)) <- arbitrary
    pure $ quantityToLovelace $ Quantity $ toInteger n

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era =
  case txFeesExplicitInEra era of
    Left  implicit -> pure (TxFeeImplicit implicit)
    Right explicit -> TxFeeExplicit explicit <$> genLovelace

genTtl :: Gen SlotNo
genTtl = genSlotNo

genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era =
  case validityLowerBoundSupportedInEra era of
    Nothing        -> pure TxValidityNoLowerBound
    Just supported -> TxValidityLowerBound supported <$> genTtl

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

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
  Nothing -> pure TxScriptValidityNone
  Just witness -> TxScriptValidity witness <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = elements [ScriptInvalid, ScriptValid]

genSeed :: Int -> Gen Crypto.Seed
genSeed n = (Crypto.mkSeedFromBytes . BS.pack) <$> vector n

genSigningKey :: Key keyrole => AsType keyrole -> Gen (SigningKey keyrole)
genSigningKey roletoken = do
    seed <- genSeed (fromIntegral seedSize)
    let sk = deterministicSigningKey roletoken seed
    return sk

    where
        seedSize :: Word
        seedSize = deterministicSigningKeySeedSize roletoken

genVerificationKey
    :: Key keyrole
    => AsType keyrole
    -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: Key keyrole => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genExtraKeyWitnesses :: CardanoEra era -> Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses era =
    case extraKeyWitnessesSupportedInEra era of
        Nothing -> pure TxExtraKeyWitnessesNone
        Just supported  -> oneof
            [ pure TxExtraKeyWitnessesNone
            , TxExtraKeyWitnesses supported
              <$> resize 3 (listOf (genVerificationKeyHash AsPaymentKey))
            ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> arbitrary

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
        [ RequireAllOf <$> scale (`mod` 10) (listOf $ recurse n)

        , RequireAnyOf <$> scale (`mod` 10) (listOf $ recurse n)

        , do ts <- scale (`mod` 10) $ listOf $ recurse n
             m  <- choose (0, length ts)
             return (RequireMOf m ts)
        ]

    recurse n = do
        (Positive m) <- arbitrary
        genTerm (n `div` (m + 3))

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript (SimpleScriptLanguage lang) =
    SimpleScript lang <$> genSimpleScript lang
genScript (PlutusScriptLanguage lang) =
    PlutusScript lang <$> genPlutusScript lang

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

genAssetName :: Gen AssetName
genAssetName =
  frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, elements ["", "a", "b", "c"])
    , (1, AssetName . fromString <$> (vectorOf 32 genAlphaNum))
    , (1, AssetName . fromString <$> (
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

genValue :: Gen AssetId -> Gen Quantity -> Gen Value
genValue genAId genQuant =
  valueFromList <$>
    resize 5 (listOf ((,) <$> genAId <*> genQuant))

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = do
    (Large (n :: Int64)) <- arbitrary
    pure $ fromIntegral n

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting = genValue genAssetIdNoAda genSignedQuantity
  where
    genAssetIdNoAda :: Gen AssetId
    genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

genTxMintValue :: forall era. CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era =
  case multiAssetSupportedInEra era of
    Left _ -> pure TxMintNone
    Right supported -> do
      let
          scriptWitnessGenerators :: [Gen (ScriptWitness WitCtxMint era)]
          scriptWitnessGenerators =
              [ genScriptWitnessMint langInEra
              | AnyScriptLanguage lang <- [minBound..maxBound]
              , Just langInEra <- [scriptLanguageSupportedInEra era lang]
              ]
      oneof
        [ pure TxMintNone
        , TxMintValue supported
          <$> genValueForMinting
          <*> ( (BuildTxWith . Map.fromList)
                <$> resize 3 (listOf ( (,)
                             <$> genPolicyId
                             <*> oneof scriptWitnessGenerators
                           ))
              )
        ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = do
    (Large n) <- arbitrary
    pure $ NetworkMagic n

genNetworkId :: Gen NetworkId
genNetworkId =
    oneof
        [ pure Mainnet
        , Testnet <$> genNetworkMagic
        ]

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
            [ ScriptDataList <$> resize 3 (listOf (recurse n))
            , ScriptDataMap
              <$> resize 3 (listOf ((,) <$> recurse n <*> recurse n))
            , ScriptDataConstructor
              <$> arbitrary
              <*> (resize 3 $ listOf (recurse n))
            ]

        recurse n = do
            (Positive m) <- arbitrary
            genTerm (n `div` (m + 3))

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = do
    (Large steps) <- arbitrary
    (Large mem) <- arbitrary
    pure $ ExecutionUnits steps mem

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era =
  case withdrawalsSupportedInEra era of
    Nothing ->
        pure TxWithdrawalsNone
    Just supported -> do
        frequency
          [ ( 1 , pure TxWithdrawalsNone )
          , ( 1 , pure $ TxWithdrawals supported [] )
          , ( 3 , TxWithdrawals supported
                  <$> resize 3 (listOf1 (genWithdrawalInfo era))
            )
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
    <> [ ScriptWitness ScriptWitnessForStakeAddr
         <$> genScriptWitnessStake langInEra
       | AnyScriptLanguage lang <- [minBound..maxBound]
       , Just langInEra <- [scriptLanguageSupportedInEra era lang]
       ]

genScriptWitnessMint
    :: ScriptLanguageInEra lang era
    -> Gen (ScriptWitness WitCtxMint era)
genScriptWitnessMint langEra =
    case languageOfScriptLanguageInEra langEra of
        (SimpleScriptLanguage ver) ->
            SimpleScriptWitness langEra ver <$> genSimpleScript ver
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScript ver
            <*> pure NoScriptDatumForMint
            <*> genScriptData
            <*> genExecutionUnits

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

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era =
  case auxScriptsSupportedInEra era of
    Nothing -> pure TxAuxScriptsNone
    Just supported ->
        frequency
        [ (1, pure TxAuxScriptsNone)
        , (3, TxAuxScripts supported
          <$> (resize 3 $ listOf (genScriptInEra era)))
        ]

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era =
  case txMetadataSupportedInEra era of
    Nothing -> pure TxMetadataNone
    Just supported ->
        oneof
            [ pure TxMetadataNone
            , TxMetadataInEra supported <$> genTxMetadata
            ]

genTxMetadata :: Gen TxMetadata
genTxMetadata =
    fmap (TxMetadata . Map.fromList) $ do
        n <- chooseInt (0, 10)
        vectorOf n
            ((,) <$> (getLarge <$> arbitrary)
                 <*> genTxMetadataValue)

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    sized $ \sz ->
        frequency
            [ (5, TxMetaNumber <$> genTxMetaNumber)
            , (5, TxMetaBytes  <$> genTxMetaBytes)
            , (5, TxMetaText   <$> genTxMetaText)
            , (fromIntegral (signum sz),
                  TxMetaList <$> genTxMetaList)
            , (fromIntegral (signum sz),
                  TxMetaMap <$> genTxMetaMap)
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
            T.pack <$> vectorOf n genAlphaNum

        genTxMetaList :: Gen [TxMetadataValue]
        genTxMetaList = do
            n <- chooseInt (0, 10)
            vectorOf n genTxMetadataValue

        genTxMetaMap :: Gen [(TxMetadataValue, TxMetadataValue)]
        genTxMetaMap = do
            n <- chooseInt (0, 10)
            vectorOf n
                ((,) <$> genTxMetadataValue <*> genTxMetadataValue)

genPtr :: Gen Ptr
genPtr = Ptr <$> genSlotNo <*> genIx <*> genIx

genIx :: Gen Ix
genIx = do
    (Large (n :: Word64)) <- arbitrary
    pure n

genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  oneof
    [ StakeAddressByValue <$> genStakeCredential
    , (StakeAddressByPointer . StakeAddressPointer) <$> genPtr
    , return NoStakeAddress
    ]

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

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = do
    (Large (n :: Word32)) <- arbitrary
    pure $ fromIntegral n

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = genValue genAssetId genUnsignedQuantity

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

genTxOutDatumHash :: CardanoEra era -> Gen (TxOutDatumHash era)
genTxOutDatumHash era =
    case scriptDataSupportedInEra era of
        Nothing -> pure TxOutDatumHashNone
        Just supported -> oneof
            [ pure TxOutDatumHashNone
            , TxOutDatumHash supported <$> genHashScriptData
            ]

mkDummyHash :: forall h a. Crypto.HashAlgorithm h => Int -> Crypto.Hash h a
mkDummyHash = coerce . Crypto.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData =
    ScriptDataHash . unsafeMakeSafeHash . mkDummyHash
        <$> (scale (`mod` 10) arbitrary)

genNat :: Gen Natural
genNat = do
    Large (n :: Word64) <- arbitrary
    pure $ fromIntegral n

genRational :: Gen Rational
genRational =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Word64
    genDenominator = do
        (Positive (Large n)) <- arbitrary
        pure n

    ratioToRational :: Ratio Word64 -> Rational
    ratioToRational = toRational

-- TODO: consolidate this back to just genRational once this is merged:
-- https://github.com/input-output-hk/cardano-ledger-specs/pull/2330
genRationalInt64 :: Gen Rational
genRationalInt64 =
    (\d -> ratioToRational (1 % d)) <$> genDenominator
  where
    genDenominator :: Gen Int64
    genDenominator = do
        (Positive (Large n)) <- arbitrary
        pure n

    ratioToRational :: Ratio Int64 -> Rational
    ratioToRational = toRational

genPraosNonce :: Gen PraosNonce
genPraosNonce = makePraosNonce <$> arbitrary

genEpochNo :: Gen EpochNo
genEpochNo = EpochNo <$> arbitrary

genCostModel :: Gen CostModel
genCostModel = case Plutus.defaultCostModelParams of
  Nothing -> error "Plutus defaultCostModelParams is broken."
  Just dcm ->
      CostModel
    -- TODO This needs to be the cost model struct for whichever
    -- Plutus version we're using, once we support multiple Plutus versions.
    <$> mapM (const $ chooseInteger (0, 5000)) dcm

genCostModels :: Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels = do
    n <- chooseInt (0, length plutusScriptVersions)
    Map.fromList
        <$> vectorOf n ((,) <$> elements plutusScriptVersions <*> genCostModel)
  where
    plutusScriptVersions :: [AnyPlutusScriptVersion]
    plutusScriptVersions = [minBound..maxBound]

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

genProtocolParameters :: Gen ProtocolParameters
genProtocolParameters =
  ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> genRational
    <*> liftArbitrary genPraosNonce
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

genProtocolParametersWithAlonzoScripts :: Gen ProtocolParameters
genProtocolParametersWithAlonzoScripts =
  ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> genRational
    <*> liftArbitrary genPraosNonce
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
    <*> (Just <$> genLovelace)
    <*> genCostModels
    <*> (Just <$> genExecutionUnitPrices)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)

genWitnessNetworkIdOrByronAddress :: Gen WitnessNetworkIdOrByronAddress
genWitnessNetworkIdOrByronAddress =
  oneof
    [ WitnessNetworkId <$> genNetworkId
    , WitnessByronAddress <$> genAddressByron
    ]

genByronKeyWitness :: Gen (KeyWitness ByronEra)
genByronKeyWitness = do
  pmId <- hedgehog genProtocolMagicId
  txinWitness <- hedgehog $ genVKWitness pmId
  return $ ByronKeyWitness txinWitness

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
      , WitnessGenesisKey
        <$> genSigningKey AsGenesisKey
      , WitnessGenesisExtendedKey
        <$> genSigningKey AsGenesisExtendedKey
      , WitnessGenesisDelegateKey
        <$> genSigningKey AsGenesisDelegateKey
      , WitnessGenesisDelegateExtendedKey
        <$> genSigningKey AsGenesisDelegateExtendedKey
      , WitnessGenesisUTxOKey
        <$> genSigningKey AsGenesisUTxOKey
      ]

genPoolId :: Gen PoolId
genPoolId = genVerificationKeyHash AsStakePoolKey

genMIRPot :: Gen MIRPot
genMIRPot = elements [ ReservesMIR, TreasuryMIR ]

genMIRTarget :: Gen MIRTarget
genMIRTarget =
    oneof
        [ StakeAddressesMIR
          <$> resize 3 (listOf ((,) <$> genStakeCredential <*> genLovelace))
        , SendToReservesMIR <$> genLovelace
        , SendToTreasuryMIR <$> genLovelace
        ]

genStakePoolMetadata :: Gen StakePoolMetadata
genStakePoolMetadata =
    StakePoolMetadata
    <$> genName
    <*> genDescription
    <*> genTicker
    <*> genHomepage

    where
        genName :: Gen T.Text
        genName = do
            -- There is a limit of 50 characters on the name
            n <- chooseInt (0, 50)
            T.pack <$> vector n

        genDescription :: Gen T.Text
        genDescription = do
            n <- arbitrary
            T.pack <$> vector n

        genTicker :: Gen T.Text
        genTicker = do
            n <- chooseInt (3, 5)
            T.pack <$> vector n

        genHomepage :: Gen T.Text
        genHomepage = do
            -- There is a limit of 64 bytes on the size of the URL
            scheme <- elements [ "http://"
                               , "https://"
                               ]
            host <- T.pack <$> vectorOf 10 genAlphaNum
            domain <- elements [ ".com"
                               , ".net"
                               , ".org"
                               ]
            elements [ ""
                     , scheme <> host <> domain
                     ]

instance ToJSON StakePoolMetadata where
    toJSON (StakePoolMetadata name description ticker homepage) =
        Aeson.object
            [ "name" .= name
            , "description" .= description
            , "ticker" .= ticker
            , "homepage" .= homepage
            ]

genStakePoolMetadataReference :: Gen StakePoolMetadataReference
genStakePoolMetadataReference = do
    meta@(StakePoolMetadata _name _desc _ticker homepage) <- genStakePoolMetadata
    pure $ StakePoolMetadataReference homepage (hashStakePoolMetadata meta)

    where
        hashStakePoolMetadata :: StakePoolMetadata -> Hash StakePoolMetadata
        hashStakePoolMetadata meta = do
            let json = Aeson.encode meta
            case validateAndHashStakePoolMetadata (BL.toStrict json) of
                Left err -> error
                    $ "genStakePoolMetadata generated an invalid stake pool metadata: "
                      <> show err
                Right (_meta, metaHash) ->
                    metaHash

genStakePoolRelay :: Gen StakePoolRelay
genStakePoolRelay = do
    relay <- hedgehog Ledger.genStakePoolRelay
    pure $ case relay of
        Ledger.SingleHostAddr mPort mIPv4 mIPv6 ->
            StakePoolRelayIp
                (strictMaybeToMaybe mIPv4)
                (strictMaybeToMaybe mIPv6)
                (castPort <$> strictMaybeToMaybe mPort)
        Ledger.SingleHostName mPort dnsName ->
            StakePoolRelayDnsARecord
                (T.encodeUtf8 . Ledger.dnsToText $ dnsName)
                (castPort <$> strictMaybeToMaybe mPort)
        Ledger.MultiHostName dnsName ->
            StakePoolRelayDnsSrvRecord
                (T.encodeUtf8 . Ledger.dnsToText $ dnsName)

    where
        castPort :: Ledger.Port -> PortNumber
        castPort = fromInteger . toInteger . Ledger.portToWord16

genStakePoolParameters :: Gen StakePoolParameters
genStakePoolParameters =
    StakePoolParameters
    <$> genPoolId
    <*> genVerificationKeyHash AsVrfKey
    <*> genLovelace
    <*> genRational
    <*> genStakeAddress
    <*> genLovelace
    <*> resize 3 (listOf (genVerificationKeyHash AsStakeKey))
    <*> resize 3 (listOf genStakePoolRelay)
    <*> liftArbitrary genStakePoolMetadataReference

genTxCertificate :: Gen Certificate
genTxCertificate =
    oneof
        [ StakeAddressRegistrationCertificate <$> genStakeCredential
        , StakeAddressDeregistrationCertificate <$> genStakeCredential
        , StakeAddressDelegationCertificate <$> genStakeCredential <*> genPoolId
        , StakePoolRegistrationCertificate <$> genStakePoolParameters
        , StakePoolRetirementCertificate <$> genPoolId <*> genEpochNo
        , GenesisKeyDelegationCertificate
          <$> genVerificationKeyHash AsGenesisKey
          <*> genVerificationKeyHash AsGenesisDelegateKey
          <*> genVerificationKeyHash AsVrfKey
        , do
              target <- genMIRTarget
              pot <- case target of
                  SendToTreasuryMIR _ -> pure ReservesMIR
                  SendToReservesMIR _ -> pure TreasuryMIR
                  _ -> genMIRPot
              pure $ MIRCertificate pot target
        ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era =
    case certificatesSupportedInEra era of
        Nothing ->
            pure TxCertificatesNone
        Just supported ->
            oneof
                [ pure TxCertificatesNone
                , TxCertificates supported
                  <$> resize 3 (listOf genTxCertificate)
                  <*> ( (BuildTxWith . Map.fromList)
                        <$> (resize 3 $ listOf ( (,)
                                     <$> genStakeCredential
                                     <*> genWitnessStake era
                                   ))
                      )
                ]

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate = do
    protocolUpdateProtocolVersion <-
        liftArbitrary ((,) <$> genNat <*> genNat)
    protocolUpdateDecentralization <-
        liftArbitrary genRational
    protocolUpdateExtraPraosEntropy <-
        liftArbitrary (liftArbitrary genPraosNonce)
    protocolUpdateMaxBlockHeaderSize <-
        liftArbitrary genNat
    protocolUpdateMaxBlockBodySize <-
        liftArbitrary genNat
    protocolUpdateMaxTxSize <-
        liftArbitrary genNat
    protocolUpdateTxFeeFixed <-
        liftArbitrary genNat
    protocolUpdateTxFeePerByte <-
        liftArbitrary genNat
    protocolUpdateMinUTxOValue <-
        liftArbitrary genLovelace
    protocolUpdateStakeAddressDeposit <-
        liftArbitrary genLovelace
    protocolUpdateStakePoolDeposit <-
        liftArbitrary genLovelace
    protocolUpdateMinPoolCost <-
        liftArbitrary genLovelace
    protocolUpdatePoolRetireMaxEpoch <-
        liftArbitrary genEpochNo
    protocolUpdateStakePoolTargetNum <-
        liftArbitrary genNat
    protocolUpdatePoolPledgeInfluence <-
        liftArbitrary genRational
    protocolUpdateMonetaryExpansion <-
        liftArbitrary genRational
    protocolUpdateTreasuryCut <-
        liftArbitrary genRational
    protocolUpdateUTxOCostPerWord <-
        liftArbitrary genLovelace
    protocolUpdateCostModels <-
        genCostModels
    protocolUpdatePrices <-
        liftArbitrary genExecutionUnitPrices
    protocolUpdateMaxTxExUnits <-
        liftArbitrary genExecutionUnits
    protocolUpdateMaxBlockExUnits <-
        liftArbitrary genExecutionUnits
    protocolUpdateMaxValueSize <-
        liftArbitrary genNat
    protocolUpdateCollateralPercent <-
        liftArbitrary genNat
    protocolUpdateMaxCollateralInputs <-
        liftArbitrary genNat

    pure $ ProtocolParametersUpdate
        { Api.protocolUpdateProtocolVersion
        , Api.protocolUpdateDecentralization
        , Api.protocolUpdateExtraPraosEntropy
        , Api.protocolUpdateMaxBlockHeaderSize
        , Api.protocolUpdateMaxBlockBodySize
        , Api.protocolUpdateMaxTxSize
        , Api.protocolUpdateTxFeeFixed
        , Api.protocolUpdateTxFeePerByte
        , Api.protocolUpdateMinUTxOValue
        , Api.protocolUpdateStakeAddressDeposit
        , Api.protocolUpdateStakePoolDeposit
        , Api.protocolUpdateMinPoolCost
        , Api.protocolUpdatePoolRetireMaxEpoch
        , Api.protocolUpdateStakePoolTargetNum
        , Api.protocolUpdatePoolPledgeInfluence
        , Api.protocolUpdateMonetaryExpansion
        , Api.protocolUpdateTreasuryCut
        , Api.protocolUpdateUTxOCostPerWord
        , Api.protocolUpdateCostModels
        , Api.protocolUpdatePrices
        , Api.protocolUpdateMaxTxExUnits
        , Api.protocolUpdateMaxBlockExUnits
        , Api.protocolUpdateMaxValueSize
        , Api.protocolUpdateCollateralPercent
        , Api.protocolUpdateMaxCollateralInputs
        }

genUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genUpdateProposal era =
    case updateProposalSupportedInEra era of
        Nothing ->
            pure TxUpdateProposalNone
        Just supported ->
            oneof
                [ pure TxUpdateProposalNone
                , TxUpdateProposal supported
                  <$> ( UpdateProposal
                        <$> ( Map.fromList
                              <$> resize 3 (listOf ( (,)
                                    <$> genVerificationKeyHash AsGenesisKey
                                    <*> genProtocolParametersUpdate
                                  ))
                            )
                        <*> genEpochNo
                      )
                ]

genExtraScriptData :: CardanoEra era -> Gen (TxExtraScriptData era)
genExtraScriptData era =
    case scriptDataSupportedInEra era of
        Nothing ->
            pure TxExtraScriptDataNone
        Just supported ->
            oneof
                [ pure TxExtraScriptDataNone
                , TxExtraScriptData supported
                  <$> resize 3 (listOf genScriptData)
                ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
    txIns <- map (, BuildTxWith (KeyWitness KeyWitnessForSpending))
             <$> resize 3 (listOf1 genTxIn)
    txOuts <- resize 8 $ listOf1 $ genTxOut era
    txFee <- genTxFee era
    txValidityRange <- genTxValidityRange era
    txMetadata <- genTxMetadataInEra era
    txAuxScripts <- genTxAuxScripts era
    txWithdrawals <- genTxWithdrawals era
    txCertificates <- genTxCertificates era
    txUpdateProposal <- genUpdateProposal era
    txMintValue <- genTxMintValue era
    txScriptValidity <- genTxScriptValidity era
    txExtraScriptData <- BuildTxWith <$> genExtraScriptData era
    txExtraKeyWits <- genExtraKeyWitnesses era

    let
        txBody = TxBodyContent
            { Api.txIns
            , Api.txInsCollateral = TxInsCollateralNone
            , Api.txOuts
            , Api.txFee
            , Api.txValidityRange
            , Api.txMetadata
            , Api.txAuxScripts
            , Api.txExtraScriptData
            , Api.txExtraKeyWits
            , Api.txProtocolParams = BuildTxWith Nothing
            , Api.txWithdrawals
            , Api.txCertificates
            , Api.txUpdateProposal
            , Api.txMintValue
            , Api.txScriptValidity
            }

    let witnesses = collectTxBodyScriptWitnesses txBody
    if Set.null (languages witnesses)
    then do
        pparams <- BuildTxWith <$> liftArbitrary genProtocolParameters
        collateral <- genTxInsCollateral era
        pure $ txBody
            { Api.txProtocolParams = pparams
            , Api.txInsCollateral = collateral
            }
    else do
        pparams <-
            (BuildTxWith . Just) <$> genProtocolParametersWithAlonzoScripts
        collateral <-
            case collateralSupportedInEra era of
                Nothing -> pure TxInsCollateralNone
                Just supported -> TxInsCollateral supported
                    <$> resize 3 (listOf genTxIn)
        pure $ txBody
            { Api.txProtocolParams = pparams
            , Api.txInsCollateral = collateral
            }

    where
        languages :: [(a, AnyScriptWitness era)] -> Set AnyPlutusScriptVersion
        languages witnesses =
            Set.fromList
            [ AnyPlutusScriptVersion v
            | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
            ]

genTxBody :: IsCardanoEra era => CardanoEra era -> Gen (TxBody era)
genTxBody era = do
  res <- makeTransactionBody <$> genTxBodyContent era
  case res of
    Left err -> error (displayError err)
    Right txBody -> pure txBody

genWitnesses :: CardanoEra era -> TxBody era -> Gen [KeyWitness era]
genWitnesses era body =
    case cardanoEraStyle era of
        LegacyByronEra    -> do
            resize 3 $ listOf1 $ makeByronKeyWitness
                <$> genNetworkId
                <*> pure body
                <*> genSigningKey AsByronKey
        ShelleyBasedEra _ -> do
            let
                genShelley =
                    makeShelleyKeyWitness body <$> genShelleyWitnessSigningKey
                genBootstrap =
                    makeShelleyBootstrapWitness
                    <$> genWitnessNetworkIdOrByronAddress
                    <*> pure body
                    <*> genSigningKey AsByronKey

            bsWits  <- frequency
                [ (3, resize 3 $ listOf1 genBootstrap)
                , (1, pure [])
                ]
            keyWits <- frequency
                [ (3, resize 3 $ listOf1 genShelley)
                , (1, pure [])
                ]
            return $ bsWits ++ keyWits

genWitness :: CardanoEra era -> TxBody era -> Gen (KeyWitness era)
genWitness era body =
  case cardanoEraStyle era of
    LegacyByronEra    ->
        makeByronKeyWitness
            <$> genNetworkId
            <*> pure body
            <*> genSigningKey AsByronKey
    ShelleyBasedEra _ ->
      oneof [ makeShelleyBootstrapWitness
                  <$> genWitnessNetworkIdOrByronAddress
                  <*> pure body
                  <*> genSigningKey AsByronKey
            , makeShelleyKeyWitness body <$> genShelleyWitnessSigningKey
            ]

genTxInEra :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTxInEra era = do
  body <- genTxBody era
  makeSignedTransaction
    <$> genWitnesses era body
    <*> pure body

genTx :: Gen (InAnyCardanoEra Tx)
genTx =
    oneof [ InAnyCardanoEra ByronEra   <$> genTxInEra ByronEra
          , InAnyCardanoEra ShelleyEra <$> genTxInEra ShelleyEra
          , InAnyCardanoEra MaryEra    <$> genTxInEra MaryEra
          , InAnyCardanoEra AllegraEra <$> genTxInEra AllegraEra
          , InAnyCardanoEra AlonzoEra  <$> genTxInEra AlonzoEra
          ]
