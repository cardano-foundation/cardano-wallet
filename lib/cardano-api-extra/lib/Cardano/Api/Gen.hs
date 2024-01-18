{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move guards forward" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Api.Gen
    ( genAddressAny
    , genAddressAnyWithNetworkId
    , genAddressShelleyWithNetworkId
    , genAddressByronWithNetworkId
    , genAddressByron
    , genAddressInEra
    , genAddressShelley
    , genAlphaNum
    , genAssetIdNoAda
    , genAssetName
    , genByronKeyWitness
    , genCertIx
    , genCostModel
    , genCostModels
    , genEncodingBoundaryLovelace
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
    , genPlutusScript
    , genPolicyId
    , genPoolId
    , genProtocolParametersUpdate
    , genPtr
    , genRational
    , genRationalInt64
    , genScript
    , genScriptData
    , genHashableScriptData
    , shrinkScriptData
    , genScriptHash
    , genScriptInAnyLang
    , genScriptInEra
    , genScriptValidity
    , genScriptWitnessStake
    , genSeed
    , genShelleyHash
    , genShelleyWitnessSigningKey
    , genSignedQuantity
    , genSignedValue
    , genSigningKey
    , genSimpleScript
    , genSlotNo
    , genSlotNo32
    , genStakeAddress
    , genStakeAddressReference
    , genStakeCredential
    , genStakePoolMetadata
    , genStakePoolMetadataReference
    , genStakePoolParameters
    , genStakePoolRelay
    , genTtl
    , genTx
    , genTxAuxScripts
    , genTxBody
    , genTxBodyContent
    , genTxBodyForBalancing
    , genTxCertificate
    , genTxCertificates
    , genTxFee
    , genTxForBalancing
    , genTxId
    , genTxIn
    , genTxIndex
    , genTxInEra
    , genTxInsCollateral
    , genTxIx
    , genTxMetadata
    , genTxMetadataInEra
    , genTxMetadataValue
    , genTxMintValue
    , genTxOut
    , genTxOutDatum
    , genTxOutValue
    , genTxReturnCollateral
    , genTxScriptValidity
    , genTxTotalCollateral
    , genTxValidityLowerBound
    , genTxValidityRange
    , genTxValidityUpperBound
    , genTxWithdrawals
    , genUnsignedQuantity
    , genUpdateProposal
    , genValueForMinting
    , genValueForTxOut
    , genVerificationKey
    , genVerificationKeyHash
    , genWithdrawalInfo
    , genWitness
    , genWitnesses
    , genWitnessNetworkIdOrByronAddress
    , genWitnessStake
    , genValidProtocolVersion
    ) where

import Prelude

import Cardano.Api hiding
    ( txIns
    )
import Cardano.Api.Byron
    ( KeyWitness (ByronKeyWitness)
    , WitnessNetworkIdOrByronAddress (..)
    )
import Cardano.Api.Shelley
    ( Hash (..)
    , PlutusScript (..)
    , PlutusScriptOrReferenceInput (..)
    , PoolId
    , ProtocolParameters (..)
    , ReferenceScript (..)
    , SimpleScriptOrReferenceInput (..)
    , StakeCredential (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataReference (..)
    , StakePoolParameters (..)
    , StakePoolRelay (..)
    )
import Cardano.Ledger.Alonzo.Language
    ( Language (..)
    )
import Cardano.Ledger.Credential.Safe
    ( Ptr
    , SlotNo32 (..)
    , safePtr
    )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Shelley.API
    ( MIRPot (..)
    )
import Data.Aeson
    ( ToJSON (..)
    , (.=)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Coerce
    ( coerce
    )
import Data.Int
    ( Int64
    )
import Data.IntCast
    ( intCast
    )
import Data.IP
    ( IPv4
    , IPv6
    , fromHostAddress
    , fromHostAddress6
    )
import Data.List
    ( nub
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( isJust
    )
import Data.Maybe.Strict
    ( strictMaybeToMaybe
    )
import Data.Ratio
    ( Ratio
    , (%)
    )
import Data.Set
    ( Set
    )
import Data.String
    ( fromString
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    )
import Network.Socket
    ( PortNumber
    )
import Numeric.Natural
    ( Natural
    )
import System.Random
    ( Random
    )
import Test.Cardano.Chain.UTxO.Gen
    ( genVKWitness
    )
import Test.Cardano.Crypto.Gen
    ( genProtocolMagicId
    )
import Test.QuickCheck
    ( Gen
    , Large (..)
    , NonNegative (..)
    , Positive (..)
    , arbitrary
    , arbitraryASCIIChar
    , arbitraryBoundedIntegral
    , arbitrarySizedNatural
    , choose
    , chooseInt
    , chooseInteger
    , elements
    , frequency
    , infiniteListOf
    , liftArbitrary
    , liftShrink2
    , listOf
    , listOf1
    , oneof
    , scale
    , shrink
    , shrinkList
    , sized
    , vector
    , vectorOf
    )
import Test.QuickCheck.Extra
    ( GenSeed (..)
    , genSizeDefault
    , generateWith
    )
import Test.QuickCheck.Hedgehog
    ( hedgehog
    )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Api as Api
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Cardano.Ledger.Alonzo.PlutusScripts as Plutus

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The smallest quantity of lovelace that can appear in a transaction output's
--   value map.
--
-- In practice, the protocol parameters may require this value to be higher, so
-- this is an absolute minimum.
--
txOutMinLovelace :: Lovelace
txOutMinLovelace = 0

-- | The greatest quantity of lovelace that can appear in a transaction output's
--   value map.
--
-- In practice, this is limited by the total available supply of lovelace.
--
txOutMaxLovelace :: Lovelace
txOutMaxLovelace = 45_000_000_000_000_000

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genShelleyHash
    :: Gen (Crypto.Hash Crypto.Blake2b_256 Ledger.EraIndependentTxBody)
genShelleyHash = return . Crypto.castHash $ Crypto.hashWith CBOR.serialize' ()

genTxIn :: Gen TxIn
genTxIn = TxIn <$> genTxId <*> genTxIndex

genTxId :: Gen TxId
genTxId = TxId <$> genShelleyHash

genTxIndex :: Gen TxIx
genTxIndex = oneof
    [ (TxIx . intCast) <$> (arbitrary @Word16)
      -- FIXME: cardano-api uses a full Word here, yet the ledger uses Word16
      -- and we'll fail to construct a tx unless we constrain ourselves to
      -- Word16 here.
    , TxIx . fromIntegral . getNonNegative <$> (arbitrary @(NonNegative Int))
    -- For some bias towards small values
    ]

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era = error "TODO conway: genTxInsCollateral"
    -- case collateralSupportedInEra era of
    --   Nothing        -> pure TxInsCollateralNone
    --   Just supported -> oneof
    --                       [ pure TxInsCollateralNone
    --                       , TxInsCollateral supported
    --                         <$> scale (`div` 3) (listOf genTxIn)
    --                       ]

genSlotNo :: Gen SlotNo
genSlotNo = do
    boundary <- genBoundary
    frequency
        [ (20, pure $ SlotNo boundary)
        , (20, pure $ SlotNo (maxBound @Word64 - boundary) )
        , (60, SlotNo <$> arbitrary @Word64)
        ]
  where
    genBoundary = choose (0, 10_000)

genSlotNo32 :: Gen SlotNo32
genSlotNo32 = do
    offset <- genOffset
    frequency
        [ (20, pure $ SlotNo32 offset)
        , (20, pure $ SlotNo32 (maxBound @Word32 - offset) )
        , (60, SlotNo32 <$> arbitrary @Word32)
        ]
  where
    genOffset = choose (0, 10_000)

genLovelace :: Gen Lovelace
genLovelace = frequency
    [ (10, Lovelace . intCast . getNonNegative @Int <$> arbitrary)
    , (50, choose (1_000_000, 1_000_000_000))
    , (10, choose (txOutMinLovelace, txOutMaxLovelace))
    , (30, genEncodingBoundaryLovelace)
    ]

genEncodingBoundaryLovelace :: Gen Lovelace
genEncodingBoundaryLovelace = do
    -- https://json.nlohmann.me/features/binary_formats/cbor/
    -- Generate a point near a boundary
    -- However, the three first ones are below the minimum utxo value on
    -- mainnet, and are less useful to generate (in that context).
    boundary <- frequency
        [ ( 1, pure            24)
        , ( 1, pure           256) -- 2^ 8
        , ( 8, pure        65_536) -- 2^16
        , (90, pure 4_294_967_296) -- 2^32
        ]

    offset <- frequency
        [ (1, choose (-10, 10))

        -- Either offset by -1 (just below boundary), or 0 (just above boundary)
        , (1, choose (-1, 0))

        -- Offset by values close to common fee values, in both the positive
        -- and negative direction, with the hope that this helps find
        -- corner-cases.
        , (1, choose (-220_000, -150_000))
        , (1, choose (150_000, 220_000))

        , (1, choose (-1_000_000, 1_000_000))
        ]
    pure $ Lovelace <$> max 0 $ boundary + offset

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee = error "TODO conway: genTxFee"
-- genTxFee era =
--   case txFeesExplicitInEra era of
--     Left  implicit -> pure (TxFeeImplicit implicit)
--     Right explicit -> TxFeeExplicit explicit <$> genLovelace

genTtl :: Gen SlotNo
genTtl = genSlotNo

genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound = error "TODO conway: genTxValidityLowerBound"
-- genTxValidityLowerBound era =
--   case validityLowerBoundSupportedInEra era of
--     Nothing        -> pure TxValidityNoLowerBound
--     Just supported -> TxValidityLowerBound supported <$> genTtl

genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound = error "TODO conway: genTxValidityUpperBound"
-- genTxValidityUpperBound era =
--   case (validityUpperBoundSupportedInEra era,
--        validityNoUpperBoundSupportedInEra era) of
--     (Just supported, _) ->
--       TxValidityUpperBound supported <$> genTtl

--     (Nothing, Just supported) ->
--       pure (TxValidityNoUpperBound supported)

--     (Nothing, Nothing) ->
--       error "genTxValidityUpperBound: unexpected era support combination"

genTxValidityRange
  :: CardanoEra era
  -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
  (,)
    <$> genTxValidityLowerBound era
    <*> genTxValidityUpperBound era

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity = error "TODO conway: genTxScriptValidity"
-- genTxScriptValidity era = case txScriptValiditySupportedInCardanoEra era of
--   Nothing -> pure TxScriptValidityNone
--   Just witness -> TxScriptValidity witness <$> genScriptValidity

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
    :: (Key keyrole, HasTypeProxy keyrole)
    => AsType keyrole
    -> Gen (VerificationKey keyrole)
genVerificationKey roletoken = getVerificationKey <$> genSigningKey roletoken

genVerificationKeyHash :: (Key keyrole, HasTypeProxy keyrole) => AsType keyrole -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
  verificationKeyHash <$> genVerificationKey roletoken

genExtraKeyWitnesses :: CardanoEra era -> Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses = error "TODO conway: genExtraKeyWitnesses"
-- genExtraKeyWitnesses era =
--     case extraKeyWitnessesSupportedInEra era of
--         Nothing -> pure TxExtraKeyWitnessesNone
--         Just supported -> oneof
--             [ pure TxExtraKeyWitnessesNone
--             , TxExtraKeyWitnesses supported
--               <$> scale (`div` 3) (listOf (genVerificationKeyHash AsPaymentKey))
--             ]

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral era = error "TODO conway: genTxTotalCollateral"
-- genTxTotalCollateral era =
--     case totalAndReturnCollateralSupportedInEra era of
--         Nothing -> pure TxTotalCollateralNone
--         Just supported -> oneof
--             [ pure TxTotalCollateralNone
--             , TxTotalCollateral supported <$> genLovelace
--             ]

genTxReturnCollateral :: CardanoEra era -> Gen (TxReturnCollateral ctx era)
genTxReturnCollateral era = error "TODO conway: genTxReturnCollateral"
-- genTxReturnCollateral era =
--     case totalAndReturnCollateralSupportedInEra era of
--         Nothing -> pure TxReturnCollateralNone
--         Just supported -> oneof
--             [ pure TxReturnCollateralNone
--             , TxReturnCollateral supported <$> genTxOut era
--             ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript _ =
    -- We make no attempt to create a valid script
    PlutusScriptSerialised . SBS.toShort <$> arbitrary

genPlutusScriptOrReferenceInput
    :: PlutusScriptVersion lang
    -> Gen (PlutusScriptOrReferenceInput lang)
genPlutusScriptOrReferenceInput lang =
    -- TODO add proper generator, perhaps as part of ADP-1655
    PScript <$> genPlutusScript lang

genSimpleScript :: Gen SimpleScript
genSimpleScript =
    sized genTerm
  where
    genTerm 0 = oneof nonRecursive
    genTerm n = frequency
        [ (3, oneof (recursive n))
        , (1, oneof nonRecursive)
        ]

    -- Non-recursive generators
    nonRecursive =
      [ RequireSignature . verificationKeyHash <$> genVerificationKey AsPaymentKey
      , RequireTimeBefore <$> genSlotNo
      , RequireTimeAfter <$> genSlotNo
      ]

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

genReferenceInput :: Gen TxIn
genReferenceInput = genTxIn

genSimpleScriptOrReferenceInput
    :: Gen (SimpleScriptOrReferenceInput lang)
genSimpleScriptOrReferenceInput =
    oneof [ SScript
            <$> genSimpleScript
          , SReferenceScript
            <$> genReferenceInput
            <*> liftArbitrary genScriptHash
          ]

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript = \case
    SimpleScriptLanguage -> SimpleScript <$> genSimpleScript
    PlutusScriptLanguage lang -> PlutusScript lang <$> genPlutusScript lang

genScriptInAnyLang :: Maybe (CardanoEra era) -> Gen ScriptInAnyLang
genScriptInAnyLang optionalEra =
    oneof
      [ ScriptInAnyLang lang <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , case optionalEra of
          Nothing -> True
          Just era ->
            error "TODO conway: genScriptInAnyLang"
            -- isJust (scriptLanguageSupportedInEra era lang)
      ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era =
    oneof
      [ ScriptInEra langInEra <$> genScript lang
      | AnyScriptLanguage lang <- [minBound..maxBound]
      , Just langInEra <- error "TODO conway: genScriptInEra"
        -- [scriptLanguageSupportedInEra era lang]
      ]

genScriptHash :: Gen ScriptHash
genScriptHash = do
    ScriptInAnyLang _ script <- genScriptInAnyLang Nothing
    return (hashScript script)

genAssetName :: Gen AssetName
genAssetName =
  frequency
    -- mostly from a small number of choices, so we get plenty of repetition
    [ (9, elements ["", "a", "b", "c"])
    , (1, AssetName . fromString <$> (scale (min 32) (listOf genAlphaNum)))
    , (1, AssetName . fromString <$> (vectorOf 1 genAlphaNum))
    , (1, AssetName . fromString <$> (vectorOf 32 genAlphaNum))
    ]

genAlphaNum :: Gen Char
genAlphaNum = elements
    "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genPolicyId :: Gen PolicyId
genPolicyId = frequency
      -- Mostly from a small number of choices, so we get plenty of repetition.
      --
      -- And because of the additional choice of asset name we repeat ourselves
      -- even more here.
    [ (80, pure $ fromString ('a' : replicate 55 '0'))
    , (18, elements [ fromString (x : replicate 55 '0') | x <- ['a'..'c'] ])
       -- and some from the full range of the type
    , (2, PolicyId <$> genScriptHash)
    ]

genAssetIdNoAda :: Gen AssetId
genAssetIdNoAda = AssetId <$> genPolicyId <*> genAssetName

-- | Generate a positive or negative quantity.
genSignedQuantity :: Gen Quantity
genSignedQuantity = do
    (Large (n :: Int64)) <- arbitrary
    pure $ fromIntegral n

-- | Generate a 'Value' suitable for usage in a transaction output, i.e. any
-- asset ID and a positive quantity.
genValueForTxOut :: Gen Value
genValueForTxOut = do
    assetIds <- oneof
        [ nub <$> scale (`div` 4) (listOf genAssetIdNoAda)
        , pure []
        ]
    assetQuantities <- infiniteListOf genUnsignedQuantity
    ada <- fromInteger . unLovelace <$> genLovelace
    return $ valueFromList $ (AdaAssetId, ada) : zip assetIds assetQuantities
  where
    unLovelace (Lovelace l) = l

-- | Generate a 'Value' which could represent the balance of a partial
-- transaction, where both ada and other assets can be included, and quantities
-- can be both positive and negative.
genSignedValue :: Gen Value
genSignedValue = do
    assetIds <- oneof
        [ nub <$> scale (`div` 4) (listOf genAssetIdNoAda)
        , pure []
        ]
    assetQuantities <- infiniteListOf genSignedQuantity
    ada <- fromInteger . unLovelace <$> oneof
        [ genLovelace
        , negate <$> genLovelace
        ]
    return $ valueFromList $ (AdaAssetId, ada) : zip assetIds assetQuantities
  where
    unLovelace (Lovelace l) = l

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen Value
genValueForMinting =
  valueFromList <$> listOf ((,) <$> genAssetIdNoAda <*> genSignedQuantity)

genTxMintValue :: forall era. CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era = error "TODO conway: genTxMintValue"
-- genTxMintValue era =
--   case multiAssetSupportedInEra era of
--     Left _ -> pure TxMintNone
--     Right supported -> do
--       let
--           scriptWitnessGenerators :: [Gen (ScriptWitness WitCtxMint era)]
--           scriptWitnessGenerators =
--               [ genScriptWitnessMint langInEra
--               | AnyScriptLanguage lang <- [minBound..maxBound]
--               , Just langInEra <- [scriptLanguageSupportedInEra era lang]
--               ]
--       oneof
--         [ pure TxMintNone
--         , TxMintValue supported
--           <$> genValueForMinting
--           <*> ( (BuildTxWith . Map.fromList)
--                 <$> scale (`div` 3) (listOf ( (,)
--                              <$> genPolicyId
--                              <*> oneof scriptWitnessGenerators
--                            ))
--               )
--         ]

genNetworkMagic :: Gen NetworkMagic
genNetworkMagic = do
    (Large n) <- arbitrary
    pure $ NetworkMagic n

genNetworkId :: Gen NetworkId
genNetworkId =
    frequency
        [ (95, pure Mainnet)
        , (5, Testnet <$> genNetworkMagic)
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

genHashableScriptData :: Gen HashableScriptData
genHashableScriptData = do
  sd <- genScriptData
  case deserialiseFromCBOR AsHashableScriptData $ serialiseToCBOR sd of
    Left e -> error $ "genHashableScriptData: " <> show e
    Right r -> return r

genScriptData :: Gen ScriptData
genScriptData =
    sized genTerm
    where
        genTerm 0 = oneof nonRecursive
        genTerm n = frequency
            [ (1, recursive n)
            , (3, oneof nonRecursive)
            ]

        -- Non-recursive generators
        nonRecursive =
            [ do
                 (Large (n :: Int64)) <- arbitrary
                 pure $ ScriptDataNumber $ fromIntegral n
            , do
                 n <- choose (0, 64)
                 (ScriptDataBytes . BS.pack) <$> vector n
            ]

        recursive n = do
            k <- choose (0, n)
            let smallerGen = genTerm (n `div` (max k 1))
            oneof
                [ ScriptDataList
                    <$> vectorOf k smallerGen
                , ScriptDataMap
                    <$> vectorOf k ((,) <$> smallerGen <*> smallerGen)
                , ScriptDataConstructor
                    <$> genConstructorIx
                    <*> vectorOf k smallerGen
                ]

        -- NOTE: Negative values would trigger "Impossible" errors to be
        -- thrown. This seems expected and fine:
        -- https://github.com/IntersectMBO/cardano-ledger/pull/2333#issuecomment-864159342
        genConstructorIx :: Gen Integer
        genConstructorIx = frequency
            [ (45, arbitrarySizedNatural)
            , (40, choose (0, 5))
            , (5, fromIntegral <$> arbitrary @Word64)
            ]

shrinkScriptData :: ScriptData -> [ScriptData]
shrinkScriptData s = aggressivelyShrink s ++ case s of
    ScriptDataList l ->
        ScriptDataList <$> shrinkList shrinkScriptData l
    ScriptDataMap m ->
        ScriptDataMap <$> shrinkList (shrinkTuple shrinkScriptData) m
    ScriptDataNumber n -> ScriptDataNumber <$> shrink n
    ScriptDataBytes bs -> ScriptDataBytes <$> shrink bs
    ScriptDataConstructor n l -> uncurry ScriptDataConstructor
        <$> liftShrink2 shrink (shrinkList shrinkScriptData) (n, l)
  where
    aggressivelyShrink = \case
        ScriptDataList l -> l
        ScriptDataMap m -> map snd m
        ScriptDataNumber _ -> []
        ScriptDataBytes _ -> []
        ScriptDataConstructor _ l -> l

    shrinkTuple :: (a -> [a]) -> (a, a) -> [(a, a)]
    shrinkTuple f = liftShrink2 f f

genExecutionUnits :: Gen ExecutionUnits
genExecutionUnits = do
    Large steps <- arbitrary
    Large mem <- arbitrary
    let fromWord64 = fromIntegral @Word64
    pure $ ExecutionUnits (fromWord64 steps) (fromWord64 mem)

genTxWithdrawals :: CardanoEra era -> Gen (TxWithdrawals BuildTx era)
genTxWithdrawals era = error "TODO conway: genTxWithdrawals"
-- genTxWithdrawals era =
--   case withdrawalsSupportedInEra era of
--     Nothing ->
--         pure TxWithdrawalsNone
--     Just supported -> do
--         frequency
--           [ ( 1 , pure TxWithdrawalsNone )
--           , ( 1 , pure $ TxWithdrawals supported [] )
--           , ( 3 , TxWithdrawals supported
--                   <$> scale (`div` 3) (listOf (genWithdrawalInfo era))
--             )
--           ]

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
       , Just langInEra <-
       error "TODO conway: genWitnessStake"
            -- [scriptLanguageSupportedInEra era lang]
       ]

genWitnessSpend :: CardanoEra era -> Gen (Witness WitCtxTxIn era)
genWitnessSpend era = oneof $
    [ pure $ KeyWitness KeyWitnessForSpending ]
    <> [ ScriptWitness ScriptWitnessForSpending
         <$> genScriptWitnessSpend langInEra
       | AnyScriptLanguage lang <- [minBound..maxBound]
       , Just langInEra <-
            error "TODO conway: genWitnessSpend"
            -- [scriptLanguageSupportedInEra era lang]
       ]

genScriptWitnessMint
    :: ScriptLanguageInEra lang era
    -> Gen (ScriptWitness WitCtxMint era)
genScriptWitnessMint langEra =
    case languageOfScriptLanguageInEra langEra of
        SimpleScriptLanguage ->
            SimpleScriptWitness langEra <$> genSimpleScriptOrReferenceInput
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScriptOrReferenceInput ver
            <*> pure NoScriptDatumForMint
            <*> genHashableScriptData
            <*> genExecutionUnits

genScriptWitnessStake
    :: ScriptLanguageInEra lang era
    -> Gen (ScriptWitness WitCtxStake era)
genScriptWitnessStake langEra =
    case languageOfScriptLanguageInEra langEra of
        SimpleScriptLanguage ->
            SimpleScriptWitness langEra <$> genSimpleScriptOrReferenceInput
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScriptOrReferenceInput ver
            <*> pure NoScriptDatumForStake
            <*> genHashableScriptData
            <*> genExecutionUnits

genScriptWitnessSpend
    :: ScriptLanguageInEra lang era
    -> Gen (ScriptWitness WitCtxTxIn era)
genScriptWitnessSpend langEra =
    case languageOfScriptLanguageInEra langEra of
        SimpleScriptLanguage ->
            SimpleScriptWitness langEra <$> genSimpleScriptOrReferenceInput
        (PlutusScriptLanguage ver) ->
            PlutusScriptWitness langEra ver
            <$> genPlutusScriptOrReferenceInput ver
            <*> (ScriptDatumForTxIn <$> genHashableScriptData)
            <*> genHashableScriptData
            <*> genExecutionUnits

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era = error "TODO conway: genTxAuxScripts"
-- genTxAuxScripts era =
--   case auxScriptsSupportedInEra era of
--     Nothing -> pure TxAuxScriptsNone
--     Just supported ->
--         frequency
--         [ (1, pure TxAuxScriptsNone)
--         , (3, TxAuxScripts supported
--               <$> scale (`div` 3) (listOf (genScriptInEra era)))
--         ]

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era = error "TODO conway: genTxMetadataInEra"
-- genTxMetadataInEra era =
--   case txMetadataSupportedInEra era of
--     Nothing -> pure TxMetadataNone
--     Just supported ->
--         oneof
--             [ pure TxMetadataNone
--             , TxMetadataInEra supported <$> genTxMetadata
--             ]

genTxMetadata :: Gen TxMetadata
genTxMetadata =
    fmap (TxMetadata . Map.fromList) $ do
        listOf
            ((,) <$> (getLarge <$> arbitrary)
                 <*> genTxMetadataValue)

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    sized $ \sz ->
        frequency
            [ (2, TxMetaNumber <$> genTxMetaNumber)
            , (2, TxMetaBytes  <$> genTxMetaBytes)
            , (2, TxMetaText   <$> genTxMetaText)
            , (sz `div` 4,
                  TxMetaList <$> scale (`div` 4) genTxMetaList)
            , (sz `div` 4,
                  TxMetaMap <$> scale (`div` 4) genTxMetaMap)
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
genPtr = safePtr <$> genSlotNo32 <*> genTxIx <*> genCertIx

genTxIx :: Gen Ledger.TxIx
genTxIx = Ledger.TxIx <$> arbitrary

genCertIx :: Gen Ledger.CertIx
genCertIx = Ledger.CertIx <$> arbitrary

genStakeAddressReference :: Gen StakeAddressReference
genStakeAddressReference =
  oneof
    [ StakeAddressByValue <$> genStakeCredential
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

genAddressAnyWithNetworkId :: Gen NetworkId -> Gen AddressAny
genAddressAnyWithNetworkId genNetworkId' =
    oneof
        [ AddressByron
            <$> genAddressByronWithNetworkId genNetworkId'
        , AddressShelley
            <$> genAddressShelleyWithNetworkId genNetworkId'
        ]

genAddressByronWithNetworkId :: Gen NetworkId -> Gen (Address ByronAddr)
genAddressByronWithNetworkId genNetworkId' =
    makeByronAddress
        <$> genNetworkId'
        <*> genVerificationKey AsByronKey

genAddressShelleyWithNetworkId :: Gen NetworkId -> Gen (Address ShelleyAddr)
genAddressShelleyWithNetworkId genNetworkId' =
    makeShelleyAddress
        <$> genNetworkId'
        <*> genPaymentCredential
        <*> genStakeAddressReference

genAddressAny :: Gen AddressAny
genAddressAny = genAddressAnyWithNetworkId genNetworkId

genAddressByron :: Gen (Address ByronAddr)
genAddressByron = genAddressByronWithNetworkId genNetworkId

genAddressShelley :: Gen (Address ShelleyAddr)
genAddressShelley = genAddressShelleyWithNetworkId genNetworkId

genAddressInEra :: CardanoEra era -> Gen (AddressInEra era)
genAddressInEra era = error "TODO conway: genAddressInEra"

-- genAddressInEra era =
--   case cardanoEraStyle era of
--     LegacyByronEra ->
--       byronAddressInEra <$> genAddressByron

--     ShelleyBasedEra _ ->
--       oneof
--         [ byronAddressInEra   <$> genAddressByron
--         , shelleyAddressInEra <$> genAddressShelley
--         ]

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = do
    n <- arbitrary @Int
    frequency
        [ (30, pure $ abs $ fromIntegral n)
        , (30, fromIntegral @Integer <$> choose (0, 1_000_000_000_000))
        , (30, fromIntegral <$> arbitrary @Word64)
        ]

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era = error "TODO conway: genTxOutValue"
-- genTxOutValue era =
--   case multiAssetSupportedInEra era of
--     Left adaOnlyInEra     -> TxOutAdaOnly adaOnlyInEra <$> genLovelace
--     Right multiAssetInEra ->
--         error "TODO conway: genTxOutValue"
--         -- TxOutValue multiAssetInEra <$> genValueForTxOut

genTxOut :: CardanoEra era -> Gen (TxOut ctx era)
genTxOut era =
  TxOut <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatum era
        <*> genReferenceScript era

genTxOutDatum :: CardanoEra era -> Gen (TxOutDatum ctx era)
genTxOutDatum era = error "TODO conway: genTxOutDatum"
-- genTxOutDatum era =
--     case scriptDataSupportedInEra era of
--     Nothing -> pure TxOutDatumNone
--     Just supported -> oneof
--         [ pure TxOutDatumNone
--         , TxOutDatumHash supported <$> genHashScriptData
--         ]

genReferenceScript :: CardanoEra era -> Gen (ReferenceScript era)
genReferenceScript era = error "TODO conway: genReferenceScript"
-- genReferenceScript era = case refInsScriptsAndInlineDatsSupportedInEra era of
--     Nothing -> pure ReferenceScriptNone
--     Just supported -> oneof
--         [ pure ReferenceScriptNone
--         , ReferenceScript supported <$> genScriptInAnyLang (Just era)
--         ]

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
-- https://github.com/IntersectMBO/cardano-ledger/pull/2330
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
genCostModel = do
    let costModelParams = Alonzo.getCostModelParams Plutus.testingCostModelV1
    eCostModel <- Alonzo.mkCostModel
        <$> genPlutusLanguage
        <*> mapM (const $ chooseInteger (0, 5_000)) costModelParams
    case eCostModel of
        Left err -> error $ "genCostModel: " ++ show err
        Right cModel -> return . CostModel $ Alonzo.getCostModelParams cModel

genPlutusLanguage :: Gen Language
genPlutusLanguage = elements [PlutusV1, PlutusV2]

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

-- | Dummy value suitable for being included in the pre-image of the script
-- integrity hash.
{-# NOINLINE protocolParametersForHashing #-}
protocolParametersForHashing :: ProtocolParameters
protocolParametersForHashing =
    generateWith (GenSeed 0) genSizeDefault
        genRecentEraProtocolParameters

genValidProtocolVersion :: Gen (Natural, Natural)
genValidProtocolVersion = do
    major <- fromIntegral @Int <$> choose (0, 9)
    minor <- genNat
    pure (major, minor)

-- | Generates a set of protocol parameters for a recent era.
--
-- Uses 'Just' as necessary to be convertible to @Ledger.PParams era@
-- for 'IsRecentEra' eras, and keep our tests from throwing exceptions.
genRecentEraProtocolParameters :: Gen ProtocolParameters
genRecentEraProtocolParameters = error "TODO conway: genRecentEraProtocolParameters"
-- genRecentEraProtocolParameters = ProtocolParameters
--     <$> genValidProtocolVersion
--     <*> (Just <$> genRational)
--     <*> liftArbitrary genPraosNonce
--     <*> genNat
--     <*> genNat
--     <*> genNat
--     <*> genLovelace
--     <*> genLovelace
--     <*> liftArbitrary genLovelace
--     <*> genLovelace
--     <*> genLovelace
--     <*> genLovelace
--     <*> genEpochNo
--     <*> genNat
--     <*> genRationalInt64
--     <*> genRational
--     <*> genRational
--     <*> (Just <$> genLovelace)
--     <*> genCostModels
--     <*> (Just <$> genExecutionUnitPrices)
--     <*> (Just <$> genExecutionUnits)
--     <*> (Just <$> genExecutionUnits)
--     <*> (Just <$> genNat)
--     <*> (Just <$> genNat)
--     <*> (Just <$> genNat)
--     <*> (Just <$> genLovelace)

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

genMIRTarget :: a
genMIRTarget = error "TODO conway: genMIRTarget"
-- genMIRTarget :: Gen MIRTarget
-- genMIRTarget =
--     oneof
--         [ StakeAddressesMIR
--           <$> scale (`div` 3) (listOf ((,) <$> genStakeCredential <*> genLovelace))
--         , SendToReservesMIR <$> genLovelace
--         , SendToTreasuryMIR <$> genLovelace
--         ]

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
            -- There is a overall limit of 512 bytes for metadata
            n <- chooseInt (0, 64)
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
    relay <- genLedgerStakePoolRelay
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

    -- See https://github.com/IntersectMBO/cardano-ledger-1-tech-writing-tweaks/blob/2de173e8574ab079c9e18013d7906c20a70a7251/eras/shelley/test-suite/src/Test/Cardano/Ledger/Shelley/Serialisation/Generators/Genesis.hs#L113
    genLedgerStakePoolRelay :: Gen Ledger.StakePoolRelay
    genLedgerStakePoolRelay = oneof
        [ Ledger.SingleHostAddr
            <$> genStrictMaybe genPort
            <*> genStrictMaybe genIPv4
            <*> genStrictMaybe genIPv6
        , Ledger.SingleHostName
            <$> genStrictMaybe genPort
            <*> genDnsName
        , Ledger.MultiHostName
            <$> genDnsName
        ]

    genDnsName :: Gen Ledger.DnsName
    genDnsName = do
        txtLength <- choose (1, 63)
        txt <- T.pack <$> vectorOf txtLength arbitraryASCIIChar
        case Ledger.textToDns txt of
            Nothing -> error "wrong generator for DnsName"
            Just dns -> return dns

    genIPv4 :: Gen IPv4
    genIPv4 = fromHostAddress <$> arbitraryBoundedIntegral

    genIPv6 :: Gen IPv6
    genIPv6 = do
        w1 <- arbitraryBoundedIntegral
        w2 <- arbitraryBoundedIntegral
        w3 <- arbitraryBoundedIntegral
        w4 <- arbitraryBoundedIntegral
        pure $ fromHostAddress6 (w1, w2, w3, w4)

    genPort :: Gen Ledger.Port
    genPort = Ledger.Port <$> arbitraryBoundedIntegral

    genStrictMaybe :: Gen a -> Gen (Ledger.StrictMaybe a)
    genStrictMaybe gen = Ledger.maybeToStrictMaybe <$> liftArbitrary gen

genStakePoolParameters :: Gen StakePoolParameters
genStakePoolParameters =
    StakePoolParameters
    <$> genPoolId
    <*> genVerificationKeyHash AsVrfKey
    <*> genLovelace
    <*> genRational
    <*> genStakeAddress
    <*> genLovelace
    <*> scale (`div` 3) (listOf (genVerificationKeyHash AsStakeKey))
    <*> scale (`div` 3) (listOf genStakePoolRelay)
    <*> liftArbitrary genStakePoolMetadataReference

-- genTxCertificate :: Gen Certificate
genTxCertificate = error "TODO conway: genTxCertificate"
genTxCertificate :: Gen a
-- genTxCertificate =
--     oneof
--         [ StakeAddressRegistrationCertificate <$> genStakeCredential
--         , StakeAddressDeregistrationCertificate <$> genStakeCredential
--         , StakeAddressPoolDelegationCertificate <$> genStakeCredential <*> genPoolId
--         , StakePoolRegistrationCertificate <$> genStakePoolParameters
--         , StakePoolRetirementCertificate <$> genPoolId <*> genEpochNo
--         , GenesisKeyDelegationCertificate
--           <$> genVerificationKeyHash AsGenesisKey
--           <*> genVerificationKeyHash AsGenesisDelegateKey
--           <*> genVerificationKeyHash AsVrfKey
--         , error "TODO conway: genTxCertificate: MIRCertificate"
--         -- , do
--         --       target <- genMIRTarget
--         --       pot <- case target of
--         --           SendToTreasuryMIR _ -> pure ReservesMIR
--         --           SendToReservesMIR _ -> pure TreasuryMIR
--         --           _ -> genMIRPot
--         --       pure $ MIRCertificate pot target
--         ]

genTxCertificates :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era = error "TODO conway: genTxCertificates"
-- genTxCertificates era =
--     case certificatesSupportedInEra era of
--         Nothing ->
--             pure TxCertificatesNone
--         Just supported ->
--             oneof
--                 [ pure TxCertificatesNone
--                 , TxCertificates supported
--                   <$> scale (`div` 3) (listOf genTxCertificate)
--                   <*> ( (BuildTxWith . Map.fromList)
--                         <$> scale (`div` 3) (listOf ( (,)
--                                      <$> genStakeCredential
--                                      <*> genWitnessStake era
--                                    ))
--                       )
--                 ]

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate = do
    protocolUpdateProtocolVersion <-
        liftArbitrary genValidProtocolVersion
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
        liftArbitrary genLovelace
    protocolUpdateTxFeePerByte <-
        liftArbitrary genLovelace
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
    -- protocolUpdateUTxOCostPerWord <- liftArbitrary genLovelace
    protocolUpdateUTxOCostPerByte <-
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
        -- , Api.protocolUpdateUTxOCostPerWord
        , Api.protocolUpdateUTxOCostPerByte
        , Api.protocolUpdateCostModels
        , Api.protocolUpdatePrices
        , Api.protocolUpdateMaxTxExUnits
        , Api.protocolUpdateMaxBlockExUnits
        , Api.protocolUpdateMaxValueSize
        , Api.protocolUpdateCollateralPercent
        , Api.protocolUpdateMaxCollateralInputs
        }

genUpdateProposal :: CardanoEra era -> Gen (TxUpdateProposal era)
genUpdateProposal era = error "TODO conway: genUpdateProposal"
-- genUpdateProposal era =
--     case updateProposalSupportedInEra era of
--         Nothing ->
--             pure TxUpdateProposalNone
--         Just supported ->
--             frequency
--                 [ (95, pure TxUpdateProposalNone)
--                 , (5, TxUpdateProposal supported
--                   <$> ( UpdateProposal
--                         <$> ( Map.fromList
--                               <$> scale (`div` 3) (listOf ( (,)
--                                     <$> genVerificationKeyHash AsGenesisKey
--                                     <*> genProtocolParametersUpdate
--                                   ))
--                             )
--                         <*> genEpochNo
--                       )
--                     )
--                 ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = do
    txIns <- scale (`div` 3) $ do
        txIns <- listOf1 genTxIn
        ctxs <- vectorOf (length txIns) (genWitnessSpend era)
        pure $ zip txIns (BuildTxWith <$> ctxs)
    txOuts <- scale (`div` 3) $ listOf1 $ genTxOut era
    txFee <- genTxFee era
    txValidityRange <- genTxValidityRange era
    txMetadata <- genTxMetadataInEra era
    txAuxScripts <- genTxAuxScripts era
    txWithdrawals <- genTxWithdrawals era
    txCertificates <- genTxCertificates era
    txUpdateProposal <- genUpdateProposal era
    txMintValue <- genTxMintValue era
    txScriptValidity <- genTxScriptValidity era
    txExtraKeyWits <- genExtraKeyWitnesses era
    txTotalCollateral <- genTxTotalCollateral era
    txReturnCollateral <- genTxReturnCollateral era

    let
        txBody = error "TODO conway: genTxBodyContent"
        -- txBody = TxBodyContent
        --     { Api.txIns
        --     , Api.txOuts
        --     -- NOTE: We are adding collateral at a later step, despite only
        --     -- generating @TxInsCollateralNone@ here. This seems to be because
        --     -- the generation currently is dependent on
        --     -- @collectTxBodyScriptWitnesses txBody@.
        --     , Api.txInsCollateral = TxInsCollateralNone

        --     -- TODO add proper generator, perhaps as part of ADP-1655
        --     , Api.txInsReference = TxInsReferenceNone

        --     , Api.txTotalCollateral
        --     , Api.txReturnCollateral
        --     , Api.txFee
        --     -- , Api.txValidityRange
        --     , Api.txMetadata
        --     , Api.txAuxScripts
        --     , Api.txExtraKeyWits
        --     , Api.txProtocolParams = BuildTxWith Nothing
        --     , Api.txWithdrawals
        --     , Api.txCertificates
        --     , Api.txUpdateProposal
        --     , Api.txMintValue
        --     , Api.txScriptValidity
        --     }

    let witnesses = collectTxBodyScriptWitnesses txBody
        pparams = BuildTxWith $ Just protocolParametersForHashing
    -- No use of a script language means no need for collateral
    if Set.null (error "TODO conway: languages")
        -- (languages witnesses)
        then do
            collateral <- genTxInsCollateral era
            pure txBody
                { Api.txProtocolParams =
                    error "TODO conway: genTxBodyContent"
                    -- pparams
                , Api.txInsCollateral = collateral
                }
        else do
            collateral <-
                error "TODO conway: genTxBodyContent"
                -- case collateralSupportedInEra era of
                --     Nothing -> pure TxInsCollateralNone
                --     Just supported -> TxInsCollateral supported <$> frequency
                --         [ (95, return [])
                --         , (5, listOf genTxIn)
                --         ]
            pure txBody
                { Api.txProtocolParams =
                    error "TODO conway: genTxBodyContent"
                    -- pparams
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
genTxBody era = error "TODO conway: genTxBody"
-- genTxBody era = do
--   res <- createAndValidateTransactionBody <$> genTxBodyContent era
--   case res of
--     Left err -> error (displayError err)
--     Right txBody -> pure txBody

-- | Similar to 'genTxBody', but with a distribution better suitable for testing
-- balancing.
genTxBodyForBalancing :: IsCardanoEra era => CardanoEra era -> Gen (TxBody era)
genTxBodyForBalancing era = do
    res <- error "TODO conway: genTxBodyForBalancing"
    -- res <- createAndValidateTransactionBody <$> genStrippedContent
    case res of
      Left err ->
        error "TODO conway: genTxBodyForBalancing"
        -- error (displayError err)
      Right txBody -> pure txBody
  where
    genStrippedContent = do
        content <- genTxBodyContent era
        genShouldStrip >>= \case
            True -> pure $ content
                { txInsCollateral = case txInsCollateral content of
                    TxInsCollateralNone -> TxInsCollateralNone
                    TxInsCollateral colInEra _ -> TxInsCollateral colInEra []
                }
            False -> pure content
    genShouldStrip = frequency [ (90, pure True), (10, pure False) ]

genWitnesses :: CardanoEra era -> TxBody era -> Gen [KeyWitness era]
genWitnesses = error "TODO conway: genWitnesses"
-- genWitnesses era body =
--     case cardanoEraStyle era of
--         LegacyByronEra    -> do
--             scale (`div` 3) $ listOf1 $ makeByronKeyWitness
--                 <$> genNetworkId
--                 <*> pure body
--                 <*> genSigningKey AsByronKey
--         ShelleyBasedEra _ -> do
--             let
--                 genShelley =
--                     makeShelleyKeyWitness body <$> genShelleyWitnessSigningKey
--                 genBootstrap =
--                     makeShelleyBootstrapWitness
--                     <$> genWitnessNetworkIdOrByronAddress
--                     <*> pure body
--                     <*> genSigningKey AsByronKey

--             bsWits  <- frequency
--                 [ (3, scale (`div` 3) $ listOf1 genBootstrap)
--                 , (1, pure [])
--                 ]
--             keyWits <- frequency
--                 [ (3, scale (`div` 3) $ listOf1 genShelley)
--                 , (1, pure [])
--                 ]
--             return $ bsWits ++ keyWits

genWitness :: CardanoEra era -> TxBody era -> Gen (KeyWitness era)
genWitness = error "TODO conway: genWitness"
-- genWitness era body =
--   case cardanoEraStyle era of
--     LegacyByronEra    ->
--         makeByronKeyWitness
--             <$> genNetworkId
--             <*> pure body
--             <*> genSigningKey AsByronKey
--     ShelleyBasedEra _ ->
--       oneof [ makeShelleyBootstrapWitness
--                   <$> genWitnessNetworkIdOrByronAddress
--                   <*> pure body
--                   <*> genSigningKey AsByronKey
--             , makeShelleyKeyWitness body <$> genShelleyWitnessSigningKey
--             ]

genTxInEra :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTxInEra era = do
  body <- genTxBody era
  makeSignedTransaction
    <$> genWitnesses era body
    <*> pure body

genTx :: Gen (InAnyCardanoEra Tx)
genTx =
    oneof [ InAnyCardanoEra ByronEra    <$> genTxInEra ByronEra
          , InAnyCardanoEra ShelleyEra  <$> genTxInEra ShelleyEra
          , InAnyCardanoEra MaryEra     <$> genTxInEra MaryEra
          , InAnyCardanoEra AllegraEra  <$> genTxInEra AllegraEra
          , InAnyCardanoEra AlonzoEra   <$> genTxInEra AlonzoEra
          , InAnyCardanoEra BabbageEra  <$> genTxInEra BabbageEra
          ]

-- TODO: Generate txs with no inputs
-- TODO: Generate txs with existing key witnesses
-- TODO: Generate txs with no outputs
genTxForBalancing :: forall era. IsCardanoEra era => CardanoEra era -> Gen (Tx era)
genTxForBalancing era = makeSignedTransaction [] <$> genTxBodyForBalancing era

--------------------------------------------------------------------------------
-- Orphan instances
--------------------------------------------------------------------------------

-- This definition makes it possible to avoid unwrapping and wrapping
-- 'Lovelace' values when using 'choose'.
--
deriving via Integer instance Random Lovelace
