{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
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
    , genEncodingBoundaryCoin
    , genEpochNo
    , genExecutionUnitPrices
    , genExecutionUnits
    , genExtraKeyWitnesses
    , genCoin
    , genMIRPot
    , genMIRTarget
    , genNat
    , genNetworkId
    , genNetworkMagic
    , genPaymentCredential
    , genPlutusLanguage
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
    , withEraWitness
    ) where

import Prelude

import Cardano.Api
    ( Address
    , AddressAny (..)
    , AddressInEra
    , AnyPlutusScriptVersion (..)
    , AnyScriptLanguage (AnyScriptLanguage)
    , AnyScriptWitness (..)
    , AsType (..)
    , AssetId (..)
    , AssetName (..)
    , BabbageEra
    , BuildTx
    , BuildTxWith (BuildTxWith)
    , ByronAddr
    , ByronEra
    , CardanoEra (..)
    , Certificate
    , Convert (..)
    , ConwayEra
    , ConwayEraOnwards (..)
    , CostModel (..)
    , Eon (..)
    , EpochNo (EpochNo)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (ExecutionUnits)
    , Featured (..)
    , HasTypeProxy (AsType)
    , Hash
    , HashableScriptData
    , InAnyCardanoEra (..)
    , Key (..)
    , KeyWitness
    , KeyWitnessInCtx (KeyWitnessForSpending, KeyWitnessForStakeAddr)
    , MIRPot (..)
    , MaryEraOnwards (..)
    , NetworkId (..)
    , NetworkMagic (NetworkMagic)
    , PaymentCredential (..)
    , PlutusScript
    , PlutusScriptVersion (..)
    , PolicyId (PolicyId)
    , PraosNonce
    , ProtocolParametersUpdate (ProtocolParametersUpdate)
    , Quantity
    , Script (..)
    , ScriptData (..)
    , ScriptDatum (..)
    , ScriptHash
    , ScriptInAnyLang (..)
    , ScriptInEra (..)
    , ScriptLanguage (..)
    , ScriptLanguageInEra
    , ScriptValidity (..)
    , ScriptWitness (..)
    , ScriptWitnessInCtx (ScriptWitnessForSpending, ScriptWitnessForStakeAddr)
    , SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR)
    , ShelleyAddr
    , ShelleyBasedEra (..)
    , ShelleyToBabbageEra (..)
    , ShelleyWitnessSigningKey (..)
    , SimpleScript (..)
    , SlotNo (SlotNo)
    , StakeAddress
    , StakeAddressReference (NoStakeAddress, StakeAddressByValue)
    , StakeAddressRequirements (..)
    , StakeCredential
    , StakeDelegationRequirements (..)
    , StakePoolMetadata
    , StakePoolMetadataReference
    , StakePoolParameters
    , StakePoolRegistrationRequirements (..)
    , StakePoolRelay
    , StakePoolRetirementRequirements (..)
    , ToJSON
    , Tx
    , TxAuxScripts (..)
    , TxBody
    , TxBodyContent (TxBodyContent, txInsCollateral)
    , TxBodyError
    , TxCertificates (..)
    , TxExtraKeyWitnesses (..)
    , TxFee (..)
    , TxId (..)
    , TxIn (..)
    , TxInsCollateral (..)
    , TxInsReference (TxInsReferenceNone)
    , TxIx (..)
    , TxMetadata (..)
    , TxMetadataInEra (..)
    , TxMetadataValue (..)
    , TxMintValue (..)
    , TxOut (..)
    , TxOutDatum (..)
    , TxOutValue (..)
    , TxProposalProcedures (..)
    , TxReturnCollateral (..)
    , TxScriptValidity (TxScriptValidity)
    , TxTotalCollateral (..)
    , TxUpdateProposal (..)
    , TxValidityLowerBound (TxValidityLowerBound)
    , TxValidityUpperBound (..)
    , TxVotingProcedures (..)
    , TxWithdrawals (..)
    , UpdateProposal (UpdateProposal)
    , Value
    , WitCtxMint
    , WitCtxStake
    , WitCtxTxIn
    , Witness (..)
    , byronAddressInEra
    , collectTxBodyScriptWitnesses
    , createTransactionBody
    , forEraMaybeEon
    , hashScript
    , languageOfScriptLanguageInEra
    , makeByronAddress
    , makePraosNonce
    , makeShelleyAddress
    , makeShelleyBootstrapWitness
    , makeShelleyKeyWitness
    , makeSignedTransaction
    , makeStakeAddress
    , makeStakeAddressDelegationCertificate
    , makeStakeAddressRegistrationCertificate
    , makeStakeAddressUnregistrationCertificate
    , makeStakePoolRegistrationCertificate
    , makeStakePoolRetirementCertificate
    , scriptLanguageSupportedInEra
    , shelleyAddressInEra
    , validateAndHashStakePoolMetadata
    )
import Cardano.Api.Byron
    ( KeyWitness (ByronKeyWitness)
    , WitnessNetworkIdOrByronAddress (..)
    )
import Cardano.Api.Shelley
    ( Hash (..)
    , LedgerProtocolParameters (..)
    , PlutusScript (..)
    , PlutusScriptOrReferenceInput (..)
    , PoolId
    , ReferenceScript (..)
    , SimpleScriptOrReferenceInput (..)
    , StakeCredential (..)
    , StakePoolMetadata (..)
    , StakePoolMetadataReference (..)
    , StakePoolParameters (..)
    , StakePoolRelay (..)
    , toShelleyPoolParams
    )
import Cardano.Ledger.Api
    ( emptyPParams
    )
import Cardano.Ledger.Credential.Safe
    ( Ptr
    , SlotNo32 (..)
    )
import Cardano.Ledger.DRep
    ( DRep (..)
    )
import Cardano.Ledger.Hashes
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Plutus.Language
    ( Language (..)
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
import Data.Either
    ( rights
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
    ( fromMaybe
    , isJust
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
import Data.Traversable
    ( for
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
    )
import GHC.IsList
    ( fromList
    )
import Network.Socket
    ( PortNumber
    )
import Numeric.Natural
    ( Natural
    )
import Test.Cardano.Chain.UTxO.Gen
    ( genVKWitness
    )
import Test.Cardano.Crypto.Gen
    ( genProtocolMagicId
    )
import Test.Cardano.Ledger.Conway.Arbitrary
    ()
import Test.Cardano.Ledger.Core.Arbitrary
    ()
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
import Test.QuickCheck.Hedgehog
    ( hedgehog
    )
import Test.QuickCheck.Instances.ByteString
    ()

import qualified Cardano.Api as Api
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Api.Shelley as ShelleyApi
import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Crypto.Seed as Crypto
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.Shelley.TxBody as Ledger
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import qualified Data.Map as Map
import qualified Data.Map.Ordered.Strict as OMap
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified PlutusLedgerApi.Test.V1.EvaluationContext as V1
import qualified PlutusLedgerApi.Test.V2.EvaluationContext as V2
import qualified PlutusLedgerApi.Test.V3.EvaluationContext as V3

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

type Lovelace = Ledger.Coin

-- | The smallest quantity of lovelace that can appear in a transaction output's
--   value map.
--
-- In practice, the protocol parameters may require this value to be higher, so
-- this is an absolute minimum.
txOutMinLovelace :: Integer
txOutMinLovelace = 0

-- | The greatest quantity of lovelace that can appear in a transaction output's
--   value map.
--
-- In practice, this is limited by the total available supply of lovelace.
txOutMaxLovelace :: Integer
txOutMaxLovelace = 45_000_000_000_000_000

--------------------------------------------------------------------------------
-- Temporary
--------------------------------------------------------------------------------

withEraWitness :: (Eon eon) => CardanoEra era -> (eon era -> a) -> a
withEraWitness era f = case forEraMaybeEon era of
    Nothing -> error "unsupported era"
    Just se -> f se

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
genTxIndex =
    oneof
        [ (TxIx . intCast) <$> (arbitrary @Word16)
        , -- FIXME: cardano-api uses a full Word here, yet the ledger uses Word16
          -- and we'll fail to construct a tx unless we constrain ourselves to
          -- Word16 here.
          TxIx . fromIntegral . getNonNegative <$> (arbitrary @(NonNegative Int))
          -- For some bias towards small values
        ]

genTxInsCollateral :: CardanoEra era -> Gen (TxInsCollateral era)
genTxInsCollateral era = withEraWitness era $ \supported ->
    oneof
        [ pure TxInsCollateralNone
        , TxInsCollateral supported
            <$> scale (`div` 3) (listOf genTxIn)
        ]

genSlotNo :: Gen SlotNo
genSlotNo = do
    boundary <- genBoundary
    frequency
        [ (20, pure $ SlotNo boundary)
        , (20, pure $ SlotNo (maxBound @Word64 - boundary))
        , (60, SlotNo <$> arbitrary @Word64)
        ]
  where
    genBoundary = choose (0, maxBound @Word64 `div` 2)

genSlotNo32 :: Gen SlotNo32
genSlotNo32 = do
    offset <- genOffset
    frequency
        [ (20, pure $ SlotNo32 offset)
        , (20, pure $ SlotNo32 (maxBound @Word32 - offset))
        , (60, SlotNo32 <$> arbitrary @Word32)
        ]
  where
    genOffset = choose (0, 10_000)

genCoin :: Gen Ledger.Coin
genCoin =
    frequency
        [ (60, genCoinForTxOutAdaValue)
        , (40, fromIntegral @Integer <$> choose (1_000_000, 1_000_000_000))
        ]

genCoinForTxOutAdaValue :: Gen Ledger.Coin
genCoinForTxOutAdaValue =
    frequency
        [ (60, fromIntegral @Integer <$> choose (1_000_000, 1_000_000_000))
        , (10, fromIntegral <$> choose (txOutMinLovelace, txOutMaxLovelace))
        , (30, genEncodingBoundaryCoin)
        ]

genEncodingBoundaryCoin :: Gen Ledger.Coin
genEncodingBoundaryCoin = do
    -- https://json.nlohmann.me/features/binary_formats/cbor/
    -- Generate a point near a boundary
    -- However, the three first ones are below the minimum utxo value on
    -- mainnet, and are less useful to generate (in that context).
    boundary <-
        frequency
            [ (1, pure 24)
            , (1, pure 256) -- 2^ 8
            , (8, pure 65_536) -- 2^16
            , (90, pure 4_294_967_296) -- 2^32
            ]

    offset <-
        frequency
            [ (1, fromIntegral @Integer <$> choose (-10, 10))
            , -- Either offset by -1 (just below boundary), or 0 (just above boundary)
              (1, fromIntegral @Integer <$> choose (-1, 0))
            , -- Offset by values close to common fee values, in both the positive
              -- and negative direction, with the hope that this helps find
              -- corner-cases.
              (1, fromIntegral @Integer <$> choose (-220_000, -150_000))
            , (1, fromIntegral @Integer <$> choose (150_000, 220_000))
            , (1, fromIntegral @Integer <$> choose (-1_000_000, 1_000_000))
            ]
    pure $  max 0 $ boundary + offset

genTxFee :: CardanoEra era -> Gen (TxFee era)
genTxFee era = withEraWitness era $ \supported ->
    TxFeeExplicit supported <$> genCoin

genTtl :: Gen SlotNo
genTtl = genSlotNo

genTxValidityLowerBound :: CardanoEra era -> Gen (TxValidityLowerBound era)
genTxValidityLowerBound era = withEraWitness era $ \supported ->
    TxValidityLowerBound supported <$> genTtl

genTxValidityUpperBound :: CardanoEra era -> Gen (TxValidityUpperBound era)
genTxValidityUpperBound era = withEraWitness era $ \supported ->
    TxValidityUpperBound supported
        <$> oneof
            [ Just <$> genTtl
            , pure Nothing
            ]

genTxValidityRange
    :: CardanoEra era
    -> Gen (TxValidityLowerBound era, TxValidityUpperBound era)
genTxValidityRange era =
    (,)
        <$> genTxValidityLowerBound era
        <*> genTxValidityUpperBound era

genTxScriptValidity :: CardanoEra era -> Gen (TxScriptValidity era)
genTxScriptValidity era = withEraWitness era $ \supported ->
    TxScriptValidity supported <$> genScriptValidity

genScriptValidity :: Gen ScriptValidity
genScriptValidity = elements [ScriptInvalid, ScriptValid]

genSeed :: Int -> Gen Crypto.Seed
genSeed n = (Crypto.mkSeedFromBytes . BS.pack) <$> vector n

genSigningKey :: (Key keyrole) => AsType keyrole -> Gen (SigningKey keyrole)
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

genVerificationKeyHash
    :: (Key keyrole, HasTypeProxy keyrole)
    => AsType keyrole
    -> Gen (Hash keyrole)
genVerificationKeyHash roletoken =
    verificationKeyHash <$> genVerificationKey roletoken

genExtraKeyWitnesses :: CardanoEra era -> Gen (TxExtraKeyWitnesses era)
genExtraKeyWitnesses era =
    case forEraMaybeEon era of
        Nothing -> pure TxExtraKeyWitnessesNone
        Just supported ->
            oneof
                [ pure TxExtraKeyWitnessesNone
                , TxExtraKeyWitnesses supported
                    <$> scale (`div` 3) (listOf (genVerificationKeyHash AsPaymentKey))
                ]

genTxTotalCollateral :: CardanoEra era -> Gen (TxTotalCollateral era)
genTxTotalCollateral era = withEraWitness era $ \supported ->
    frequency
        [ (95, pure TxTotalCollateralNone)
        , (5, TxTotalCollateral supported <$> genCoin)
        ]

genTxReturnCollateral :: CardanoEra era -> Gen (TxReturnCollateral ctx era)
genTxReturnCollateral era = withEraWitness era $ \supported ->
    frequency
        [ (95, pure TxReturnCollateralNone)
        , (5, TxReturnCollateral supported <$> genTxOut era)
        ]

genPlutusScript :: PlutusScriptVersion lang -> Gen (PlutusScript lang)
genPlutusScript lang = PlutusScriptSerialised . SBS.toShort <$>
     case lang of
         PlutusScriptV3 -> elements sampleScriptsV3
         PlutusScriptV2 -> elements sampleScriptsV2
         PlutusScriptV1 -> elements sampleScriptsV1

sampleScriptsV3 :: [ByteString]
sampleScriptsV3 =
    rights $ B16.decode <$> [ "5902aa010100323232323232323232322533300332323232325332330093001300b37540042646644a66666602800c2646464a66601e60060022a66602660246ea802400803854ccc03cc01c00454ccc04cc048dd500480100700718081baa0081533300d3001300f37540042646464646464a6660266016602a6ea80344cc010c01530103d87980003370e6660026eacc064c068c068c068c068c058dd50071bae30033016375400c91010b487964726148656164563100480044c94ccc050c020c058dd5000899802980326103d87a8000300c333002375660346036602e6ea8c068c05cdd50009bae30043017375400e9110b4879647261486561645631001533015491054c35373b39001632533301800114c103d87a80001300333019301a0014bd701bac30033016375401c44464a66602c601c60306ea8004520001375a603860326ea8004c94ccc058c038c060dd50008a6103d87a8000132330010013756603a60346ea8008894ccc070004530103d87a8000132333222533301c337220100062a66603866e3c02000c4c02ccc084dd400125eb80530103d87a8000133006006001375c60360026eb4c070004c080008c078004c8cc004004010894ccc06c0045300103d87a8000132333222533301b337220100062a66603666e3c02000c4c028cc080dd300125eb80530103d87a8000133006006001375c60340026eacc06c004c07c008c074004dd2a40004602e6030603000244a666022002294454cc0480085894ccc03cc00cc044dd50008a490343303100149103433032003013301037540042a6601c9201054c34373b350016370e900000580580580598080009808180880098061baa002370e90010b1806980700198060011805801180580098031baa00114984d95854cc0092401054c34333b3500165734ae7155ceaab9e5573eae815d0aba257481"
    , "59044b010100323232323232323232322533300332323232325332330093001300b37540042646644a66666602800c2646464a66601e6006002264a66602800201e264a666666032002020020020020264a66602c603200600a0226eb8004c058004c048dd50048a9998079803800899299980a000807899299999980c800808008008099299980b180c8018028089bad00101030160013012375401201c60206ea802054ccc034c004c03cdd5001099191919191929998099803980a9baa00d153330133300330044c103d87e80003371e6eb8c008c058dd50031bae30193016375401a2660066008980103d87980003322325333016300e30183754002266e24dd6980e180c9baa00100213300630074c103d87a80004a0600860306ea8c00cc060dd50011802980b1baa00e375a6002602c6ea8018528099299980a198021802a6103d87c80003322325333017300f30193754002266e20008dd6980e980d1baa00113300730084c103d87b80004a0600a60326ea8c014c064dd50011803180b9baa00f375a6004602e6ea801c4c8c8c8c8cc020c02530103d87d80003371e6e48ccc00ccc008ccc004004dd61802180d9baa0130052376600291010022337140040026e48ccc00ccc008c8cc004004dd61802980e1baa00c22533301e00114bd700991919800800998020021811801912999810800899811001a5eb804cc894ccc07ccdd79991192999811180d18121baa00113322533302433710004002298103d879800015333024337100020042980103d87b800014c103d87a8000375a6020604a6ea800cdd6980818129baa002100133225333023337200040022980103d8798000153330233371e0040022980103d87a800014c103d87b8000375c602060486ea8008dd7180818121baa001300e3022375400a601c60446ea800930103d8798000133024005003133024002330040040013023001302400130200012375c600e60386ea8005221002233714004002444a66603466e24005200014bd700a99980f0010a5eb804cc07cc080008ccc00c00cc084008cdc0000a400244646600200200644a66603c002297ae013301f37526006604000266004004604200244464666002002008006444a66603e004200226660060066044004660080026eb8c0840088c06cc070c0700045281bad30193016375401a4603260340024603000244a666024002294454cc04c008528119299980898028008a490344303100153330113009001149010344303200153330113370e90020008a490344303300153330113370e90030008a490344303400153330113370e90040008a49034430350014910344303600301237540024602a602c602c602c602c602c602c602c002602660206ea800854cc039241054c35353b350016370e900000580580580598080009808180880098061baa002370e90010b1806980700198060011805801180580098031baa00114984d95854cc0092401054c35313b3500165734ae7155ceaab9e5573eae815d0aba257481"
    , "590a360101003232323232323232323223225333005323232323253323300b3001300d37540042646644a66666602c00c26464646464a66602660080022a66602e602c6ea802c00804854ccc04cc0240044c94ccc06000404c4c94cccccc0740040500504c94ccc068c07400c4cc020004894ccc07000801c4c94cccccc0840044ccc0240044c008c08000c060060060060060c078008054dd600080a00a180d000980b1baa00b012301437540142a666022600460266ea80104c8c8c8c8c8c8c94ccc060c024c068dd500809980224903493031003370e6660026eacc018c06cdd5008803a4410b487964726148656164563100480044c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c94cccccc0d00040080084c8cc07c004894ccc0cc0084c8c94ccc0c4cc07524103493133003371e0406eb8c050c0d0dd50040a999818991919299981a181280089810a49034930350015333034302a00113232533303a0010021533303a303d0011323302349010349303200323300100100722533303d00114a026644a66607666e3c0080145288998020020009bae303f0013040001375c6078002004646600200200844a666076002297ae0132333222533303b303100213304037520066600c00c00226600c00c0026eb8c0e8004dd6981d800981f801181e80089810a4903493036001302149103493036003018001330163301c0020214bd6f7b6301bac303730383038303830383038303830383038303437540542a6660626603a92103493134003375e603e60686ea80a9300101a000153330313301d49103493033003371266e00c040004c040ccc048cc04400894ccc0c8c08cc0d0dd500089bab300e30353754601c606a6ea8c0e0c0d4dd50008a5eb7bdb1812f5bded8c002660206660246602201246eacc038c0d4dd5000a5eb7bdb18004c4cc0280080145280a5014a02940cc050c94ccc0c4c09cc0ccdd50008a60103d87a80001301c33036300d30343754601a60686ea8c0dcc0d0dd5000a5eb80cc02cdd6180b98199baa0290244bd6f7b6301980680b9192999818981118199baa0011301c3303630373034375400297ae01300e490103493135003300b3758602e60666ea80a40044c94cccccc0e000454ccc0c0c084c0c8dd5000899299981a800803899299999981d000899299981b800804899299999981e000805005005005099299981c981e001899981280209803981e0040058059bae0013039001303900200800800800830370013033375400200c00c00c00c00c606a0046eb0004008008c01cc0b8dd50010980ba491f4661696c656420746f206465636f6465206c6f636b65645f636f6d6d697473003232533302f0010021533302f303200113232533302d301e0011300a490103493039001533302d30230011300a490103493130001323253333330370021533302f302030313754004264a66606800200426464a66606c00200826464a66607000200c264a66666607a00200e00e00e00e264a666074607a00620120106eb8004c0e8004c0e8008c0e0004c0e0008c0d8004c0c8dd500100080080080080089805a490349313100303330303754004605c6ea8004c0c4004008cc0200088c038c0b8dd5000898032481034931320032330010013758601860586ea8088894ccc0b800452f5c026644a66605864a66605a6046605e6ea80044cdc78151bae3033303037540022940c04cc0bcdd5180998179baa002133031002330040040011330040040013030001303100130010012223232533302f0011533302f37586062606400429444c061240103493037001325333030001130194901034930380013333222253330303021303237540082646603a921034930340053330313371e6eccc034c0d0dd50009bae300d303437540062a66606266ebcc060c0d0dd5000980c181a1baa003133300b00b00400214a02940c0d8c0ccdd50020a5030320023033002303200130330013758606260640046eb0c0c0004cc0b8dd3801198171ba70014bd701119198008008019129998168008a6103d87a800013322533302b3375e6024605c6ea80080144c058cc0c00092f5c0266008008002605e00260600024605660580024a6604a0022c44646600200200644a666054002297ae013302b3003302c00133002002302d001233300b00148810048810022323300100100322533302800114bd700998149ba63003302a00133002002302b00122232333001001004003222533302900210011333003003302c00233004001375660560044464666002002006004444a66604e0042002264664466600c00c605a00a646600200200a44a66605800226605a66ec0dd48021ba60034bd6f7b630099199911299981619b90008003133031337606ea4020dd30038028a99981619b8f008003132533302d301e302f375400226606466ec0dd4804981998181baa001002100232533302d533303100114a22940530103d87a80001301833032374c00297ae03233300100100800322253330330021001132332233300600630390053233001001005225333038001133039337606ea4010dd4001a5eb7bdb1804c8ccc8894ccc0e0cdc800400189981e99bb037520106ea001c01454ccc0e0cdc7804001899299981c9815181d9baa00113303e337606ea4024c0fcc0f0dd5000801080119299981c98150008a60103d87a8000130243303e375000297ae03370000e00426607a66ec0dd48019ba800233006006001375c606e0026eb4c0e0004c0f0008c0e8004dd718190009bad30330013035002133031337606ea400cdd3001198030030009bae302b001375660580026060004605c0026eb8c098004dd598138009814801118121812981280091299980f180a18101baa002100113756604860426ea8008c004004894ccc080004520001337009001198010011811800918100009bac301e301b375402044464a6660366022603a6ea8004520001375a6042603c6ea8004c94ccc06cc044c074dd50008a6103d87a80001323300100137566044603e6ea8008894ccc084004530103d87a80001323332225333021337220100062a66604266e3c02000c4c030cc098dd400125eb80530103d87a8000133006006001375c60400026eb4c084004c094008c08c004cc01000c00888c8cc00400400c894ccc078004530103d87a8000132333222533301e3372200e0062a66603c66e3c01c00c4c024cc08cdd300125eb80530103d87a8000133006006001375c603a0026eacc078004c088008c080004dd2a400044a66602c00229444c00c00894cc054004588c064c068c068c068c068004dd7180b980a1baa0041533012491054c36323b3500162225333013300430153754006264a666030002004264a66666603a00200600600600626464a66603600200a264a66666604000200c00c00c264a66603a604000601000e6eb4004018c074004c07400cdd7000980d000980b1baa003001370e90001119198008008019119801800980100100680680680698090009809180980098071baa002370e90010b1807980800198070011806801180680098041baa00114984d958dd70008a998012481054c35383b3500165734ae7155ceaab9e5573eae815d0aba257481"
    ]

sampleScriptsV2 :: [ByteString]
sampleScriptsV2 =
    [ "5905540100003232323232323232322223232323232533300b32323232323232323232323232323232323232323232323232323253330263370e90000018a99981319b870054800854ccc098cdc38022400026644646600200200644a66605a00229404c8c94ccc0b0c01400852889980200200098188011bae302f00137586002604804646600601a0022c2c264a66604e66e1d20003026001132323232533302b3370e900218150008991919299981719b8700d4800854ccc0b8cdc3806240042a66605c66e1cdd6980998160011bad3013302c02b1533302e3375e60206058004602060580562a66605c66ebcc014c0b0008c014c0b00ac54ccc0b8c8c8cdc418009bac300b302e02d300137586016605c008600200244a66606600229000099b8048008cc008008c0d80044ccccc88888c8c8c8c94ccc0dc01054ccc0dc00c54ccc0dc00840045280a5014a06601000c4a66606e6602601000229444cc04c014004cc01c0148cc048014004cc0180148cc044014004cc01400c8cdc399199800800802a4000444a66607400420022666006006607a0046644a66607266e3c0080184cdc0000a400420026eb8c0f0008005200222323300100100322533303500114a226464a666068600a0042660080080022940c0e4008dd7181b8009bac3009302c02b3758601260580046eb0c014c0b00ac0505280a5014a029405280a5030250013031001302900116300130280022302f30303030001302d0013025001163300a01323375e0106018604a0024605660586058605800244646600200200644a66605600229404c8c94ccc0a8cdc78010028a51133004004001302f002375c605a002604403e6600401e00666002646600200202444a66604c002297ae01330273005302130280013300200230290010022232333001001003480008894ccc0a000840044ccc00c00cc0ac008cc894ccc09ccdd780318061812801099b80001480084004c0a8008004c010c074c004c0740088c090c0940054ccc074cdc3a4004603801e2646464a66604066e1d2000301f00113025301e001163300300f23375e004600a603c0026046002603601e2c44646600200200644a666046002298103d87a8000132325333022300500213374a90001981300125eb804cc010010004c09c008c0940048c084004dd6180f800980f800980f000980e800980e000980d800980d0011bac3018001301800130170023758602a002601a006602600260260046022002601200c2930b19299980599b874800000454ccc038c02401c526161533300b3370e90010008a99980718048038a4c2c2c601200c600200c464a66601466e1d200000113232323232323232533301530180021323232498cc03400c8dd70009980600211bae0013253330133370e9000000899191919299980d180e8010991924c64a66603266e1d200000113232533301e3021002132498c94ccc070cdc3a400000226464a666042604800426493180c0008b1811000980d0010a99980e19b87480080044c8c8c8c8c8c94ccc094c0a000852616375a604c002604c0046eb4c090004c090008dd69811000980d0010b180d0008b180f800980b8018a99980c99b874800800454ccc070c05c00c52616163017002301100316301b001301b00230190013011006163011005163758602c002602c0046eb0c050004c050008c048004c048008dd6980800098040010b18040009119198008008019129998070008a4c26466006006602400460066020002464a66601066e1d200000113232533300d3010002149858dd7180700098030010a99980419b87480080044c8c94ccc034c04000852616375c601c002600c0042c600c0024600a6ea80048c00cdd5000ab9a5573aaae7955cfaba05742ae89"
    , "5905fc010000323232323232323232232222323232533300b323232323232323232323232323232533301a3370e9000180c8008991919299980e99b8748000c0700044c8c8c8c94ccc084cdc3a40086040002264646464a66604a66e1d20043024001132323232323232533302c3370e6002646600200203a44a666062002297ae01330323017302c30330013300200230340014801054ccc0b0cdc3980080ca40042a6660580042a66605866646444a66606066e1cc00c008c00c0044c8cc00400400c894ccc0d400452889919299981a191980080080311299981c8008a501323253330383371e00400c29444cc010010004c0f4008dd7181d8008998020020008a503039002375c606e0022c600200244a66606200229000099b8048008cc008008c0d0004dd6180a98150139bac3031303230323032302a00a13322323300100101c22533303300114a026464a666064a66606466ebcc078c0c00080184cdc480298051bab301b303000214a029444cc010010004c0dc008c0d4004c054c0a8028dd6980c18150050a5014a0294052811919980080080124000444a66606400420022666006006606a0046644a6660626464a66606666e1d20020011324a260620042940c0c4004c074c0bcc074c0bc0084cdc0000a400420026068004002a66605466e25208092f4010011533302a337126eb4c058c0a00100044c94ccc0accdd7980a1814813180a1814802899b89375a602e605204c66e00dd6980b98148028008a50375a602c6050010294052818009bab3012302700b232332232533302d3370e9001000880109bad3032302b003302b00232533302b3370e90010008a60103d87a8000132323300100100222533303100114c103d87a800013232323253330323371e9110000213374a90001981b1ba80014bd700998030030019bad3033003375c6062004606a00460660026eacc0c0c0a4008c0a40052000323300100100222533302e00114c0103d87a8000132323232533302f3371e9110000213374a9000198199ba60014bd700998030030019bab3030003375c605c00460640046060002603c002605600260460022c600a604400c664464a66604c66e1d200000113232323232323232533303130340021323232498cc0a000c8dd70009981380211bae00132533302f3370e9000000899191919299981b181c8010991924c64a66606a66e1d200000113232533303a303d002132498c94ccc0e0cdc3a400000226464a66607a608000426493180b8008b181f000981b0010a99981c19b87480080044c8c8c8c8c8c94ccc104c11000852616375a608400260840046eb4c100004c100008dd6981f000981b0010b181b0008b181d80098198018a99981a99b874800800454ccc0e0c0cc00c52616163033002301000316303700130370023035001302d00616302d005163758606400260640046eb0c0c0004c0c0008c0b8004c0b8008dd6981600098120010b1812000919299981299b87480000044c8c94ccc0a8c0b400852616375c605600260460042a66604a66e1d200200113232533302a302d002149858dd7181580098118010b18118008009813800980f8008b1800980f1804980f00291812981318130009811800980d8008b19803804919baf0053009301b00130200013018001163300400923232533301c3370e9001000899b8f018375c604260340042940c068004c018c060c018c060c00cc060004c010c058c004c0580088c074c0780054ccc058cdc3a4004602a0102646464a66603266e1d200030180011301e3017001163300300823375e004600a602e002603800260280102c44646600200200644a6660380022980103d87a800013232533301b300500213374a90001980f80125eb804cc010010004c080008c0780048c068004dd6180c000980c000980b8011bac3015001300d0033013001301300230110013009004149858c0040148c94ccc02ccdc3a4000002264646464a666024602a004264931980380091bae001163758602600260260046eb4c044004c02400858c02400488c8cc00400400c894ccc03c00452613233003003301300230033011001375c0024600a6ea80048c00cdd5000ab9a5573aaae7955cfaba05742ae89"
    , "590221010000323232323232323232323223222232533300b32323232533300f3370e9000180700089919191919191919191919299980e98100010991919299980e99b87480000044c94ccc078cdc3a4000603a002264a66603e66e1c011200213371e00a0322940c07000458c8cc004004030894ccc088004530103d87a80001323253330213375e6603a603e004900000d099ba548000cc0940092f5c0266008008002604c00460480022a66603a66e1c009200113371e00602e2940c06c050dd6980e8011bae301b00116301e001323232533301b3370e90010008a5eb7bdb1804c8dd59810800980c801180c800991980080080111299980f0008a6103d87a8000132323232533301f3371e01e004266e95200033023374c00297ae0133006006003375660400066eb8c078008c088008c080004c8cc004004008894ccc07400452f5bded8c0264646464a66603c66e3d221000021003133022337606ea4008dd3000998030030019bab301f003375c603a0046042004603e0026eacc070004c070004c06c004c068004c064008dd6180b80098078029bae3015001300d001163013001301300230110013009002149858c94ccc02ccdc3a40000022a66601c60120062930b0a99980599b874800800454ccc038c02400c52616163009002375c0026600200290001111199980399b8700100300c233330050053370000890011807000801001118029baa001230033754002ae6955ceaab9e5573eae815d0aba201"
    ]

sampleScriptsV1 :: [ByteString]
sampleScriptsV1 =
    [ "59084601000033233322232332232333222323332223322323332223233223233223332223333222233322233223322332233223332223322332233322232323232322222325335300b001103c13503d35303b3357389201035054350003c498ccc888c8c8c94cd4c05c0144d4c0680188888cd4c04c480048d4c0ed40188888888888cd4c078480048ccd5cd19b8f375c0020180440420066a6040006446a6048004446a605000444666aa60302400244a66a6a07c0044266a08c0020042002a0886466a002a088a08a2446600466a609000846a0820024a0806600400e00226a606ca002444444444466a6032240024646464666ae68cdc399991119191800802990009aa82c1119a9a826000a4000446a6aa08a00444a66a6050666ae68cdc78010048150148980380089803001990009aa82b9119a9a825800a4000446a6aa08800444a66a604e666ae68cdc7801003814814080089803001999aa81e3ae335503c75ceb4d4c084cccd5cd19b8735573aa006900011998119aba1500335742a00466a080eb8d5d09aba2500223505135304f33573892010350543100050499262220020183371491010270200035302801422220044800808007c4d5d1280089aab9e500113754002012264a66a6a070601a6aae78dd50008a81a910a99a9a81d0008a81b910a99a9a81e0008a81c910a99a9a81f0008a81d910a99a9a8200008a81e910a99a9a8210008a81f910a99a9a8220008a820910a99a9a8230008a821910a99a9a8240008a822910a99a9a8250008a823910a99a9a82600089999999999825981000a18100090080071810006181000500418100031810002001110a8259a980a1999ab9a3370e6aae754009200023301635742a0046ae84d5d1280111a8211a982019ab9c490103505431000414992622002135573ca00226ea8004cd40148c8c8c8c8cccd5cd19b8735573aa00890001199980d9bae35742a0086464646666ae68cdc39aab9d5002480008cc88cc08c008004c8c8c8cccd5cd19b8735573aa004900011991198148010009919191999ab9a3370e6aae754009200023302d304735742a00466a07a4646464646666ae68cdc3a800a4004466606a6eb4d5d0a8021bad35742a0066eb4d5d09aba2500323333573466e1d4009200023037304e357426aae7940188d4154d4c14ccd5ce2490350543100054499264984d55cea80189aba25001135573ca00226ea8004d5d09aba2500223504e35304c335738921035054310004d49926135573ca00226ea8004d5d0a80119a81cbae357426ae8940088d4128d4c120cd5ce249035054310004949926135573ca00226ea8004d5d0a80119a81abae357426ae8940088d4118d4c110cd5ce249035054310004549926135573ca00226ea8004d5d0a8019bad35742a00464646464646666ae68cdc3a800a40084605c646464646666ae68cdc3a800a40044606c6464646666ae68cdc39aab9d5002480008cc88cd40f8008004dd69aba15002375a6ae84d5d1280111a8289a982799ab9c491035054310005049926135573ca00226ea8004d5d09aab9e500423333573466e1d40092000233036304b35742a0086eb4d5d09aba2500423504e35304c335738921035054310004d499264984d55cea80109aab9e5001137540026ae84d55cf280291999ab9a3370ea0049001118169bad357426aae7940188cccd5cd19b875003480008ccc0bcc11cd5d0a8031bad35742a00a66a072eb4d5d09aba2500523504a353048335738920103505431000494992649926135573aa00626ae8940044d55cf280089baa001357426ae8940088d4108d4c100cd5ce249035054310004149926135744a00226ae8940044d55cf280089baa0010033350052323333573466e1d40052002201623333573466e1d40092000201623504035303e335738921035054310003f499264984d55ce9baa001002335005200100112001230023758002640026aa072446666aae7c004940c08cd40bcd5d080118019aba2002498c8004d540e088448894cd4d40bc0044008884cc014008ccd54c01c48004014010004c8004d540dc884894cd4d40b400440188854cd4c01cc01000840244cd4c01848004010004488008488004800488848ccc00401000c00880048848cc00400c00880044880084880048004888848cccc00401401000c00880048848cc00400c00880048848cc00400c00880048848cc00400c00880048488c00800c888488ccc00401401000c800484888c00c0108884888ccc00801801401084888c00401080048488c00800c88488cc00401000c800448848cc00400c008480044488c88c008dd5800990009aa80d11191999aab9f0022501223350113355008300635573aa004600a6aae794008c010d5d100180c09aba10011122123300100300211200112232323333573466e1d400520002350083005357426aae79400c8cccd5cd19b87500248008940208d405cd4c054cd5ce24810350543100016499264984d55cea80089baa00112122300200311220011200113500d35300b3357389211f556e6578706563746564205478496e666f20636f6e737472756374696f6e2e0000c498888888888848cccccccccc00402c02802402001c01801401000c00880044488008488488cc00401000c480048c8c8cccd5cd19b875001480088c018dd71aba135573ca00646666ae68cdc3a80124000460106eb8d5d09aab9e500423500c35300a3357389201035054310000b499264984d55cea80089baa001212230020032122300100320012323333573466e1d40052002200823333573466e1d40092000200a2350073530053357389210350543100006499264984d55ce9baa0011200120011261220021220012001112323001001223300330020020014891c0029cb7c88c7567b63d1a512c0ed626aa169688ec980730c0473b9130001"
    ]

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
    genTerm n =
        frequency
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
        , do
            ts <- scale (`mod` 10) $ listOf $ recurse n
            m <- choose (0, length ts)
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
    oneof
        [ SScript
            <$> genSimpleScript
        , SReferenceScript
            <$> genReferenceInput
        ]

genScript :: ScriptLanguage lang -> Gen (Script lang)
genScript = \case
    SimpleScriptLanguage -> SimpleScript <$> genSimpleScript
    PlutusScriptLanguage lang -> PlutusScript lang <$> genPlutusScript lang

genScriptInAnyLang :: Maybe (CardanoEra era) -> Gen ScriptInAnyLang
genScriptInAnyLang optionalEra =
    oneof
        [ ScriptInAnyLang lang <$> genScript lang
        | AnyScriptLanguage lang <- [minBound .. maxBound]
        , case optionalEra of
            Nothing -> True
            Just era -> withEraWitness era $ \supported ->
                isJust (scriptLanguageSupportedInEra supported lang)
        ]

genScriptInEra :: CardanoEra era -> Gen (ScriptInEra era)
genScriptInEra era = withEraWitness era $ \supported ->
    oneof
        [ ScriptInEra langInEra <$> genScript lang
        | AnyScriptLanguage lang <- [minBound .. maxBound]
        , Just langInEra <-
            [scriptLanguageSupportedInEra supported lang]
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
genAlphaNum =
    elements
        "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

genPolicyId :: Gen PolicyId
genPolicyId =
    frequency
        -- Mostly from a small number of choices, so we get plenty of repetition.
        --
        -- And because of the additional choice of asset name we repeat ourselves
        -- even more here.
        [ (80, pure $ fromString ('a' : replicate 55 '0'))
        , (18, elements [fromString (x : replicate 55 '0') | x <- ['a' .. 'c']])
        , -- and some from the full range of the type
          (2, PolicyId <$> genScriptHash)
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
    assetIds <-
        oneof
            [ nub <$> scale (`div` 4) (listOf genAssetIdNoAda)
            , pure []
            ]
    assetQuantities <- infiniteListOf genUnsignedQuantity
    ada <- fromIntegral <$> genCoinForTxOutAdaValue
    return $ fromList $ (AdaAssetId, ada) : zip assetIds assetQuantities

-- | Generate a 'Value' which could represent the balance of a partial
-- transaction, where both ada and other assets can be included, and quantities
-- can be both positive and negative.
genSignedValue :: Gen Value
genSignedValue = do
    assetIds <-
        oneof
            [ nub <$> scale (`div` 4) (listOf genAssetIdNoAda)
            , pure []
            ]
    assetQuantities <- infiniteListOf genSignedQuantity
    ada <-
        fromIntegral
            <$> oneof
                [ genCoin
                , negate <$> genCoin
                ]
    return $ fromList $ (AdaAssetId, ada) : zip assetIds assetQuantities

-- | Generate a 'Value' suitable for minting, i.e. non-ADA asset ID and a
-- positive or negative quantity.
genValueForMinting :: Gen [(AssetId, Quantity)]
genValueForMinting =
    listOf ((,) <$> genAssetIdNoAda <*> genSignedQuantity)

genTxMintValue
    :: forall era. CardanoEra era -> Gen (TxMintValue BuildTx era)
genTxMintValue era = withEraWitness era $ \supported ->
    do
        let
            scriptWitnessGenerators :: Gen (ScriptWitness WitCtxMint era)
            scriptWitnessGenerators =
                oneof
                    [ genScriptWitnessMint langInEra
                    | AnyScriptLanguage lang <- [minBound .. maxBound]
                    , Just langInEra <-
                        [ scriptLanguageSupportedInEra
                            (convert supported)
                            lang
                        ]
                    ]
            mints = do
                policy <- genPolicyId
                assets <- fromList <$> listOf (do
                    assetName <- genAssetName
                    quantity <- genSignedQuantity
                    pure (assetName, quantity))
                script <- BuildTxWith <$> scriptWitnessGenerators
                pure (policy, (assets, script))

        oneof
            [ pure TxMintNone
            , scale (`div` 3)
                $ TxMintValue supported . Map.fromList
                    <$> listOf mints
            ]

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
    genTerm n =
        frequency
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
    genConstructorIx =
        frequency
            [ (45, arbitrarySizedNatural)
            , (40, choose (0, 5))
            , (5, fromIntegral <$> arbitrary @Word64)
            ]

shrinkScriptData :: ScriptData -> [ScriptData]
shrinkScriptData s =
    aggressivelyShrink s ++ case s of
        ScriptDataList l ->
            ScriptDataList <$> shrinkList shrinkScriptData l
        ScriptDataMap m ->
            ScriptDataMap <$> shrinkList (shrinkTuple shrinkScriptData) m
        ScriptDataNumber n -> ScriptDataNumber <$> shrink n
        ScriptDataBytes bs -> ScriptDataBytes <$> shrink bs
        ScriptDataConstructor n l ->
            uncurry ScriptDataConstructor
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
genTxWithdrawals era = withEraWitness era $ \supported -> do
    frequency
        [ (1, pure TxWithdrawalsNone)
        , (1, pure $ TxWithdrawals supported [])
        ,
            ( 3
            , TxWithdrawals supported
                <$> scale (`div` 3) (listOf (genWithdrawalInfo era))
            )
        ]

genWithdrawalInfo
    :: CardanoEra era
    -> Gen
        ( StakeAddress
        , Lovelace
        , BuildTxWith BuildTx (Witness WitCtxStake era)
        )
genWithdrawalInfo era = do
    stakeAddr <- genStakeAddress
    amt <- genCoin
    wit <- BuildTxWith <$> genWitnessStake era
    pure (stakeAddr, amt, wit)

genWitnessStake :: CardanoEra era -> Gen (Witness WitCtxStake era)
genWitnessStake era = withEraWitness era $ \support ->
    oneof
        $ [pure $ KeyWitness KeyWitnessForStakeAddr]
            <> [ ScriptWitness ScriptWitnessForStakeAddr
                <$> genScriptWitnessStake langInEra
               | AnyScriptLanguage lang <- [minBound .. maxBound]
               , Just langInEra <-
                    [scriptLanguageSupportedInEra support lang]
               ]

genWitnessSpend :: CardanoEra era -> Gen (Witness WitCtxTxIn era)
genWitnessSpend era = withEraWitness era $ \support ->
    oneof
        $ [pure $ KeyWitness KeyWitnessForSpending]
            <> [ ScriptWitness ScriptWitnessForSpending
                <$> genScriptWitnessSpend langInEra
               | AnyScriptLanguage lang <- [minBound .. maxBound]
               , Just langInEra <-
                    [scriptLanguageSupportedInEra support lang]
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
                <*> (ScriptDatumForTxIn . Just <$> genHashableScriptData)
                <*> genHashableScriptData
                <*> genExecutionUnits

genTxAuxScripts :: CardanoEra era -> Gen (TxAuxScripts era)
genTxAuxScripts era = withEraWitness era $ \supported ->
    frequency
        [ (1, pure TxAuxScriptsNone)
        ,
            ( 3
            , TxAuxScripts supported
                <$> scale (`div` 3) (listOf (genScriptInEra era))
            )
        ]

genTxMetadataInEra :: CardanoEra era -> Gen (TxMetadataInEra era)
genTxMetadataInEra era = withEraWitness era $ \supported ->
    oneof
        [ pure TxMetadataNone
        , TxMetadataInEra supported <$> genTxMetadata
        ]

genTxMetadata :: Gen TxMetadata
genTxMetadata =
    fmap (TxMetadata . Map.fromList) $ do
        listOf
            ( ((,) . getLarge <$> arbitrary)
                <*> genTxMetadataValue
            )

genTxMetadataValue :: Gen TxMetadataValue
genTxMetadataValue =
    sized $ \sz ->
        frequency
            [ (2, TxMetaNumber <$> genTxMetaNumber)
            , (2, TxMetaBytes <$> genTxMetaBytes)
            , (2, TxMetaText <$> genTxMetaText)
            ,
                ( sz `div` 4
                , TxMetaList <$> scale (`div` 4) genTxMetaList
                )
            ,
                ( sz `div` 4
                , TxMetaMap <$> scale (`div` 4) genTxMetaMap
                )
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
        vectorOf
            n
            ((,) <$> genTxMetadataValue <*> genTxMetadataValue)

genPtr :: Gen Ptr
genPtr = Ledger.Ptr <$> genSlotNo32 <*> genTxIx <*> genCertIx

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
genAddressInEra era = case forEraMaybeEon era of
    Nothing -> byronAddressInEra <$> genAddressByron
    Just supported ->
        oneof
            [ byronAddressInEra <$> genAddressByron
            , shelleyAddressInEra supported <$> genAddressShelley
            ]

genUnsignedQuantity :: Gen Quantity
genUnsignedQuantity = do
    n <- arbitrary @Int
    frequency
        [ (30, pure $ abs $ fromIntegral n)
        , (30, fromIntegral @Integer <$> choose (0, 1_000_000_000_000))
        , (30, fromIntegral <$> arbitrary @Word64)
        ]

genTxOutValue :: CardanoEra era -> Gen (TxOutValue era)
genTxOutValue era = case era of
    ConwayEra -> TxOutValueShelleyBased ShelleyApi.ShelleyBasedEraConway
        . Api.toLedgerValue @ConwayEra MaryEraOnwardsConway
        <$> genValueForTxOut
    BabbageEra -> TxOutValueShelleyBased ShelleyApi.ShelleyBasedEraBabbage
        . Api.toLedgerValue @BabbageEra MaryEraOnwardsBabbage
        <$> genValueForTxOut
    _ -> error "genTxOutValue: unsupported era"

genTxOut :: CardanoEra era -> Gen (TxOut ctx era)
genTxOut era =
    TxOut
        <$> genAddressInEra era
        <*> genTxOutValue era
        <*> genTxOutDatum era
        <*> genReferenceScript era

genTxOutDatum :: CardanoEra era -> Gen (TxOutDatum ctx era)
genTxOutDatum era = withEraWitness era $ \supported ->
    oneof
        [ pure TxOutDatumNone
        , TxOutDatumHash supported <$> genHashScriptData
        ]

genReferenceScript :: CardanoEra era -> Gen (ReferenceScript era)
genReferenceScript era = withEraWitness era $ \supported ->
    oneof
        [ pure ReferenceScriptNone
        , ReferenceScript supported <$> genScriptInAnyLang (Just era)
        ]

mkDummyHash :: forall h a. (Crypto.HashAlgorithm h) => Int -> Crypto.Hash h a
mkDummyHash = coerce . Crypto.hashWithSerialiser @h CBOR.toCBOR

genHashScriptData :: Gen (Cardano.Api.Hash ScriptData)
genHashScriptData =
    ScriptDataHash . unsafeMakeSafeHash . mkDummyHash
        <$> (scale (`mod` 10) arbitrary)

genNat :: Gen Natural
genNat = do
    Large (n :: Word64) <- arbitrary
    pure $ fromIntegral n

genLarge :: (Integral a, Bounded a) => Gen a
genLarge = do
    Large (n :: a) <- arbitrary
    pure n

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

genCostModel :: Language -> Gen CostModel
genCostModel language = do
    let costModelParamsValues = case language of
            PlutusV1 -> snd <$> V1.costModelParamsForTesting
            PlutusV2 -> snd <$> V2.costModelParamsForTesting
            PlutusV3 -> snd <$> V3.costModelParamsForTesting

    eCostModel <-
        Alonzo.mkCostModel language
            <$> mapM (const $ fromIntegral <$> chooseInteger (0, 5_000))
                costModelParamsValues
    case eCostModel of
        Left err -> error $ "genCostModel: " ++ show err
        Right cModel -> return . CostModel $ Alonzo.getCostModelParams cModel

plutusLanguages :: [Language]
plutusLanguages = [PlutusV1, PlutusV2, PlutusV3]

genPlutusLanguage :: Gen Language
genPlutusLanguage = elements plutusLanguages

genCostModels :: Gen (Map AnyPlutusScriptVersion CostModel)
genCostModels = do
    n <- chooseInt (0, length plutusLanguages)
    costModels <-
        for (take n plutusLanguages) $ \lang ->
            (fromLanguage lang,) <$> genCostModel lang
    pure $ Map.fromList costModels
  where
    fromLanguage :: Language -> AnyPlutusScriptVersion
    fromLanguage = toEnum . fromEnum

genExecutionUnitPrices :: Gen ExecutionUnitPrices
genExecutionUnitPrices = ExecutionUnitPrices <$> genRational <*> genRational

-- | Dummy value suitable for being included in the pre-image of the script
-- integrity hash.
{-# NOINLINE protocolParametersForHashing #-}
protocolParametersForHashing
    :: ShelleyBasedEra era
    -> LedgerProtocolParameters era
protocolParametersForHashing = \case
    ShelleyBasedEraShelley -> LedgerProtocolParameters emptyPParams
    ShelleyBasedEraAllegra -> LedgerProtocolParameters emptyPParams
    ShelleyBasedEraMary -> LedgerProtocolParameters emptyPParams
    ShelleyBasedEraAlonzo -> LedgerProtocolParameters emptyPParams
    ShelleyBasedEraBabbage -> LedgerProtocolParameters emptyPParams
    ShelleyBasedEraConway -> LedgerProtocolParameters emptyPParams

genValidProtocolVersion :: Gen (Natural, Natural)
genValidProtocolVersion = do
    major <- fromIntegral @Int <$> choose (0, 9)
    minor <- genNat
    pure (major, minor)

genInterval :: Gen Ledger.EpochInterval
genInterval = Ledger.EpochInterval <$> arbitrary

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
genMIRPot = elements [ReservesMIR, TreasuryMIR]

genMIRTarget :: Gen (Ledger.MIRTarget)
genMIRTarget = error "TODO conway: genMIRTarget"

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
        scheme <-
            elements
                [ "http://"
                , "https://"
                ]
        host <- T.pack <$> vectorOf 10 genAlphaNum
        domain <-
            elements
                [ ".com"
                , ".net"
                , ".org"
                ]
        elements
            [ ""
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
            Left err ->
                error
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
    genLedgerStakePoolRelay =
        oneof
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
        case Ledger.textToDns 64 txt of
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
        <*> genCoin
        <*> genRational
        <*> genStakeAddress
        <*> genCoin
        <*> scale (`div` 3) (listOf (genVerificationKeyHash AsStakeKey))
        <*> scale (`div` 3) (listOf genStakePoolRelay)
        <*> liftArbitrary genStakePoolMetadataReference

genTxCertificate :: forall era. CardanoEra era -> Gen (Certificate era)
genTxCertificate era =
    oneof
        [ mkStakeAddressRegistrationCertificate
            <$> genCoin
            <*> genStakeCredential
        , mkStakeAddressUnregistrationCertificate
            <$> genCoin
            <*> genStakeCredential
        , genStakeAddressPoolDelegationCertificate
        , mkPoolRegistrationCertificate <$> genStakePoolParameters
        , mkPoolRetirementCertificate
            <$> genPoolId
            <*> genEpochNo
            -- MIR and Genesis Key Certs not generated, but not that important
        ]
  where
    mkStakeAddressRegistrationCertificate
        :: Lovelace -> StakeCredential -> Certificate era
    mkStakeAddressRegistrationCertificate deposit cred = case era of
        ConwayEra ->
            makeStakeAddressRegistrationCertificate
                $ StakeAddrRegistrationConway
                    ConwayEraOnwardsConway
                    deposit
                    cred
        BabbageEra ->
            makeStakeAddressRegistrationCertificate
                $ StakeAddrRegistrationPreConway
                    ShelleyToBabbageEraBabbage
                    cred
        _ -> error "genTxCertificate: unsupported era"

    mkStakeAddressUnregistrationCertificate
        :: Lovelace -> StakeCredential -> Certificate era
    mkStakeAddressUnregistrationCertificate deposit cred = case era of
        ConwayEra ->
            makeStakeAddressUnregistrationCertificate
                $ StakeAddrRegistrationConway
                    ConwayEraOnwardsConway
                    deposit
                    cred
        BabbageEra ->
            makeStakeAddressUnregistrationCertificate
                $ StakeAddrRegistrationPreConway
                    ShelleyToBabbageEraBabbage
                    cred
        _ -> error "genTxCertificate: unsupported era"

    genStakeAddressPoolDelegationCertificate
        :: Gen (Certificate era)
    genStakeAddressPoolDelegationCertificate = case era of
        ConwayEra ->
            makeStakeAddressDelegationCertificate
                <$> ( ( StakeDelegationRequirementsConwayOnwards
                            ConwayEraOnwardsConway
                      )
                        <$> genStakeCredential
                        <*> genConwayDelegatee
                    )
        BabbageEra ->
            makeStakeAddressDelegationCertificate
                <$> ( ( StakeDelegationRequirementsPreConway
                            ShelleyToBabbageEraBabbage
                      )
                        <$> genStakeCredential
                        <*> genPoolId
                    )
        _ -> error "genTxCertificate: unsupported era"

    mkPoolRegistrationCertificate
        :: StakePoolParameters -> Certificate era
    mkPoolRegistrationCertificate params = case era of
        ConwayEra ->
            makeStakePoolRegistrationCertificate
                $ StakePoolRegistrationRequirementsConwayOnwards
                    ConwayEraOnwardsConway
                    (toShelleyPoolParams params)
        BabbageEra ->
            makeStakePoolRegistrationCertificate
                $ StakePoolRegistrationRequirementsPreConway
                    ShelleyToBabbageEraBabbage
                    (toShelleyPoolParams params)
        _ -> error "genTxCertificate: unsupported era"

    mkPoolRetirementCertificate
        :: PoolId -> EpochNo -> Certificate era
    mkPoolRetirementCertificate p e = case era of
        ConwayEra ->
            makeStakePoolRetirementCertificate
                $ StakePoolRetirementRequirementsConwayOnwards
                    ConwayEraOnwardsConway
                    p
                    e
        BabbageEra ->
            makeStakePoolRetirementCertificate
                $ StakePoolRetirementRequirementsPreConway
                    ShelleyToBabbageEraBabbage
                    p
                    e
        _ -> error "genTxCertificate: unsupported era"

genTxCertificates
    :: CardanoEra era -> Gen (TxCertificates BuildTx era)
genTxCertificates era = withEraWitness era $ \sbe -> do
    let stakingCore =
            (,)
                <$> genStakeCredential
                <*> genWitnessStake era
        staking = BuildTxWith <$> oneof [pure Nothing, Just <$> stakingCore]
        stakingCertificate = do
            cert <- genTxCertificate era
            stake <- staking
            pure (cert, stake)
        certificates =
            scale (`div` 3)
                $ TxCertificates sbe . OMap.fromList
                    <$> listOf stakingCertificate
    oneof
        [ pure TxCertificatesNone
        , certificates
        ]

genProtocolParametersUpdate :: Gen ProtocolParametersUpdate
genProtocolParametersUpdate = do
    protocolUpdateProtocolVersion <-
        liftArbitrary genValidProtocolVersion
    protocolUpdateDecentralization <-
        liftArbitrary genRational
    protocolUpdateExtraPraosEntropy <-
        liftArbitrary (liftArbitrary genPraosNonce)
    protocolUpdateMaxBlockHeaderSize <-
        liftArbitrary genLarge
    protocolUpdateMaxBlockBodySize <-
        liftArbitrary genLarge
    protocolUpdateMaxTxSize <-
        liftArbitrary genLarge
    protocolUpdateTxFeeFixed <-
        liftArbitrary genCoin
    protocolUpdateTxFeePerByte <-
        liftArbitrary genCoin
    protocolUpdateMinUTxOValue <-
        liftArbitrary genCoin
    protocolUpdateStakeAddressDeposit <-
        liftArbitrary genCoin
    protocolUpdateStakePoolDeposit <-
        liftArbitrary genCoin
    protocolUpdateMinPoolCost <-
        liftArbitrary genCoin
    protocolUpdatePoolRetireMaxEpoch <-
        liftArbitrary genInterval
    protocolUpdateStakePoolTargetNum <- fmap fromIntegral <$>
        liftArbitrary genNat
    protocolUpdatePoolPledgeInfluence <-
        liftArbitrary genRational
    protocolUpdateMonetaryExpansion <-
        liftArbitrary genRational
    protocolUpdateTreasuryCut <-
        liftArbitrary genRational
    protocolUpdateUTxOCostPerByte <-
        liftArbitrary genCoin
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

    pure
        $ ProtocolParametersUpdate
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
genUpdateProposal era = case forEraMaybeEon era of
    Nothing -> pure TxUpdateProposalNone
    Just (supported :: ShelleyToBabbageEra era) ->
        frequency
            [ (95, pure TxUpdateProposalNone)
            ,
                ( 5
                , TxUpdateProposal supported
                    <$> ( ( UpdateProposal . Map.fromList
                                <$> scale
                                    (`div` 3)
                                    ( listOf
                                        ( (,)
                                            <$> genVerificationKeyHash AsGenesisKey
                                            <*> genProtocolParametersUpdate
                                        )
                                    )
                          )
                            <*> genEpochNo
                        )
                )
            ]

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genFeaturedInEra
    :: (Functor f)
    => eon era
    -> f a
    -> f (Featured eon era a)
genFeaturedInEra witness gen =
    Featured witness <$> gen

-- | Generate a 'Featured' for the given 'CardanoEra' with the provided generator.
genMaybeFeaturedInEra
    :: (Applicative f)
    => (Eon eon)
    => (eon era -> f a)
    -> CardanoEra era
    -> f (Maybe (Featured eon era a))
genMaybeFeaturedInEra f =
    inEonForEra (pure Nothing) $ \w ->
        fmap Just (genFeaturedInEra w (f w))

genProposals :: forall era. ConwayEraOnwards era -> Gen (ShelleyApi.TxProposalProcedures BuildTx era)
genProposals w = case w of
    ConwayEraOnwardsConway ->
        oneof
            [ pure TxProposalProceduresNone
            , TxProposalProcedures . fromList <$> genProcedures
            ]
        where
            genProcedures = listOf $ do
                procedure <- arbitrary
                pure (procedure, BuildTxWith Nothing)

genVotingProcedures :: ConwayEraOnwards era -> Gen (ShelleyApi.TxVotingProcedures BuildTx era)
genVotingProcedures w = case w of
    ConwayEraOnwardsConway ->
        oneof
            [ TxVotingProcedures <$> arbitrary <*> pure (BuildTxWith mempty)
            , pure TxVotingProceduresNone
            ]

genTxBodyContent :: CardanoEra era -> Gen (TxBodyContent BuildTx era)
genTxBodyContent era = withEraWitness era $ \sbe -> do
    txIns <- scale (`div` 3) $ do
        txIns <- listOf1 genTxIn
        ctxs <- vectorOf (length txIns) (genWitnessSpend era)
        pure $ zip txIns (BuildTxWith <$> ctxs)
    txOuts <- scale (`div` 3) $ listOf1 $ genTxOut era
    txFee <- genTxFee era
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
    txValidityLowerBound <- genTxValidityLowerBound era
    txValidityUpperBound <- genTxValidityUpperBound era
    txProposalProcedures <- genMaybeFeaturedInEra genProposals era
    txVotingProcedures <- genMaybeFeaturedInEra genVotingProcedures era
    txCurrentTreasuryValue <-
        genMaybeFeaturedInEra (const (pure <$> genCoin)) era
    txTreasuryDonation <- genMaybeFeaturedInEra (const genCoin) era
    let
        txBody =
            TxBodyContent
                { Api.txIns
                , Api.txOuts
                , -- NOTE: We are adding collateral at a later step, despite only
                  -- generating @TxInsCollateralNone@ here. This seems to be because
                  -- the generation currently is dependent on
                  -- @collectTxBodyScriptWitnesses txBody@.
                  Api.txInsCollateral = TxInsCollateralNone
                , -- TODO add proper generator, perhaps as part of ADP-1655
                  Api.txInsReference = TxInsReferenceNone
                , Api.txTotalCollateral
                , Api.txReturnCollateral
                , Api.txFee
                , -- , Api.txValidityRange
                  Api.txMetadata
                , Api.txAuxScripts
                , Api.txExtraKeyWits
                , Api.txProtocolParams = BuildTxWith Nothing
                , Api.txWithdrawals
                , Api.txCertificates
                , Api.txUpdateProposal
                , Api.txMintValue
                , Api.txScriptValidity
                , Api.txValidityLowerBound
                , Api.txValidityUpperBound
                , Api.txProposalProcedures
                , Api.txVotingProcedures
                , Api.txCurrentTreasuryValue
                , Api.txTreasuryDonation
                }

    let witnesses = collectTxBodyScriptWitnesses sbe txBody
        pparams = BuildTxWith $ Just $ protocolParametersForHashing sbe
    -- No use of a script language means no need for collateral
    if Set.null (languages witnesses)
        then do
            collateral <- genTxInsCollateral era
            pure
                txBody
                    { Api.txProtocolParams = pparams
                    , Api.txInsCollateral = collateral
                    }
        else do
            collateral <-
                case forEraMaybeEon era of
                    Nothing -> pure TxInsCollateralNone
                    Just supported ->
                        TxInsCollateral supported
                            <$> frequency
                                [ (95, return [])
                                , (5, listOf genTxIn)
                                ]
            pure
                txBody
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

genTxBody :: CardanoEra era -> Gen (TxBody era)
genTxBody era = withEraWitness era $ \se -> do
    res <- createTransactionBody se <$> genTxBodyContent era
    case res of
        Left err -> error (displayError err)
        Right txBody -> pure txBody

displayError :: TxBodyError -> String
displayError = show

-- | Similar to 'genTxBody', but with a distribution better suitable for testing
-- balancing.
genTxBodyForBalancing :: ShelleyApi.IsShelleyBasedEra era => CardanoEra era -> Gen (TxBody era)
genTxBodyForBalancing era = withEraWitness era $ \se -> do
    content <- genStrippedContent
    case createTransactionBody se content of
        Left err -> error (displayError err <> "\n" <> show content)
        Right txBody -> pure txBody
  where
    genStrippedContent = do
        content <- genTxBodyContent era
        genShouldStrip >>= \case
            True ->
                pure
                    $ content
                        { txInsCollateral = case txInsCollateral content of
                            TxInsCollateralNone -> TxInsCollateralNone
                            TxInsCollateral colInEra _ -> TxInsCollateral colInEra []
                        }
            False -> pure content
    genShouldStrip = frequency [(100, pure True), (0, pure False)]

genWitnesses :: CardanoEra era -> TxBody era -> Gen [KeyWitness era]
genWitnesses era body =
    case forEraMaybeEon era of
        Nothing -> error "unsupported era"
        Just se -> do
            let
                genShelley =
                    makeShelleyKeyWitness se body <$> genShelleyWitnessSigningKey
                genBootstrap =
                    makeShelleyBootstrapWitness se
                        <$> genWitnessNetworkIdOrByronAddress
                        <*> pure body
                        <*> genSigningKey AsByronKey
            bsWits <-
                frequency
                    [ (3, scale (`div` 3) $ listOf1 genBootstrap)
                    , (1, pure [])
                    ]
            keyWits <-
                frequency
                    [ (3, scale (`div` 3) $ listOf1 genShelley)
                    , (1, pure [])
                    ]
            return $ bsWits ++ keyWits

genWitness :: CardanoEra era -> TxBody era -> Gen (KeyWitness era)
genWitness era body =
    case forEraMaybeEon era of
        Nothing -> error "unsupported era"
        Just se ->
            oneof
                [ makeShelleyBootstrapWitness se
                    <$> genWitnessNetworkIdOrByronAddress
                    <*> pure body
                    <*> genSigningKey AsByronKey
                , makeShelleyKeyWitness se body <$> genShelleyWitnessSigningKey
                ]

genTxInEra :: forall era. CardanoEra era -> Gen (Tx era)
genTxInEra era = do
    body <- genTxBody era
    makeSignedTransaction
        <$> genWitnesses era body
        <*> pure body

genTx :: Gen (InAnyCardanoEra Tx)
genTx =
    oneof
        [ InAnyCardanoEra ByronEra <$> genTxInEra ByronEra
        , InAnyCardanoEra ShelleyEra <$> genTxInEra ShelleyEra
        , InAnyCardanoEra MaryEra <$> genTxInEra MaryEra
        , InAnyCardanoEra AllegraEra <$> genTxInEra AllegraEra
        , InAnyCardanoEra AlonzoEra <$> genTxInEra AlonzoEra
        , InAnyCardanoEra BabbageEra <$> genTxInEra BabbageEra
        ]

-- TODO: Generate txs with no inputs
-- TODO: Generate txs with existing key witnesses
-- TODO: Generate txs with no outputs
genTxForBalancing :: ShelleyApi.IsShelleyBasedEra era => CardanoEra era -> Gen (Tx era)
genTxForBalancing era = makeSignedTransaction [] <$> genTxBodyForBalancing era

--------------------------------------------------------------------------------
-- Assisting ledger generators
--------------------------------------------------------------------------------

genKeyHash :: Gen (Ledger.KeyHash keyRole)
genKeyHash =
    Ledger.KeyHash
        . fromMaybe (error "genKeyHash: invalid hash")
        . Crypto.hashFromBytes
        . BS.pack
        <$> vectorOf 28 arbitrary

genCredential :: Gen (Ledger.Credential keyRole)
genCredential =
    oneof
        [ Ledger.KeyHashObj <$> genKeyHash
        , Ledger.ScriptHashObj <$> genLedgerScriptHash
        ]

genLedgerScriptHash :: Gen (Ledger.ScriptHash)
genLedgerScriptHash =
    Ledger.ScriptHash
        . fromMaybe (error "genKeyHash: invalid hash")
        . Crypto.hashFromBytes
        . BS.pack
        <$> vectorOf 28 arbitrary

genConwayDelegatee :: Gen (Ledger.Delegatee)
genConwayDelegatee =
    oneof
        [ Ledger.DelegStake <$> genKeyHash
        , Ledger.DelegVote <$> genDRep
        , Ledger.DelegStakeVote <$> genKeyHash <*> genDRep
        ]

genDRep :: Gen (Ledger.DRep)
genDRep =
    oneof
        [ DRepCredential <$> genCredential
        , pure DRepAlwaysAbstain
        , pure DRepAlwaysNoConfidence
        ]
