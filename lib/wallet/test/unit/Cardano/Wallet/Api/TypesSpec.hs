{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Api.TypesSpec (spec) where

import Prelude hiding
    ( id )

import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , KeyRole (..)
    , Script (..)
    , ScriptHash (..)
    , ScriptTemplate (..)
    , ValidationLevel (..)
    )
import Cardano.Api
    ( StakeAddress, deserialiseFromRawBytes, proxyToAsType )
import Cardano.Api.Gen
    ( genAddressAnyWithNetworkId )
import Cardano.Mnemonic
    ( CheckSumBits
    , ConsistentEntropy
    , Entropy
    , EntropySize
    , MnemonicException (..)
    , SomeMnemonic (..)
    , ValidChecksumSize
    , ValidEntropySize
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.Pool.Metadata
    ( HealthCheckSMASH )
import Cardano.Pool.Metadata.Types
import Cardano.Pool.Types
    ( PoolId (..), PoolOwner (..), StakePoolTicker (..) )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , Role (..)
    , WalletKey (..)
    , fromHex
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( purposeCIP1854 )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Address.DerivationSpec
    ()
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap, getAddressPoolGap, purposeCIP1852 )
import Cardano.Wallet.Address.Discovery.Shared
    ( retrieveAllCosigners )
import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Types
    ( AccountPostData (..)
    , AddressAmount (..)
    , AddressAmountNoAssets (..)
    , AnyAddress (..)
    , ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiAccountPublicKey (..)
    , ApiAccountSharedPublicKey (..)
    , ApiActiveSharedWallet (..)
    , ApiAddress
    , ApiAddressData (..)
    , ApiAddressDataPayload (..)
    , ApiAddressInspect (..)
    , ApiAddressWithPath (..)
    , ApiAnyCertificate (..)
    , ApiAsArray (..)
    , ApiAsset (..)
    , ApiAssetMintBurn (..)
    , ApiBalanceTransactionPostData (..)
    , ApiBase64
    , ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiBurnData (..)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiBytesT (..)
    , ApiCertificate (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionChange (..)
    , ApiCoinSelectionCollateral (..)
    , ApiCoinSelectionOutput (..)
    , ApiCoinSelectionWithdrawal (..)
    , ApiConstructTransaction (..)
    , ApiConstructTransactionData (..)
    , ApiCosignerIndex (..)
    , ApiCredential (..)
    , ApiCredentialType (..)
    , ApiDecodedTransaction (..)
    , ApiDelegationAction (..)
    , ApiDeregisterPool (..)
    , ApiEra (..)
    , ApiEraInfo (..)
    , ApiExternalCertificate (..)
    , ApiExternalInput (..)
    , ApiFee (..)
    , ApiForeignStakeKey
    , ApiHealthCheck (..)
    , ApiIncompleteSharedWallet (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , ApiMintBurnData (..)
    , ApiMintBurnOperation (..)
    , ApiMintData (..)
    , ApiMnemonicT (..)
    , ApiMultiDelegationAction (..)
    , ApiNetworkClock (..)
    , ApiNetworkInfo (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiNullStakeKey
    , ApiOurStakeKey
    , ApiPaymentDestination (..)
    , ApiPolicyId (..)
    , ApiPolicyKey (..)
    , ApiPostAccountKeyData (..)
    , ApiPostAccountKeyDataWithPurpose
    , ApiPostPolicyIdData (..)
    , ApiPostPolicyKeyData (..)
    , ApiPostRandomAddressData
    , ApiPutAddressesData (..)
    , ApiRedeemer (..)
    , ApiRegisterPool (..)
    , ApiScriptTemplate (..)
    , ApiScriptTemplateEntry (..)
    , ApiSealedTxEncoding (..)
    , ApiSelectCoinsAction (..)
    , ApiSelectCoinsData (..)
    , ApiSelectCoinsPayments (..)
    , ApiSelfWithdrawalPostData (..)
    , ApiSerialisedTransaction (..)
    , ApiSharedWallet (..)
    , ApiSharedWalletPatchData (..)
    , ApiSharedWalletPostData (..)
    , ApiSharedWalletPostDataFromAccountPubX (..)
    , ApiSharedWalletPostDataFromMnemonics (..)
    , ApiSignTransactionPostData (..)
    , ApiSlotId (..)
    , ApiSlotReference (..)
    , ApiStakeKeyIndex (..)
    , ApiStakeKeys
    , ApiT (..)
    , ApiTokenAmountFingerprint (..)
    , ApiTokens (..)
    , ApiTransaction (..)
    , ApiTxCollateral (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutputGeneral (..)
    , ApiUtxoStatistics (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeyShelley (..)
    , ApiWallet (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletBalance (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletDiscovery (..)
    , ApiWalletInput (..)
    , ApiWalletMigrationBalance (..)
    , ApiWalletMigrationPlan (..)
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletMode
    , ApiWalletOutput (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWalletSignData (..)
    , ApiWalletUtxoSnapshot (..)
    , ApiWalletUtxoSnapshotEntry (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalGeneral (..)
    , ApiWithdrawalPostData (..)
    , Base (Base16, Base64)
    , ByronWalletFromXPrvPostData (..)
    , ByronWalletPostData (..)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , KeyFormat (..)
    , NtpSyncingStatus (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , ResourceContext (..)
    , SettingsPutData (..)
    , SomeByronWalletPostData (..)
    , VerificationKeyHashing (..)
    , WalletOrAccountPostData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , WalletPutPassphraseMnemonicData (..)
    , WalletPutPassphraseOldPassphraseData (..)
    , XPubOrSelf (..)
    , toApiAsset
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader )
import Cardano.Wallet.Api.Types.Certificate
    ( ApiRewardAccount (..) )
import Cardano.Wallet.Api.Types.Error
    ( ApiError (..)
    , ApiErrorBalanceTxUnderestimatedFee (..)
    , ApiErrorInfo (..)
    , ApiErrorMessage (..)
    , ApiErrorNodeNotYetInRecentEra (..)
    , ApiErrorSharedWalletNoSuchCosigner (..)
    , ApiErrorTxOutputLovelaceInsufficient (..)
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..), TxMetadataWithSchema (..) )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiAddress (..)
    , ApiValidityIntervalExplicit (..)
    , ApiWitnessCount (..)
    , mkApiWitnessCount
    )
import Cardano.Wallet.Gen
    ( genMnemonic
    , genMockXPub
    , genNestedTxMetadata
    , genPercentage
    , genScript
    , genScriptCosigners
    , genScriptTemplate
    , shrinkPercentage
    , shrinkTxMetadata
    )
import Cardano.Wallet.Pools
    ( EpochInfo (..)
    , StakePool (StakePool)
    , StakePoolFlag
    , StakePoolMetrics (StakePoolMetrics)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (PassphraseHash)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , passphraseMaxLength
    , passphraseMinLength
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , ExecutionUnitPrices (..)
    , NonWalletCertificate (..)
    , PoolMetadataSource
    , Settings
    , SlotId (..)
    , SlotInEpoch (..)
    , SlotNo (..)
    , SmashServer
    , SortOrder (..)
    , StartTime (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletName (..)
    , walletNameMaxLength
    , walletNameMinLength
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( AssetId (..), TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetId, genTokenMapSmallRange, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( AssetDecimals (..)
    , AssetLogo (..)
    , AssetMetadata (..)
    , AssetURL (..)
    , TokenFingerprint
    , TokenName (..)
    , TokenPolicyId (..)
    , mkTokenFingerprint
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenName )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , SerialisedTx (..)
    , TxMetadata (..)
    , TxScriptValidity (..)
    , TxStatus (..)
    , unsafeSealedTxFromBytes
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxScriptValidity, shrinkTxScriptValidity )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutCoin )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( HistogramBar (..), UTxOStatistics (..) )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminant (..), networkIdVal )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataError (..) )
import Cardano.Wallet.Transaction
    ( AnyExplicitScript (..)
    , AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , ReferenceInput (..)
    , ScriptReference (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromText, unsafeXPrv )
import Cardano.Wallet.Write.Tx.Gen
    ( genDatumHash )
import Control.Lens
    ( at, (?~) )
import Control.Monad
    ( forM, forM_, replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Crypto.Hash
    ( hash )
import Data.Aeson
    ( FromJSON (..)
    , Result (..)
    , ToJSON (..)
    , fromJSON
    , withObject
    , (.:?)
    , (.=)
    )
import Data.Aeson.KeyMap
    ( keys )
import Data.Aeson.QQ
    ( aesonQQ )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.ByteString
    ( ByteString )
import Data.Data
    ( dataTypeConstrs, dataTypeOf, showConstr )
import Data.Either
    ( lefts )
import Data.Either.Combinators
    ( fromRight' )
import Data.FileEmbed
    ( embedFile, makeRelativeToProject )
import Data.Function
    ( (&) )
import Data.List
    ( foldl', intercalate )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.OpenApi
    ( Definitions, NamedSchema (..), Schema, ToSchema (..) )
import Data.OpenApi.Declare
    ( Declare, declare, look )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..) )
import Data.Time.Clock
    ( NominalDiffTime )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32, Word64, Word8 )
import Data.Word.Odd
    ( Word31 )
import GHC.TypeLits
    ( KnownSymbol, natVal, symbolVal )
import Network.Ntp
    ( NtpStatusWithOffset (..) )
import Network.URI
    ( URI, parseURI )
import Numeric.Natural
    ( Natural )
import Servant
    ( (:<|>)
    , (:>)
    , Capture
    , Header'
    , JSON
    , PostNoContent
    , QueryFlag
    , QueryParam
    , ReqBody
    , StdMethod (..)
    , Verb
    )
import Servant.API.Verbs
    ( NoContentVerb )
import Servant.OpenApi.Test
    ( validateEveryToJSON, validateEveryToJSONWithPatternChecker )
import System.Environment
    ( lookupEnv )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Arbitrary1 (..)
    , Gen
    , InfiniteList (..)
    , applyArbitrary2
    , applyArbitrary3
    , arbitraryBoundedEnum
    , arbitraryPrintableChar
    , arbitrarySizedBoundedIntegral
    , choose
    , chooseInt
    , counterexample
    , elements
    , forAll
    , frequency
    , liftArbitrary
    , listOf
    , oneof
    , property
    , scale
    , shrinkIntegral
    , shrinkMapBy
    , sized
    , suchThat
    , vector
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( reasonablySized, shrinkBoundedEnum )
import Test.QuickCheck.Gen
    ( sublistOf )
import Test.QuickCheck.Instances
    ()
import Test.QuickCheck.Modifiers
    ( NonNegative (..) )
import Test.Text.Roundtrip
    ( textRoundtrip )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Roundtrip
    ( httpApiDataRoundtrip )
import Test.Utils.Time
    ( genUniformTime )
import Text.Regex.PCRE
    ( compBlank, execBlank, makeRegexOpts, matchTest )
import Web.HttpApiData
    ( FromHttpApiData (..) )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Yaml as Yaml
import qualified Prelude
import qualified Test.Utils.Roundtrip as Utils

type T0 = 'Testnet 0

spec :: Spec
spec = do

    let jsonTestDataPath :: FilePath
        jsonTestDataPath = ($(getTestData) </> "Cardano" </> "Wallet" </> "Api")

    let jsonTest
            :: forall a. (Arbitrary a, Typeable a, ToJSON a, FromJSON a)
            => Spec
        jsonTest = Utils.jsonRoundtripAndGolden jsonTestDataPath (Proxy @a)

    describe "JSON golden roundtrip" $ do
        jsonTest @(AddressAmount (ApiAddress T0))
        jsonTest @(AddressAmountNoAssets (ApiAddress T0))
        jsonTest @(ApiAsArray "Test" (Maybe Word64))
        jsonTest @(ApiBalanceTransactionPostData T0)
        jsonTest @(ApiCoinSelection T0)
        jsonTest @(ApiCoinSelectionChange T0)
        jsonTest @(ApiCoinSelectionCollateral T0)
        jsonTest @(ApiCoinSelectionOutput T0)
        jsonTest @(ApiCoinSelectionWithdrawal T0)
        jsonTest @(ApiConstructTransaction T0)
        jsonTest @(ApiConstructTransactionData T0)
        jsonTest @(ApiDecodedTransaction T0)
        jsonTest @(ApiExternalInput T0)
        jsonTest @(ApiForeignStakeKey T0)
        jsonTest @(ApiOurStakeKey T0)
        jsonTest @(ApiPaymentDestination T0)
        jsonTest @(ApiPutAddressesData T0)
        jsonTest @(ApiSelectCoinsData T0)
        jsonTest @(ApiStakeKeys T0)
        jsonTest @(ApiT (Hash "Genesis"))
        jsonTest @(ApiT (Hash "Tx"))
        jsonTest @(ApiT (Passphrase "lenient"))
        jsonTest @(ApiT (Passphrase "user"))
        jsonTest @(ApiAddress T0)
        jsonTest @(ApiT AddressPoolGap)
        jsonTest @(ApiT DerivationIndex)
        jsonTest @(ApiT Direction)
        jsonTest @(ApiT StakePool)
        jsonTest @(ApiT StakePoolMetadata)
        jsonTest @(ApiT StakePoolMetrics)
        jsonTest @(ApiT SyncProgress)
        jsonTest @(ApiT TxMetadata)
        jsonTest @(ApiT TxScriptValidity)
        jsonTest @(ApiT TxStatus)
        jsonTest @(ApiT WalletId)
        jsonTest @(ApiT WalletName)
        jsonTest @(ApiTransaction T0)
        jsonTest @(ApiTxInputGeneral T0)
        jsonTest @(ApiTxOutputGeneral T0)
        jsonTest @(ApiWalletBalance)
        jsonTest @(ApiWalletMigrationPlan T0)
        jsonTest @(ApiWalletMigrationPlanPostData T0)
        jsonTest @(ApiWalletMigrationPostData T0 "lenient")
        jsonTest @(ApiWalletMigrationPostData T0 "user")
        jsonTest @(ApiWithdrawalGeneral T0)
        jsonTest @(PostTransactionFeeOldData T0)
        jsonTest @(PostTransactionOldData T0)
        jsonTest @AccountPostData
        jsonTest @AnyAddress
        jsonTest @ApiAccountKey
        jsonTest @ApiAccountKeyShared
        jsonTest @ApiActiveSharedWallet
        jsonTest @ApiAddressData
        jsonTest @ApiAsset
        jsonTest @ApiAssetMintBurn
        jsonTest @ApiBase64
        jsonTest @ApiBlockReference
        jsonTest @ApiByronWallet
        jsonTest @ApiByronWalletBalance
        jsonTest @ApiCosignerIndex
        jsonTest @ApiCredential
        jsonTest @ApiCredentialType
        jsonTest @ApiDelegationAction
        jsonTest @ApiEra
        jsonTest @ApiEraInfo
        jsonTest @ApiError
        jsonTest @ApiErrorSharedWalletNoSuchCosigner
        jsonTest @ApiErrorTxOutputLovelaceInsufficient
        jsonTest @ApiErrorBalanceTxUnderestimatedFee
        jsonTest @ApiErrorNodeNotYetInRecentEra
        jsonTest @ApiFee
        jsonTest @ApiHealthCheck
        jsonTest @ApiIncompleteSharedWallet
        jsonTest @ApiMaintenanceAction
        jsonTest @ApiMaintenanceActionPostData
        jsonTest @ApiMultiDelegationAction
        jsonTest @ApiNetworkClock
        jsonTest @ApiNetworkInformation
        jsonTest @ApiNetworkParameters
        jsonTest @ApiNullStakeKey
        jsonTest @ApiPolicyId
        jsonTest @ApiPolicyKey
        jsonTest @ApiPostAccountKeyData
        jsonTest @ApiPostAccountKeyDataWithPurpose
        jsonTest @ApiPostPolicyIdData
        jsonTest @ApiPostRandomAddressData
        jsonTest @ApiScriptTemplateEntry
        jsonTest @ApiSerialisedTransaction
        jsonTest @ApiSharedWallet
        jsonTest @ApiSharedWalletPatchData
        jsonTest @ApiSharedWalletPostData
        jsonTest @ApiSharedWalletPostDataFromAccountPubX
        jsonTest @ApiSharedWalletPostDataFromMnemonics
        jsonTest @ApiSignTransactionPostData
        jsonTest @ApiSlotReference
        jsonTest @ApiTokenAmountFingerprint
        jsonTest @ApiTokens
        jsonTest @ApiTxId
        jsonTest @ApiTxMetadata
        jsonTest @ApiUtxoStatistics
        jsonTest @ApiValidityIntervalExplicit
        jsonTest @ApiVerificationKeyShared
        jsonTest @ApiVerificationKeyShelley
        jsonTest @ApiWallet
        jsonTest @ApiWalletDelegation
        jsonTest @ApiWalletDelegationNext
        jsonTest @ApiWalletDelegationStatus
        jsonTest @ApiWalletMigrationBalance
        jsonTest @ApiWalletPassphrase
        jsonTest @ApiWalletPassphraseInfo
        jsonTest @ApiWalletUtxoSnapshot
        jsonTest @ApiWitnessCount
        jsonTest @ByronWalletFromXPrvPostData
        jsonTest @ByronWalletPutPassphraseData
        jsonTest @SettingsPutData
        jsonTest @SomeByronWalletPostData
        jsonTest @TxMetadataWithSchema
        jsonTest @WalletOrAccountPostData
        jsonTest @WalletPostData
        jsonTest @WalletPutData
        jsonTest @WalletPutPassphraseData
        jsonTest @(ApiRewardAccount T0)
        jsonTest @(ApiExternalCertificate T0)

    describe "ApiEra roundtrip" $
        it "toApiEra . fromApiEra == id" $ property $ \era -> do
            Api.toApiEra (Api.fromApiEra era) === era

    describe "ToText-FromText Roundtrip" $ do
            textRoundtrip $ Proxy @Iso8601Time
            textRoundtrip $ Proxy @SortOrder
            textRoundtrip $ Proxy @Coin
            textRoundtrip $ Proxy @TokenFingerprint

    describe "SealedTx JSON decoding" $ do
        -- NOTE(AB): I tried to factor more of the properties as their structure only
        -- differs by the encoding but this required exporting 'HasBase' from Types to
        let parseJSONSealedTx jsonTx =
                (serialisedTx . getApiT  <$> Aeson.eitherDecode @(ApiT SealedTx) jsonTx)

        it "can decode from base-16 encoded string" $
            forAll selectFromPreparedBinaries $ \ bs ->
                 let result = parseJSONSealedTx $ Aeson.encode $ ApiBytesT @'Base16 bs
                  in result == Right bs &
                     counterexample ("Parse result: " <> show result)

        it "can decode from base-64 encoded string" $
            forAll selectFromPreparedBinaries $ \ bs ->
                 let result = parseJSONSealedTx $ Aeson.encode $ ApiBytesT @'Base64 bs
                  in result == Right bs &
                     counterexample ("Parse result: " <> show result)

    describe "AddressAmount" $ do
        it "fromText \"22323\"" $
            let err =
                    "Parse error. " <>
                    "Expecting format \"<amount>@<address>\" but got \"22323\""
            in
                fromText @(AddressAmount Text) "22323"
                    === Left (TextDecodingError err)

    describe "HttpApiData roundtrip" $ do
        httpApiDataRoundtrip $ Proxy @(ApiT PoolId)
        httpApiDataRoundtrip $ Proxy @(ApiT WalletId)
        httpApiDataRoundtrip $ Proxy @(ApiT AddressState)
        httpApiDataRoundtrip $ Proxy @Iso8601Time
        httpApiDataRoundtrip $ Proxy @(ApiT SortOrder)

    describe
        "verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using a matcher" $ do
        let match regex sourc = matchTest
                (makeRegexOpts compBlank execBlank $ T.unpack regex)
                (T.unpack sourc)
        validateEveryToJSONWithPatternChecker match (Proxy @(Api T0))

    describe
        "Verify that every type used with JSON content type in a servant API \
        \has compatible ToJSON and ToSchema instances using validateEveryToJSON" $
        validateEveryToJSON $
            Proxy @(
                ReqBody '[JSON] AccountPostData :> PostNoContent
              :<|>
                ReqBody '[JSON] WalletPostData  :> PostNoContent
            )

    describe
        "verify that every path specified by the servant server matches an \
        \existing path in the specification" $
        forM_ (everyApiEndpoint (Proxy @(Api T0))) $ \endpoint ->
        it (show endpoint <> " exists in specification") $ do
            let path = T.pack (apiEndpointPath endpoint)
                verb = apiEndpointVerb endpoint
            case foldl' unsafeLookupKey specification ["paths", path] of
                Aeson.Object obj -> do
                    let key = Aeson.fromString (Char.toLower <$> verb)
                    case Aeson.lookup key obj of
                        Just{} -> pure @IO ()
                        Nothing ->
                            fail $ "Path " <> show path
                                <> " doesn't allow method " <> show verb
                _ -> fail $
                    "couldn't find path " <> show path <> " in specification: "
                    <> show (unsafeLookupKey specification "paths" &
                        \(Aeson.Object m) -> keys m)

    describe "verify JSON parsing failures too" $ do

        describe "ApiAsArray (Maybe t)" $ do
            let parseApiAsArray = Aeson.parseEither $
                    parseJSON @(ApiAsArray "test_field" (Maybe Word64))
            let expectedFailureMessage =
                    "Error in $: Expected at most one item for \"test_field\"."
            it "element count = 0 (should succeed)" $
                parseApiAsArray [aesonQQ|[]|]
                    `shouldBe` Right (ApiAsArray Nothing)
            it "element count = 1 (should succeed)" $
                parseApiAsArray [aesonQQ|[0]|]
                    `shouldBe` Right (ApiAsArray (Just 0))
            it "element count = 2 (should fail)" $
                parseApiAsArray [aesonQQ|[0, 1]|]
                    `shouldBe` Left expectedFailureMessage
            it "element count = 3 (should fail)" $
                parseApiAsArray [aesonQQ|[0, 1, 2]|]
                    `shouldBe` Left expectedFailureMessage

        it "ApiT (Passphrase \"user\") (too short)" $ do
            let minLength = passphraseMinLength (Proxy :: Proxy "user")
            let msg = "Error in $: passphrase is too short: \
                    \expected at least " <> show minLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|"patate"|]
                `shouldBe` (Left @String @(ApiT (Passphrase "user")) msg)

        it "ApiT (Passphrase \"user\") (too long)" $ do
            let maxLength = passphraseMaxLength (Proxy :: Proxy "user")
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show maxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*maxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "user")) msg)

        it "ApiT (Passphrase \"lenient\") (too long)" $ do
            let maxLength = passphraseMaxLength (Proxy :: Proxy "lenient")
            let msg = "Error in $: passphrase is too long: \
                    \expected at most " <> show maxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*maxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT (Passphrase "lenient")) msg)

        it "ApiT WalletName (too short)" $ do
            let msg = "Error in $: name is too short: \
                    \expected at least " <> show walletNameMinLength <> " character"
            Aeson.parseEither parseJSON [aesonQQ|""|]
                `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiT WalletName (too long)" $ do
            let msg = "Error in $: name is too long: \
                    \expected at most " <> show walletNameMaxLength <> " characters"
            Aeson.parseEither parseJSON [aesonQQ|
                #{replicate (2*walletNameMaxLength) '*'}
            |] `shouldBe` (Left @String @(ApiT WalletName) msg)

        it "ApiMnemonicT '[12] (not enough words)" $ do
            let msg = "Error in $: Invalid number of words: 12 words\
                    \ are expected."
            Aeson.parseEither parseJSON [aesonQQ|
                ["toilet", "toilet", "toilet"]
            |] `shouldBe` (Left @String @(ApiMnemonicT '[12]) msg)

        it "ApiT DerivationIndex (too small)" $ do
            let message = unwords
                  [ "Error in $:"
                  , "A derivation index must be a natural number between"
                  , show (getIndex @'Soft minBound)
                  , "and"
                  , show (getIndex @'Soft maxBound)
                  , "with an optional 'H' suffix (e.g. '1815H' or '44')."
                  , "Indexes without suffixes are called 'Soft'"
                  , "Indexes with suffixes are called 'Hardened'."
                  ]

            let value = show $ pred $ toInteger $ getIndex @'Soft minBound
            Aeson.parseEither parseJSON [aesonQQ|#{value}|]
                `shouldBe` Left @String @(ApiT DerivationIndex) message

        it "ApiT DerivationIndex (too large)" $ do
            let message = unwords
                  [ "Error in $:"
                  , "A derivation index must be a natural number between"
                  , show (getIndex @'Soft minBound)
                  , "and"
                  , show (getIndex @'Soft maxBound)
                  , "with an optional 'H' suffix (e.g. '1815H' or '44')."
                  , "Indexes without suffixes are called 'Soft'"
                  , "Indexes with suffixes are called 'Hardened'."
                  ]

            let value = show $ succ $ toInteger $ getIndex @'Soft maxBound
            Aeson.parseEither parseJSON [aesonQQ|#{value}|]
                `shouldBe` Left @String @(ApiT DerivationIndex) message

        it "ApiT AddressPoolGap (too small)" $ do
            let msg = "Error in $: An address pool gap must be a natural number between "
                    <> show (getAddressPoolGap minBound)
                    <> " and "
                    <> show (getAddressPoolGap maxBound)
                    <> "."
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap minBound - 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT AddressPoolGap (too big)" $ do
            let msg = "Error in $: An address pool gap must be a natural number between "
                    <> show (getAddressPoolGap minBound)
                    <> " and "
                    <> show (getAddressPoolGap maxBound)
                    <> "."
            Aeson.parseEither parseJSON [aesonQQ|
                #{getAddressPoolGap maxBound + 1}
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT AddressPoolGap (not a integer)" $ do
            let msg = "Error in $: parsing Integer failed, unexpected floating number\
                    \ 2.5"
            Aeson.parseEither parseJSON [aesonQQ|
                2.5
            |] `shouldBe` (Left @String @(ApiT AddressPoolGap) msg)

        it "ApiT (Hash \"Tx\")" $ do
            let msg = "Error in $: Invalid tx hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "-----"
            |] `shouldBe` (Left @String @(ApiT (Hash "Tx")) msg)

        it "ApiT WalletId" $ do
            let msg = "Error in $: wallet id should be a hex-encoded \
                    \string of 40 characters"
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT WalletId) msg)

        it "AddressAmount (too small)" $ do
            let msg = "Error in $.amount.quantity: \
                    \parsing AddressAmount failed, parsing Natural failed, \
                    \unexpected negative number -14"
            Aeson.parseEither parseJSON [aesonQQ|
                { "address": "addr_test12rt90k38sak06mwtj6rfkgzpzw2nzgxtkp5675nxl6wqqc5p745g840sg6q73dcrf3w6up"
                , "amount": {"unit":"lovelace","quantity":-14}
                }
            |] `shouldBe` (Left @String @(AddressAmount (ApiAddress T0)) msg)

        it "AddressAmount (too big)" $ do
            let msg = "Error in $: parsing AddressAmount failed, \
                    \invalid coin value: value has to be lower \
                    \than or equal to " <> show (unCoin txOutMaxCoin)
                    <> " lovelace."
            Aeson.parseEither parseJSON [aesonQQ|
                { "address": "addr_test12rt90k38sak06mwtj6rfkgzpzw2nzgxtkp5675nxl6wqqc5p745g840sg6q73dcrf3w6up"
                , "amount":
                    { "unit":"lovelace"
                    ,"quantity":#{unCoin txOutMaxCoin + 1}
                    }
                }
            |] `shouldBe` (Left @String @(AddressAmount (ApiAddress T0)) msg)

        it "ApiT PoolId" $ do
            let msg =
                    "Error in $: Invalid stake pool id: expecting a Bech32 \
                    \encoded value with human readable part of 'pool'."
            Aeson.parseEither parseJSON [aesonQQ|
                "invalid-id"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT PoolId" $ do
            let msg =
                    "Error in $: Invalid stake pool id: expecting a Bech32 \
                    \encoded value with human readable part of 'pool'."
            Aeson.parseEither parseJSON [aesonQQ|
                "4c43d68b21921034519c36d2475f5adba989bb4465ec"
            |] `shouldBe` (Left @String @(ApiT PoolId) msg)

        it "ApiT (Hash \"Genesis\")" $ do
            let msg = "Error in $: Invalid genesis hash: \
                    \expecting a hex-encoded value that is 32 bytes in length."
            Aeson.parseEither parseJSON [aesonQQ|
                "-----"
            |] `shouldBe` (Left @String @(ApiT (Hash "Genesis")) msg)

        describe "StakePoolMetadata" $ do
            let msg = "Error in $.ticker: stake pool ticker length must be \
                      \3-5 characters"

            let testInvalidTicker :: Text -> SpecWith ()
                testInvalidTicker txt =
                    it ("Invalid ticker length: " ++ show (T.length txt)) $ do
                        Aeson.parseEither parseJSON [aesonQQ|
                            {
                                "owner": "ed25519_pk1afhcpw2tg7nr2m3wr4x8jaa4dv7d09gnv27kwfxpjyvukwxs8qdqwg85xp",
                                "homepage": "https://12345",
                                "ticker": #{txt},
                                "pledge_address": "ed25519_pk15vz9yc5c3upgze8tg5kd7kkzxqgqfxk5a3kudp22hdg0l2za00sq2ufkk7",
                                "name": "invalid"
                            }
                        |] `shouldBe` (Left @String @(ApiT StakePoolMetadata) msg)

            forM_ ["too long", "sh", ""] testInvalidTicker

    describe "verify HttpApiData parsing failures too" $ do
        it "ApiT WalletId" $ do
            let msg = "wallet id should be a hex-encoded string of 40 characters"
            parseUrlPiece "invalid-id"
                `shouldBe` (Left @Text @(ApiT WalletId) msg)

        it "ApiT AddressState" $ do
            let msg = "Unable to decode the given text value.\
                    \ Please specify one of the following values: used, unused."
            parseUrlPiece "patate"
                `shouldBe` (Left @Text @(ApiT AddressState) msg)

    describe "Api Errors" $ do
        it "Every ApiErrorInfo constructor has a corresponding schema type" $
            let res = fromJSON @SchemaApiErrorInfo specification
                errStr = case res of
                    Error s -> s
                    _ -> ""
            in counterexample errStr $ res == Success SchemaApiErrorInfo

{-------------------------------------------------------------------------------
                              Error type encoding
-------------------------------------------------------------------------------}

-- | We use this empty data type to define a custom JSON instance that checks
--   'ApiErrorInfo' has corresponding constructors in the schema file.
data SchemaApiErrorInfo = SchemaApiErrorInfo
    deriving (Show, Eq)

instance FromJSON SchemaApiErrorInfo where
    parseJSON = withObject "SchemaApiErrorInfo" $ \o -> do
        let constructors :: [String] =
                showConstr <$> dataTypeConstrs (dataTypeOf NoSuchWallet)
        vals :: [Either String Yaml.Value] <-
            forM constructors $ \c ->
                maybe (Left c) Right <$> o .:? Aeson.fromString (toSchemaName c)
        case lefts vals of
            [] -> pure SchemaApiErrorInfo
            xs -> fail $ unlines
                [ "Missing ApiErrorInfo constructors for:"
                , show xs
                , "Each of these need a corresponding swagger type of the form:"
                , "x-errConstructorName"
                ]
      where
        toSchemaName :: String -> String
        toSchemaName [] = []
        toSchemaName xs = "x-err" <> xs


{-------------------------------------------------------------------------------
                              Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (ApiRewardAccount n)
    where
        arbitrary = ApiRewardAccount <$> arbitrary

fromCardanoAddressAny :: Cardano.AddressAny -> Address
fromCardanoAddressAny =  Address . Cardano.serialiseToRawBytes

instance HasSNetworkId n => Arbitrary (ApiAddress n) where
    arbitrary = ApiAddress . fromCardanoAddressAny <$>
        genAddressAnyWithNetworkId (pure $ networkIdVal (sNetworkId @n))

instance HasSNetworkId n => Arbitrary (ApiAddressWithPath n) where
    shrink _ = []
    arbitrary = ApiAddressWithPath
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary EpochInfo where
    arbitrary = EpochInfo <$> arbitrary <*> genUniformTime
    shrink _ = []

instance Arbitrary (Script KeyHash) where
    arbitrary = do
        keyHashes <- vectorOf 10 arbitrary
        genScript keyHashes

instance Arbitrary KeyHash where
    arbitrary = do
        cred <- oneof [pure Payment, pure Delegation]
        KeyHash cred . BS.pack <$> vectorOf 28 arbitrary

instance Arbitrary (Script Cosigner) where
    arbitrary = genScriptCosigners

instance Arbitrary ScriptTemplate where
    arbitrary = genScriptTemplate

deriving newtype instance Arbitrary ApiCosignerIndex

instance Arbitrary ApiCredential where
    arbitrary = do
        pubKey <- BS.pack <$> replicateM 32 arbitrary
        xpubKey <- BS.pack <$> replicateM 64 arbitrary
        keyHash <- BS.pack <$> replicateM 28 arbitrary
        scriptHash <- ScriptHash . BS.pack <$> replicateM 28 arbitrary
        oneof [ pure $ CredentialPubKey pubKey
              , pure $ CredentialExtendedPubKey xpubKey
              , pure $ CredentialKeyHash keyHash
              , pure $ CredentialScriptHash scriptHash
              , CredentialScript <$> arbitrary ]

instance Arbitrary ApiCredentialType where
    arbitrary = ApiCredentialType <$> arbitraryBoundedEnum
    shrink = shrinkMapBy ApiCredentialType unApiCredentialType shrinkBoundedEnum

instance Arbitrary ValidationLevel where
    arbitrary =
        elements [RequiredValidation, RecommendedValidation]

instance Arbitrary ApiAddressData where
    arbitrary = do
        validation' <- oneof [pure Nothing, Just <$> arbitrary]
        credential1 <- arbitrary
        credential2 <- arbitrary
        addr <- elements
            [ AddrEnterprise credential1
            , AddrRewardAccount credential2
            , AddrBase credential1 credential2
            ]
        pure $ ApiAddressData addr validation'

instance Arbitrary AnyAddress where
    arbitrary = do
        payload' <- BS.pack <$> replicateM 32 arbitrary
        network' <- choose (0,1)
        addrType <- arbitraryBoundedEnum
        pure $ AnyAddress payload' addrType network'

instance HasSNetworkId n => Arbitrary (ApiSelectCoinsPayments n) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiDelegationAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiMultiDelegationAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Cosigner where
    arbitrary = Cosigner <$> choose (0,10)

instance Arbitrary ApiScriptTemplate where
    arbitrary = ApiScriptTemplate <$> arbitrary

instance Arbitrary ApiIncompleteSharedWallet where
    arbitrary = genericArbitrary -- fixme: seems to be slow

instance Arbitrary ApiActiveSharedWallet where
    arbitrary = genericArbitrary -- fixme: seems to be slow

instance Arbitrary ApiSharedWallet where
    arbitrary = oneof
        [ ApiSharedWallet . Right <$> arbitrary
        , ApiSharedWallet . Left <$> arbitrary ]

instance Arbitrary ApiScriptTemplateEntry where
    arbitrary = do
        script <-
            genScriptCosigners `suchThat` (not . null . retrieveAllCosigners)
        let scriptCosigners = retrieveAllCosigners script
        cosignersSubset <- sublistOf scriptCosigners `suchThat` (not . null)
        xpubsOrSelf <- vectorOf (length cosignersSubset) genXPubOrSelf
        pure $ ApiScriptTemplateEntry
            (Map.fromList $ zip cosignersSubset xpubsOrSelf) script
      where
        genXPubOrSelf :: Gen XPubOrSelf
        genXPubOrSelf = oneof [SomeAccountKey <$> genMockXPub, pure Self]

instance Arbitrary ApiSharedWalletPostDataFromMnemonics where
    arbitrary = genericArbitrary

instance Arbitrary ApiSharedWalletPostDataFromAccountPubX where
    arbitrary = genericArbitrary

instance Arbitrary ApiSharedWalletPostData where
    arbitrary = do
        let fromMnemonics = arbitrary :: Gen ApiSharedWalletPostDataFromMnemonics
        let fromAccXPub = arbitrary :: Gen ApiSharedWalletPostDataFromAccountPubX
        oneof [ ApiSharedWalletPostData . Right <$> fromAccXPub
              , ApiSharedWalletPostData . Left <$> fromMnemonics ]

instance Arbitrary ApiSharedWalletPatchData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiSelectCoinsAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiSelectCoinsData n) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiCertificate where
    arbitrary =
        oneof [ JoinPool <$> arbitraryRewardAccountPath <*> arbitrary
            , QuitPool <$> arbitraryRewardAccountPath
            , RegisterRewardAccount <$> arbitraryRewardAccountPath
            ]
      where
        arbitraryRewardAccountPath :: Gen (NonEmpty (ApiT DerivationIndex))
        arbitraryRewardAccountPath = NE.fromList <$> vectorOf 5 arbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiCoinSelection n) where
    arbitrary = ApiCoinSelection
        <$> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> arbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiCoinSelectionChange n) where
    arbitrary = ApiCoinSelectionChange
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink _ = []

instance HasSNetworkId n => Arbitrary (ApiCoinSelectionCollateral n) where
    arbitrary = ApiCoinSelectionCollateral
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink _ = []

instance HasSNetworkId n => Arbitrary (ApiCoinSelectionOutput n) where
    arbitrary = applyArbitrary3 ApiCoinSelectionOutput
    shrink _ = []

instance HasSNetworkId n => Arbitrary (ApiCoinSelectionWithdrawal n) where
    arbitrary = ApiCoinSelectionWithdrawal
        <$> arbitrary
        <*> reasonablySized arbitrary
        <*> arbitrary

instance Arbitrary AddressState where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Address where
    arbitrary = pure $ Address "<addr>"

instance Arbitrary (Quantity "lovelace" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "assets" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Quantity "byte" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary (Quantity "percent" Percentage) where
    shrink (Quantity p) = Quantity <$> shrinkPercentage p
    arbitrary = Quantity <$> genPercentage

instance Arbitrary ApiWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWallet where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletDiscovery where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiByronWalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletMigrationBalance where
    arbitrary = ApiWalletMigrationBalance
        <$> reasonablySized arbitrary
        <*> reasonablySized arbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiWalletMigrationPlan n) where
    arbitrary = ApiWalletMigrationPlan
        <$> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
        <*> reasonablySized arbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiWalletMigrationPlanPostData n) where
    arbitrary = do
        addrCount <- choose (1, 255)
        addrs <- (:|)
            <$> arbitrary
            <*> replicateM (addrCount - 1) arbitrary
        pure $ ApiWalletMigrationPlanPostData addrs

instance
    (HasSNetworkId n, Arbitrary (Passphrase purpose))
    => Arbitrary (ApiWalletMigrationPostData n purpose)
    where
    arbitrary = do
        pwd <- arbitrary
        addrCount <- choose (1, 255)
        addrs <-
            (:|)
                <$> arbitrary
                <*> replicateM (addrCount - 1) arbitrary
        pure $ ApiWalletMigrationPostData pwd addrs

instance Arbitrary ApiWalletPassphrase where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiFee where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiTxId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AddressPoolGap where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Iso8601Time where
    arbitrary = Iso8601Time <$> genUniformTime

instance Arbitrary PoolMetadataGCStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SortOrder where
    arbitrary = arbitraryBoundedEnum
    shrink = genericShrink

instance Arbitrary WalletOrAccountPostData where
    arbitrary = do
        let walletPostDataGen = arbitrary :: Gen WalletPostData
        let accountPostDataGen = arbitrary :: Gen AccountPostData
        oneof [ WalletOrAccountPostData . Left <$> walletPostDataGen
              , WalletOrAccountPostData . Right <$> accountPostDataGen ]

instance Arbitrary ApiAccountPublicKey where
    arbitrary = do
        seed <- SomeMnemonic <$> genMnemonic @15
        let rootXPrv = generateKeyFromSeed (seed, Nothing) mempty
        let accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv minBound
        pure $ ApiAccountPublicKey $ ApiT $ getKey accXPub

instance Arbitrary ApiAccountSharedPublicKey where
    arbitrary = do
        seed <- SomeMnemonic <$> genMnemonic @15
        let rootXPrv = generateKeyFromSeed (seed, Nothing) mempty
        let accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv minBound
        pure $ ApiAccountSharedPublicKey $ ApiT $ getKey accXPub

instance Arbitrary AccountPostData where
    arbitrary = do
        wName <- ApiT <$> arbitrary
        accXPub <- arbitrary
        pure $ AccountPostData wName accXPub Nothing

instance Arbitrary WalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ByronWalletFromXPrvPostData where
    arbitrary = do
        n <- arbitrary
        rootXPrv <- ApiT . unsafeXPrv . BS.pack <$> vector 128
        bytesNumber <- choose (64,100)
        h <- ApiT . PassphraseHash . BA.convert . B8.pack <$> replicateM bytesNumber arbitrary
        pure $ ByronWalletFromXPrvPostData n rootXPrv h

instance Arbitrary SomeByronWalletPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[12]) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[15]) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ByronWalletPostData '[12,15,18,21,24]) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PoolMetadataSource where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary SmashServer where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary URI where
    arbitrary = elements
        [fromJust (parseURI "https://my.little.friend")
        ,fromJust (parseURI "http://its-friday.com:8000")]

instance Arbitrary Settings where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SettingsPutData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutPassphraseData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletPutPassphraseOldPassphraseData where
    arbitrary = genericArbitrary
    shrink = genericShrink
instance Arbitrary WalletPutPassphraseMnemonicData where
    arbitrary = genericArbitrary
    shrink = genericShrink
instance Arbitrary ByronWalletPutPassphraseData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletAssetsBalance where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary WalletDelegationStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiWalletDelegationStatus where
    arbitrary = genericArbitrary

instance Arbitrary ApiWalletDelegationNext where
    arbitrary = oneof
        [ ApiWalletDelegationNext Api.Delegating
            <$> fmap Just arbitrary
            <*> fmap Just arbitrary
        , ApiWalletDelegationNext Api.NotDelegating
            Nothing . Just <$> arbitrary
        ]

instance Arbitrary (Passphrase "lenient") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "lenient"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "lenient"

instance Arbitrary ApiWalletDelegation where
    arbitrary = ApiWalletDelegation
        <$> fmap (\x -> x { changesAt = Nothing }) arbitrary
        <*> oneof [ vector i | i <- [0..2 ] ]

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 28 bytes

instance Arbitrary StakePool where
    arbitrary = StakePool
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary StakePoolFlag where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary StakePoolMetrics where
    arbitrary = StakePoolMetrics
        <$> (Quantity . fromIntegral <$> choose (1::Integer, 1_000_000_000_000))
        <*> arbitrary
        <*> (choose (0.0, 5.0))
        <*> (Quantity . fromIntegral <$> choose (1::Integer, 22_600_000))

instance Arbitrary StakePoolMetadata where
    arbitrary = StakePoolMetadata
        <$> arbitrary
        <*> arbitraryText 50
        <*> arbitraryMaybeText 255
        <*> arbitraryText 100
      where
        arbitraryText maxLen = do
            len <- choose (1, maxLen)
            T.pack <$> vector len
        arbitraryMaybeText maxLen = frequency
            [ (9, Just <$> arbitraryText maxLen)
            , (1, pure Nothing) ]

instance Arbitrary StakePoolTicker where
    arbitrary = unsafeFromText . T.pack <$> do
        len <- choose (3, 5)
        replicateM len arbitrary

instance Arbitrary ApiWalletSignData where
    arbitrary = ApiWalletSignData <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary PoolOwner where
    arbitrary = PoolOwner . BS.pack <$> vector 32

instance Arbitrary WalletId where
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    arbitrary = do
        len <- Test.QuickCheck.scale (min walletNameMaxLength) $ sized $ \n ->
            chooseInt (walletNameMinLength, walletNameMinLength `max` n)
        WalletName . T.pack <$> vectorOf len arbitraryPrintableChar
    shrink (WalletName t)
        | T.length t <= walletNameMinLength = []
        | otherwise = [WalletName $ T.take walletNameMinLength t]

instance Arbitrary ApiWalletPassphraseInfo where
    arbitrary = ApiWalletPassphraseInfo <$> genUniformTime

instance Arbitrary ApiMaintenanceAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiMaintenanceActionPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SyncProgress where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT

instance Arbitrary1 ApiT where
    liftArbitrary = fmap ApiT
    liftShrink = shrinkMapBy ApiT getApiT

-- | The initial seed has to be vector or length multiple of 4 bytes and shorter
-- than 64 bytes. Note that this is good for testing or examples, but probably
-- not for generating truly random Mnemonic words.
instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ natVal @n Proxy
            entropy =
                mkEntropy  @n . BA.convert . B8.pack <$> vector (size `quot` 8)
        in
            either (error . show . UnexpectedEntropyError) Prelude.id <$> entropy

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    )
    => Arbitrary (ApiMnemonicT (mw ': '[]))
  where
    arbitrary = do
        ent <- arbitrary @(Entropy n)
        return
            . ApiMnemonicT
            . SomeMnemonic
            $ entropyToMnemonic ent

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , Arbitrary (ApiMnemonicT rest)
    )
    => Arbitrary (ApiMnemonicT (mw ': rest))
  where
    arbitrary = do
        ApiMnemonicT x <- arbitrary @(ApiMnemonicT '[mw])
        ApiMnemonicT y <- arbitrary @(ApiMnemonicT rest)
        -- NOTE
        -- If we were to "naively" combine previous generators without weights,
        -- we would be tilting probabilities towards the leftmost element, so
        -- that every element would be twice as likely to appear as its right-
        -- hand neighbour, with an exponential decrease. (After the 7th element,
        -- subsequent elements would have less than 1 percent chance of
        -- appearing.) By tweaking the weights a bit as we have done below, we
        -- make it possible for every element to have at least 10% chance of
        -- appearing, for lists up to 10 elements.
        frequency
            [ (1, pure $ ApiMnemonicT x)
            , (5, pure $ ApiMnemonicT y)
            ]

instance Arbitrary ApiBlockReference where
    arbitrary = ApiBlockReference
        <$> arbitrary <*> arbitrary <*> genUniformTime <*> arbitrary
    shrink (ApiBlockReference sln sli t bh) =
        [ ApiBlockReference sln' sli' t bh'
        | (sln', sli', bh') <- shrink (sln, sli, bh) ]

instance Arbitrary ApiBlockInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiSlotReference where
    arbitrary = ApiSlotReference <$> arbitrary <*> arbitrary <*> genUniformTime
    shrink (ApiSlotReference sln sli t) =
        [ ApiSlotReference sln' sli' t
        | (sln', sli') <- shrink (sln, sli) ]

instance Arbitrary ApiSlotId where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiNetworkInfo where
    arbitrary = oneof
        [ pure $ ApiNetworkInfo "mainnet" 764_824_073
        , ApiNetworkInfo "testnet" <$> arbitrary
        ]

instance Arbitrary ApiWalletMode where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiNetworkInformation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NtpStatusWithOffset where
    arbitrary = do
        o <- Quantity <$> arbitrary @Integer
        elements
            [ NtpStatusWithOffset NtpSyncingStatusUnavailable Nothing
            , NtpStatusWithOffset NtpSyncingStatusPending Nothing
            , NtpStatusWithOffset NtpSyncingStatusAvailable (Just o)
            ]

instance Arbitrary ApiNetworkClock where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)

instance Arbitrary (Quantity "slot" Word32) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word32)

instance Arbitrary SlotNo where
    shrink = fmap SlotNo . shrink . unSlotNo
    arbitrary = SlotNo <$> arbitrary

instance Arbitrary (Hash "Genesis") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary StartTime where
    arbitrary = StartTime <$> genUniformTime

instance Arbitrary (Quantity "second" NominalDiffTime) where
    shrink (Quantity 0.0) = []
    shrink _ = [Quantity 0.0]
    arbitrary = Quantity . fromInteger <$> choose (0, 10_000)

instance Arbitrary (Quantity "percent" Double) where
    shrink (Quantity 0.0) = []
    shrink _ = [Quantity 0.0]
    arbitrary = Quantity <$> choose (0,100)

deriving instance Arbitrary ApiPostPolicyKeyData

instance Arbitrary ApiPolicyKey where
    arbitrary = do
        hashing <- elements [WithHashing, WithoutHashing]
        ApiPolicyKey <$> genKey (len hashing) <*> pure hashing
      where
        genKey n = fmap B8.pack (vectorOf n arbitrary)
        len WithHashing = 28
        len WithoutHashing = 32

instance ToSchema ApiPolicyKey where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPolicyKey"

instance Arbitrary ApiPolicyId where
    arbitrary = ApiPolicyId <$> arbitrary

instance ToSchema ApiPolicyId where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPolicyId"

instance Arbitrary ApiVerificationKeyShelley where
    arbitrary = do
        hashing <- elements [WithHashing, WithoutHashing]
        ApiVerificationKeyShelley <$> genKeyRole (len hashing) <*> pure hashing
      where
        genKeyRole n = (,)
            <$> fmap B8.pack (vectorOf n arbitrary)
            <*> elements [UtxoExternal, UtxoInternal, MutableAccount]
        len WithHashing = 28
        len WithoutHashing = 32

instance ToSchema ApiVerificationKeyShelley where
    declareNamedSchema _ = declareSchemaForDefinition "ApiVerificationKeyShelley"

instance Arbitrary ApiVerificationKeyShared where
    arbitrary =
        oneof [noHashedGen, hashedGen]
      where
          noHashedGen = do
              payload' <- fmap B8.pack (replicateM 32 arbitrary)
              role' <- elements [UtxoExternal, MutableAccount]
              pure $ ApiVerificationKeyShared (payload', role') WithoutHashing

          hashedGen = do
              payload' <- fmap B8.pack (replicateM 28 arbitrary)
              role' <- elements [UtxoExternal, MutableAccount]
              pure $ ApiVerificationKeyShared (payload', role') WithHashing

instance ToSchema ApiVerificationKeyShared where
    declareNamedSchema _ = declareSchemaForDefinition "ApiVerificationKeyShared"

instance Arbitrary Api.MaintenanceAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance ToSchema Api.ApiMaintenanceAction where
    declareNamedSchema _ = declareSchemaForDefinition "ApiMaintenanceAction"

instance ToSchema Api.ApiMaintenanceActionPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiMaintenanceActionPostData"

instance Arbitrary ApiNetworkParameters where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ExecutionUnitPrices where
    shrink = genericShrink
    arbitrary = do
        step <- getNonNegative <$> arbitrary
        mem <- getNonNegative <$> arbitrary
        pure $ ExecutionUnitPrices step mem

instance Arbitrary ApiEra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiEraInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = applyArbitrary2 SlotId
    shrink = genericShrink

instance Arbitrary SlotInEpoch where
    shrink (SlotInEpoch x) = SlotInEpoch <$> shrink x
    arbitrary = SlotInEpoch <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary AssetDecimals where
    arbitrary = AssetDecimals <$> choose (1, 19)

instance Arbitrary AssetMetadata where
    -- TODO: We should add a proper arbitrary instance
    arbitrary = AssetMetadata "asset" "An asset"
        <$> (oneof [pure Nothing, pure $ Just "AST"])
        <*> genMaybe (pure $ AssetURL $ fromJust $ parseURI "https://asset.url")
        <*> genLogo
        <*> arbitrary
      where
        genMaybe g = frequency
            [ (80, pure Nothing)
            , (20, Just <$> g)
            ]
        genLogo = genMaybe $ pure $ AssetLogo $ B8.pack "<logo>"

    shrink _ = []

instance Arbitrary TokenMetadataError where
    arbitrary = oneof $ map pure
        [ TokenMetadataClientError (error "actual exception not needed")
        , TokenMetadataFetchError (error "actual exception not needed")
        , TokenMetadataJSONParseError "" ""
        ]

instance Arbitrary ApiAsset where
    arbitrary = toApiAsset <$> arbitrary <*> genAssetId

instance Arbitrary a => Arbitrary (AddressAmount a) where
    arbitrary = applyArbitrary3 AddressAmount
    shrink _ = []

instance Arbitrary a => Arbitrary (AddressAmountNoAssets a) where
    arbitrary = applyArbitrary2 AddressAmountNoAssets
    shrink _ = []

instance Arbitrary ApiSignTransactionPostData where
    arbitrary = ApiSignTransactionPostData
        <$> arbitrary
        <*> arbitrary
        <*> elements [Just HexEncoded, Just Base64Encoded, Nothing]

instance HasSNetworkId n => Arbitrary (PostTransactionOldData n) where
    arbitrary = PostTransactionOldData
        <$> arbitrary
        <*> arbitrary
        <*> elements [Just SelfWithdrawal, Nothing]
        <*> arbitrary
        <*> arbitrary

instance Arbitrary TxMetadataWithSchema where
  arbitrary = TxMetadataWithSchema
    <$> elements [TxMetadataNoSchema, TxMetadataDetailedSchema]
    <*> arbitrary

instance HasSNetworkId n => Arbitrary (ApiConstructTransactionData n) where
    arbitrary = ApiConstructTransactionData
        <$> arbitrary
        <*> elements [Just SelfWithdraw, Nothing]
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure Nothing
        <*> elements [Just HexEncoded, Just Base64Encoded, Nothing]

instance HasSNetworkId n => Arbitrary (ApiExternalInput n) where
    arbitrary = ApiExternalInput
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> liftArbitrary (liftArbitrary genDatumHash)

instance HasSNetworkId n => Arbitrary (ApiBalanceTransactionPostData n) where
    arbitrary = ApiBalanceTransactionPostData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> elements [Just HexEncoded, Just Base64Encoded, Nothing]

instance HasSNetworkId n => Arbitrary (ApiRedeemer n) where
    arbitrary = oneof
        [ ApiRedeemerSpending <$> arbitrary <*> arbitrary
        , ApiRedeemerMinting <$> arbitrary <*> arbitrary
        , ApiRedeemerRewarding <$> arbitrary <*> arbitrary
        ]

instance HasSNetworkId n => Arbitrary (ApiTxInputGeneral n) where
    arbitrary = oneof
        [ ExternalInput <$> arbitrary
        , WalletInput <$> arbitrary
        ]

instance HasSNetworkId n => Arbitrary (ApiWithdrawalGeneral n) where
    arbitrary = ApiWithdrawalGeneral
        <$> arbitrary
        <*> arbitrary
        <*> oneof [pure External, pure Our]

instance HasSNetworkId n => Arbitrary (ApiWalletInput n) where
    arbitrary = ApiWalletInput
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance HasSNetworkId n => Arbitrary (ApiWalletOutput n) where
    arbitrary = ApiWalletOutput
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance HasSNetworkId n => Arbitrary (ApiTxOutputGeneral n) where
    arbitrary = oneof
        [ ExternalOutput <$> arbitrary
        , WalletOutput <$> arbitrary
        ]

instance Arbitrary NonWalletCertificate where
    arbitrary = oneof
        [ pure GenesisCertificate
        , pure MIRCertificate
        ]

instance Arbitrary ApiDeregisterPool where
    arbitrary = ApiDeregisterPool
        <$> arbitrary
        <*> arbitrary

instance Arbitrary ApiRegisterPool where
    arbitrary = ApiRegisterPool
        <$> arbitrary
        <*> arbitrary
        <*> fmap Quantity genPercentage
        <*> arbitrary
        <*> arbitrary
        <*> pure Nothing

instance HasSNetworkId n => Arbitrary (ApiExternalCertificate n) where
    arbitrary = oneof
        [ RegisterRewardAccountExternal <$> arbitrary
        , JoinPoolExternal <$> arbitrary <*> arbitrary
        , QuitPoolExternal <$> arbitrary
        ]

instance HasSNetworkId n => Arbitrary (ApiAnyCertificate n) where
    arbitrary = oneof
        [ WalletDelegationCertificate <$> arbitrary
        , DelegationCertificate <$> arbitrary
        , StakePoolRegister <$> arbitrary
        , StakePoolDeregister <$> arbitrary
        , OtherCertificate <$> arbitrary
        ]

instance Arbitrary ValidityIntervalExplicit where
    arbitrary = do
        slot1 <- arbitrary
        slot2 <- arbitrary `suchThat` (> slot1)
        pure $ ValidityIntervalExplicit (Quantity slot1) (Quantity slot2)

instance Arbitrary ApiWitnessCount where
    arbitrary = do
        numberOfScripts <- choose (0, 1)
        txId <- arbitrary
        referenceInp <-
            elements [ViaSpending, ViaReferenceInput (ReferenceInput txId)]
        fmap mkApiWitnessCount $ WitnessCount
            <$> choose (0, 10)
            <*> vectorOf numberOfScripts (flip NativeExplicitScript referenceInp  <$> arbitrary)
            <*> choose (0, 2)

instance HasSNetworkId n => Arbitrary (ApiDecodedTransaction n) where
    arbitrary = ApiDecodedTransaction
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary StakeAddress where
    arbitrary = do
        header  <- elements [ BS.singleton 241, BS.singleton 224 ]
        payload <- BS.pack <$> vector 28
        pure $ fromRight' $ deserialiseFromRawBytes
            (proxyToAsType Proxy)
            (header <> payload)

instance Arbitrary ApiSealedTxEncoding where
    arbitrary = elements [HexEncoded, Base64Encoded]

instance HasSNetworkId n => Arbitrary (ApiConstructTransaction n) where
    arbitrary = applyArbitrary3 ApiConstructTransaction

instance Arbitrary ApiTokenAmountFingerprint where
    arbitrary = do
        name <- genTokenName
        policyid <- arbitrary
        let fingerprint = ApiT $ mkTokenFingerprint policyid name
        ApiTokenAmountFingerprint (ApiT name)
            <$> (fromIntegral <$> choose @Int (1, 10_000))
            <*> pure fingerprint

instance Arbitrary ApiTokens where
    arbitrary = do
        policyid <- arbitrary
        scriptHash <- ScriptHash . BS.pack <$> vector 28
        let keyhash = KeyHash Policy $ getHash $ unTokenPolicyId policyid
        script <- elements
            [ ApiT $ NativeScript (RequireSignatureOf keyhash) ViaSpending
            , ApiT $ NativeScript (RequireAllOf
                [ RequireSignatureOf keyhash
                , ActiveFromSlot 100
                ]) ViaSpending
            , ApiT $ NativeScript (RequireAllOf
                [ RequireSignatureOf keyhash
                , ActiveFromSlot 100
                , ActiveUntilSlot 150
                ]) ViaSpending
            , ApiT $ PlutusScript (PlutusScriptInfo PlutusVersionV1 scriptHash)
                ViaSpending
            , ApiT $ PlutusScript (PlutusScriptInfo PlutusVersionV2 scriptHash)
                ViaSpending
            ]
        assetNum <- choose (1,4)
        assets <- vectorOf assetNum arbitrary
        pure $ ApiTokens (ApiT policyid) script (NE.fromList assets)

instance Arbitrary ApiAssetMintBurn where
    arbitrary = do
        let keyix = ApiT $ DerivationIndex $
                getIndex (minBound :: Index 'Hardened 'PolicyK)
        ApiAssetMintBurn
            <$> arbitrary
            <*> arbitrary
            <*> pure (Just keyix)

instance Arbitrary ApiPostPolicyIdData where
    arbitrary = ApiPostPolicyIdData
        <$> elements
            [ ApiT $ RequireSignatureOf (Cosigner 0)
            , ApiT $ RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , ActiveFromSlot 100
                ]
            , ApiT $ RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , ActiveFromSlot 100
                , ActiveUntilSlot 150
                ]
            ]

instance ToSchema ApiPostPolicyIdData where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiPostPolicyIdData"

instance HasSNetworkId n => Arbitrary (ApiMintBurnData n) where
    arbitrary = ApiMintBurnData
        <$> elements
            [ ApiT $ RequireSignatureOf (Cosigner 0)
            , ApiT $ RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , ActiveFromSlot 100
                ]
            , ApiT $ RequireAllOf
                [ RequireSignatureOf (Cosigner 0)
                , ActiveFromSlot 100
                , ActiveUntilSlot 150
                ]
            ]
        <*> oneof
            [ Just . ApiT <$> genTokenName
            , pure Nothing
            ]
        <*> arbitrary

instance Arbitrary ApiStakeKeyIndex where
    arbitrary = ApiStakeKeyIndex <$> arbitrary

instance HasSNetworkId n => Arbitrary (ApiMintData n) where
    arbitrary = ApiMintData <$> arbitrary <*> arbitrary

instance HasSNetworkId n => Arbitrary (ApiPaymentDestination n) where
    arbitrary = ApiPaymentAddresses <$> arbitrary

instance Arbitrary ApiBurnData where
    arbitrary = ApiBurnData <$> arbitrary

instance HasSNetworkId n => Arbitrary (ApiMintBurnOperation n) where
    arbitrary = oneof
        [ ApiMint <$> arbitrary
        , ApiBurn <$> arbitrary
        ]

{--
instance Arbitrary ApiMintBurnInfo where
    arbitrary = do
        mpi <- arbitrary
        policyId <- arbitrary
        assetName <- arbitrary
        let
            subject = mkTokenFingerprint policyId assetName
            script  =
                RequireSignatureOf
                    (KeyHash Payment $ getHash $ unTokenPolicyId policyId)

        pure $ ApiMintBurnInfo
            (ApiT mpi)
            (ApiT policyId)
            (ApiT assetName)
            (ApiT subject)
            (ApiT script)
--}
instance Arbitrary TokenPolicyId where
    arbitrary = UnsafeTokenPolicyId . Hash . BS.pack <$> vector 28

instance Arbitrary TokenName where
    arbitrary = UnsafeTokenName . BS.pack <$> vector 32

instance Arbitrary ApiWithdrawalPostData where
    arbitrary = genericArbitrary
    shrink = genericShrink

deriving instance Arbitrary ApiValidityIntervalExplicit

instance HasSNetworkId n => Arbitrary (ApiPutAddressesData n) where
    arbitrary = do
        n <- choose (1,255)
        addrs <- vector n
        pure $ ApiPutAddressesData addrs

instance HasSNetworkId n => Arbitrary (PostTransactionFeeOldData n) where
    arbitrary = PostTransactionFeeOldData
        <$> arbitrary
        <*> elements [Just SelfWithdrawal, Nothing]
        <*> arbitrary
        <*> arbitrary

selectFromPreparedBinaries :: Gen ByteString
selectFromPreparedBinaries = elements $ toByteString <$>
    [ "83a400818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1770e07fa22b\
      \a864808018282583901bdd74c3bd086d38939876fcbd56e91dd56fccca9be70b424390443\
      \67af33d417814e6fa7953195797d73f9b5fb511854b4b0d8b2023959951a002dc6c082583\
      \9011a2f2f103b895dbe7388acc9cc10f90dc4ada53f46c841d2ac44630789fc61d21ddfcb\
      \d4d43652bf05c40c346fa794871423b65052d7614c1b0000001748472188021a0001ffb80\
      \3198d11a1008182582043ea6d45e9abe6e30faff4a9b675abdc49534a6eda9ba96f9368d1\
      \2d879dfc6758409b898ca143e1b245c9c745c690b8137b724fc63f8a3b852bcd2234cee4e\
      \68c25cd333e845a224b9cb4600f271d545e35a41d17a16c046aea66ed34a536559f0df6"
    , "83a400818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1770e07fa22b\
      \a86481301828258390118b8c2b229e68b21c54c68d91944fead4c043e8348368b1ac551c9\
      \00c93e3edd82798a526ccf85b2a42e04037349ffe185e26a16356dfce61a002dc6c082583\
      \90110a9b4666ba80e4878491d1ac20465c9893a8df5581dc705770626203d4d23fe6a7acd\
      \da5a1b41f56100f02bfa270a3c560c4e55cf8312331b0000001748472188021a0001ffb80\
      \3198d4fa10081825820fc2f860286fc72c1c1e29f1c0a23e9e11771f60e1d26799f71846c\
      \89f5aa91315840e4dec970d40b749d9bc77996c2f102bd056b5f9ba3fd13745f410d8fc96\
      \e0aaca4a4b4e1d52d6ce1d92b0d79412e542f2bebfa29f991c09c131b1dfeb2832300f6"
    , "83a40081825820c03484e3bf981bec7cc5cf4da1a3d9fe4eab83b3f5f7574752a2b8a9b24\
      \09b2f0001828258390154fe81cb1633e9f2f4446b78f67f00ac19abc4351be6548719118b\
      \f8c93e3edd82798a526ccf85b2a42e04037349ffe185e26a16356dfce61a000f424082583\
      \90184f41fa42d8ab05639aeb780ff4b3fe290f11d3439b04303fdfa9488af33d417814e6f\
      \a7953195797d73f9b5fb511854b4b0d8b2023959951a001c84c8021a0001ffb803198d88a\
      \10081825820931c2a7343df63fb785aec4b2de688b8290cf634f638f647f22c2e68256919\
      \6a5840c6e88b52c406131a7b004b76fe24f611b1f5d19964ee2ce3144fff391a725be7a5a\
      \b3f27389eb75dbe354026d5822f7b71f9deb84cff0126e809bd5690409f00f6"
    , "83a400818258200eaa33be8780935ca5a7c1e628a2d54402446f96236ca8f1770e07fa22b\
      \a8648000184825839014067fb21919c12519843c07d09b4c548e34f7e7c473b352f2751bd\
      \a42387e650558a9026a0b9623adc92aa411f8fa598ee901db296a51bf51a000f424082583\
      \90132a432e6d711312ba6c390725ee81cd525c9b5ec5e8bf99772062d6d2387e650558a90\
      \26a0b9623adc92aa411f8fa598ee901db296a51bf51a000f4240825839011a2f2f103b895\
      \dbe7388acc9cc10f90dc4ada53f46c841d2ac44630789fc61d21ddfcbd4d43652bf05c40c\
      \346fa794871423b65052d7614c1b0000000ba42b175482583901c59701fee28ad31559870\
      \ecd6ea92b143b1ce1b68ccb62f8e8437b3089fc61d21ddfcbd4d43652bf05c40c346fa794\
      \871423b65052d7614c1b0000000ba42b1754021a000234d803198d16a10081825820c15b9\
      \90344122b12494a5edd1020d9eb32e34b0f82691f8e31645ddab712ff2b58408e6a29053f\
      \9f7f04f3de256cc4b30f24b2d5ffe4927c86e9d6310b224afb94f4e5b8eea6573e6fa1404\
      \07153c12fdf8cf619edff0c7c27aa91ae3acb56041a00f6"
    ]
  where
    toByteString txt =
        let (Right bs) = fromHex $ T.encodeUtf8 txt
        in bs

deriving instance Arbitrary a => Arbitrary (ApiAsArray s a)

instance Arbitrary (ApiBytesT base ByteString) where
    arbitrary = ApiBytesT <$> selectFromPreparedBinaries

instance Arbitrary (ApiBytesT base SerialisedTx) where
    arbitrary = ApiBytesT <$> arbitrary

instance Arbitrary ApiSerialisedTransaction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SealedTx where
    arbitrary = unsafeSealedTxFromBytes <$> selectFromPreparedBinaries

instance Arbitrary SerialisedTx where
    arbitrary = SerialisedTx <$> selectFromPreparedBinaries

instance Arbitrary TxMetadata where
    arbitrary = genNestedTxMetadata
    shrink = shrinkTxMetadata

instance Arbitrary ApiError where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorInfo where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorMessage where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorSharedWalletNoSuchCosigner where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorTxOutputLovelaceInsufficient where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorBalanceTxUnderestimatedFee where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiErrorNodeNotYetInRecentEra where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiTxMetadata where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance HasSNetworkId n => Arbitrary (ApiTransaction n) where
    shrink = genericShrink
    arbitrary = do
        txStatus <- arbitrary
        txInsertedAt <- case txStatus of
            ApiT Pending -> pure Nothing
            ApiT InLedger -> arbitrary
            ApiT Expired -> pure Nothing
        txPendingSince <- case txStatus of
            ApiT Pending -> arbitrary
            ApiT InLedger -> pure Nothing
            ApiT Expired -> arbitrary
        txExpiresAt <- case txStatus of
            ApiT Pending -> arbitrary
            ApiT InLedger -> pure Nothing
            ApiT Expired -> Just <$> arbitrary

        ApiTransaction
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> pure txInsertedAt
            <*> pure txPendingSince
            <*> pure txExpiresAt
            <*> arbitrary
            <*> arbitrary
            <*> genInputs
            <*> genOutputs
            <*> genCollateral
            <*> genCollateralOutputs
            <*> genWithdrawals
            <*> pure txStatus
            <*> arbitrary
            <*> liftArbitrary (ApiT <$> genTxScriptValidity)
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
      where
        genInputs =
            Test.QuickCheck.scale (`mod` 3) arbitrary
        genOutputs =
            Test.QuickCheck.scale (`mod` 3) arbitrary
        genWithdrawals =
            Test.QuickCheck.scale (`mod` 3) arbitrary
        genCollateral =
            Test.QuickCheck.scale (`mod` 3) arbitrary
        genCollateralOutputs =
            Test.QuickCheck.scale (`mod` 3) arbitrary

instance Arbitrary TxScriptValidity where
    arbitrary = genTxScriptValidity
    shrink = shrinkTxScriptValidity

instance Arbitrary (ApiWithdrawal (t :: NetworkDiscriminant)) where
    arbitrary = ApiWithdrawal
        <$> arbitrary
        <*> arbitrary

instance Arbitrary RewardAccount where
    arbitrary = do
        bs <- BS.pack <$> vector 28
        oneof [pure $ FromKeyHash bs, pure $ FromScriptHash bs]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = genTxOutCoin

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList utxo

instance Arbitrary TokenBundle where
    shrink = shrinkTokenBundleSmallRange
    arbitrary = genTokenBundleSmallRange

instance Arbitrary TokenMap where
    shrink = shrinkTokenMap
    arbitrary = genTokenMapSmallRange

instance Arbitrary TxOut where
    -- Shrink token bundle but not address
    shrink (TxOut a t) = TxOut a <$> shrink t
    arbitrary = TxOut <$> arbitrary <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        -- NOTE: No need for a crazy high indexes
        <*> Test.QuickCheck.scale (`mod` 3) arbitrary

instance Arbitrary ApiWalletUtxoSnapshot where
    arbitrary = do
        entryCount <- choose (0, 4)
        entries <- replicateM entryCount genEntry
        pure $ ApiWalletUtxoSnapshot { entries }
      where
        genEntry :: Gen ApiWalletUtxoSnapshotEntry
        genEntry = do
            adaValue1 <- genCoinPositive
            adaValue2 <- genCoinPositive
            -- The actual ada quantity of an output's token bundle must be
            -- greater than or equal to the minimum permissible ada quantity:
            let ada = Coin.toQuantity $ max adaValue1 adaValue2
            let adaMinimum = Coin.toQuantity $ min adaValue1 adaValue2
            assets <- ApiT <$> genTokenMapSmallRange
            pure ApiWalletUtxoSnapshotEntry
                { ada
                , adaMinimum
                , assets
                }

instance Arbitrary ApiUtxoStatistics where
    arbitrary = do
        utxos <- arbitrary
        let (UTxOStatistics histoBars stakes bType) =
                UTxOStatistics.compute utxos
        let boundCountMap =
                Map.fromList $ map (\(HistogramBar k v)-> (k,v)) histoBars
        return $ ApiUtxoStatistics
            (Quantity $ fromIntegral stakes)
            (ApiT bType)
            boundCountMap

instance HasSNetworkId n => Arbitrary (ApiTxInput n) where
    shrink _ = []
    arbitrary = applyArbitrary2 ApiTxInput

instance HasSNetworkId n => Arbitrary (ApiTxCollateral n) where
    shrink _ = []
    arbitrary = applyArbitrary2 ApiTxCollateral

instance Arbitrary (Quantity "slot" Natural) where
    shrink (Quantity 0) = []
    shrink _ = [Quantity 0]
    arbitrary = Quantity . fromIntegral <$> (arbitrary @Word8)

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary (Hash "Datum") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary (Hash "ScriptIntegrity") where
    arbitrary = Hash . BS.pack <$> vector 32

instance Arbitrary (Hash "ExtraSignature") where
    arbitrary = Hash . BS.pack <$> vector 28

instance Arbitrary Direction where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxStatus where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Quantity "block" Natural) where
    arbitrary = Quantity . fromIntegral <$> arbitrary @Word32

instance Arbitrary (Quantity "slot" Word64) where
    arbitrary = Quantity <$> arbitrary @Word64

instance Arbitrary ApiPostRandomAddressData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiAddressInspect where
    arbitrary = do
        style <- elements [ "Byron", "Icarus", "Shelley" ]
        stake <- elements [ "none", "by value", "by pointer" ]
        pure $ ApiAddressInspect $ Aeson.object
            [ "address_style" .= Aeson.String style
            , "stake_reference" .= Aeson.String stake
            ]

instance Arbitrary HealthCheckSMASH where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiHealthCheck where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiPostAccountKeyData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiPostAccountKeyDataWithPurpose where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TokenFingerprint where
    arbitrary = do
        AssetId policy aName <- genAssetId
        pure $ mkTokenFingerprint policy aName
    shrink _ = []

instance Arbitrary KeyFormat where
    arbitrary =
        oneof [pure NonExtended, pure Extended]

instance Arbitrary ApiAccountKey where
    arbitrary = do
        xpubKey <- BS.pack <$> replicateM 64 arbitrary
        pubKey <- BS.pack <$> replicateM 32 arbitrary
        oneof [ pure $ ApiAccountKey pubKey NonExtended purposeCIP1852
              , pure $ ApiAccountKey xpubKey Extended purposeCIP1852
              , pure $ ApiAccountKey pubKey NonExtended purposeCIP1854
              , pure $ ApiAccountKey xpubKey Extended purposeCIP1854]

instance Arbitrary ApiAccountKeyShared where
    arbitrary = do
        xpubKey <- BS.pack <$> replicateM 64 arbitrary
        pubKey <- BS.pack <$> replicateM 32 arbitrary
        oneof [ pure $ ApiAccountKeyShared pubKey NonExtended purposeCIP1854
              , pure $ ApiAccountKeyShared xpubKey Extended purposeCIP1854 ]

instance Arbitrary (ApiStakeKeys n) where
    arbitrary = Test.QuickCheck.scale (`div` 4) genericArbitrary
    shrink = genericShrink

instance Arbitrary (ApiOurStakeKey n) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (ApiForeignStakeKey n) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary ApiNullStakeKey where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (Hash "BlockHeader") where
    arbitrary = Hash . B8.pack <$> replicateM 32 arbitrary

instance Arbitrary ApiBlockHeader where
    arbitrary = genericArbitrary
    shrink = genericShrink
{-------------------------------------------------------------------------------
                   Specification / Servant-Swagger Machinery

  Below is a bit of complicated API-Level stuff in order to achieve two things:

  1/ Verify that every response from the API that actually has a JSON content
     type returns a JSON instance that matches the JSON format described by the
     specification (field names should be the same, and constraints on values as
     well).
     For this, we need three things:
         - ToJSON instances on all those types, it's a given with the above
         - Arbitrary instances on all those types, that reflect as much as
           possible, all possible values of those types. Also given by using
           'genericArbitrary' whenever possible.
         - ToSchema instances which tells how do a given type should be
           represented.
     The trick is for the later point. In a "classic" scenario, we would have
     defined the `ToSchema` instances directly in Haskell on our types, which
     eventually becomes a real pain to maintain. Instead, we have written the
     spec by hand, and we want to check that our implementation matches it.
     So, we "emulate" the 'ToSchema' instance by:
         - Parsing the specification file (which is embedded at compile-time)
         - Creating missing 'ToSchema' by doing lookups in that global schema

  2/ The above verification is rather weak, because it just controls the return
     types of endpoints, but not that those endpoints are somewhat valid. Thus,
     we've also built another check 'extractEndpoints' which crawls our servant
     API type, and checks whether every path we have in our API appears in the
     specification. It does it by defining a few recursive type-classes to
     crawl the API, and for each endpoint:
         - construct the corresponding path (with verb)
         - build an HSpec scenario which checks whether the path is present
    This seemingly means that the identifiers we use in our servant paths (in
    particular, those for path parameters) should exactly match the specs.

-------------------------------------------------------------------------------}

-- | Specification file, embedded at compile-time and decoded right away
specification :: Aeson.Value
specification =
    unsafeDecode bytes
  where
    bytes = $(
        let swaggerYaml = "./specifications/api/swagger.yaml"
        in liftIO (lookupEnv "SWAGGER_YAML") >>=
        maybe (makeRelativeToProject swaggerYaml) pure >>=
        embedFile
        )
    unsafeDecode =
        either (error . (msg <>) . show) Prelude.id . Yaml.decodeEither'
    msg = "Whoops! Failed to parse or find the api specification document: "

instance Typeable n => ToSchema (ApiAddressWithPath n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAddressWithPath"

instance ToSchema ApiAddressInspect where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAddressInspect"

instance Typeable n => ToSchema (ApiPutAddressesData n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPutAddressesData"

instance Typeable n => ToSchema (ApiSelectCoinsData n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        declareSchemaForDefinition "ApiSelectCoinsData"

instance ToSchema (ApiT SmashServer) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSmashServer"

instance ToSchema ApiHealthCheck where
    declareNamedSchema _ = declareSchemaForDefinition "ApiHealthCheck"

instance Typeable n => ToSchema (ApiCoinSelection n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiCoinSelection"

instance ToSchema ApiWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWallet"

instance ToSchema ApiByronWallet where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWallet"

instance ToSchema ApiWalletMigrationBalance where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletMigrationBalance"

instance Typeable n => ToSchema (ApiWalletMigrationPlan n) where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletMigrationPlan"

instance Typeable n => ToSchema (ApiWalletMigrationPlanPostData n) where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletMigrationPlanPostData"

instance Typeable n => ToSchema (ApiWalletMigrationPostData n "lenient") where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiByronWalletMigrationPostData"

instance Typeable n => ToSchema (ApiWalletMigrationPostData n "user") where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiShelleyWalletMigrationPostData"

instance ToSchema ApiWalletPassphrase where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletPassphrase"

instance ToSchema ApiWalletUtxoSnapshot where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletUtxoSnapshot"

instance ToSchema (ApiT StakePool) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiStakePool"

instance ToSchema (ApiT StakePoolMetrics) where
    declareNamedSchema _ = declareSchemaForDefinition "StakePoolMetrics"

instance ToSchema ApiFee where
    declareNamedSchema _ = declareSchemaForDefinition "ApiFee"

instance ToSchema ApiAsset where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAsset"

instance ToSchema ApiTxId where
    declareNamedSchema _ = declareSchemaForDefinition "ApiTxId"

instance ToSchema WalletPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletPostData"

instance ToSchema AccountPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAccountPostData"

instance ToSchema WalletOrAccountPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletOrAccountPostData"

instance ToSchema (ByronWalletPostData '[12]) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletRandomPostData"

instance ToSchema (ByronWalletPostData '[15]) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletIcarusPostData"

instance ToSchema (ByronWalletPostData '[12,15,18,21,24]) where
    -- NOTE ApiByronWalletLedgerPostData works too. Only the description differs.
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletTrezorPostData"

instance ToSchema ByronWalletFromXPrvPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiByronWalletRandomXPrvPostData"

instance ToSchema SomeByronWalletPostData where
    declareNamedSchema _ = declareSchemaForDefinition "SomeByronWalletPostData"

instance ToSchema WalletPutData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletPutData"

instance ToSchema SettingsPutData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSettingsPutData"

instance ToSchema (ApiT Settings) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiGetSettings"

instance ToSchema WalletPutPassphraseData where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiWalletPutPassphraseData"

instance ToSchema ByronWalletPutPassphraseData where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiByronWalletPutPassphraseData"

instance ToSchema ApiTxMetadata where
    declareNamedSchema _ = declareSchemaForDefinition "TransactionMetadataValue"

instance ToSchema ApiSignTransactionPostData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSignTransactionPostData"

instance ToSchema ApiSerialisedTransaction where
    -- fixme: tests don't seem to like allOf
    declareNamedSchema _ = declareSchemaForDefinition "ApiSerialisedTransaction"

instance ToSchema (ApiBytesT 'Base64 SerialisedTx) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSerialisedTx"

instance Typeable n => ToSchema (PostTransactionOldData n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        declareSchemaForDefinition "ApiPostTransactionData"

instance Typeable n => ToSchema (PostTransactionFeeOldData n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        declareSchemaForDefinition "ApiPostTransactionFeeData"

instance Typeable n => ToSchema (ApiExternalInput n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiExternalInput"

instance Typeable n => ToSchema (ApiBalanceTransactionPostData n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiBalanceTransactionPostData"

instance Typeable n => ToSchema (ApiTransaction n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        declareSchemaForDefinition "ApiTransaction"

instance ToSchema ApiUtxoStatistics where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletUTxOsStatistics"

instance ToSchema ApiNetworkInformation where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkInformation"

instance ToSchema ApiNetworkClock where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkClock"

data ApiScript
instance ToSchema ApiScript where
    declareNamedSchema _ = declareSchemaForDefinition "ApiScript"

data ApiPubKey
instance ToSchema ApiPubKey where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPubKey"

instance ToSchema ApiSharedWalletPostDataFromMnemonics where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiSharedWalletPostDataFromMnemonics"

instance ToSchema ApiSharedWalletPostDataFromAccountPubX where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiSharedWalletPostDataFromAccountPubX"

instance ToSchema ApiSharedWalletPostData where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiSharedWalletPostData"

instance ToSchema ApiSharedWalletPatchData where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiSharedWalletPatchData"

instance ToSchema ApiActiveSharedWallet where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiActiveSharedWallet"

instance ToSchema ApiIncompleteSharedWallet where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiIncompleteSharedWallet"

instance ToSchema ApiSharedWallet where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiSharedWallet"

instance ToSchema ApiAddressData where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        addDefinition =<< declareSchemaForDefinition "CredentialValue"
        declareSchemaForDefinition "ApiAddressData"

instance ToSchema ApiCredential where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        declareSchemaForDefinition "ApiCredential"

instance ToSchema AnyAddress where
    declareNamedSchema _ = declareSchemaForDefinition "AnyAddress"

instance ToSchema ApiNetworkParameters where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNetworkParameters"

instance ToSchema ApiEra where
    declareNamedSchema _ = declareSchemaForDefinition "ApiEra"

instance ToSchema ApiEraInfo where
    declareNamedSchema _ = declareSchemaForDefinition "ApiEraInfo"

instance ToSchema ApiSlotReference where
    declareNamedSchema _ = declareSchemaForDefinition "ApiSlotReference"

instance ToSchema ApiBlockReference where
    declareNamedSchema _ = declareSchemaForDefinition "ApiBlockReference"

instance ToSchema ApiWalletDelegationStatus where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegationStatus"

instance ToSchema ApiWalletDelegationNext where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegationNext"

instance ToSchema ApiWalletDelegation where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWalletDelegation"

instance ToSchema ApiPostRandomAddressData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostRandomAddressData"

instance ToSchema ApiWalletSignData where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        declareSchemaForDefinition "ApiWalletSignData"

instance ToSchema ApiPostAccountKeyData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostAccountKeyData"

instance ToSchema ApiPostAccountKeyDataWithPurpose where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostAccountKeyDataWithPurpose"

instance ToSchema ApiAccountKey where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAccountKey"

instance ToSchema ApiPostPolicyKeyData where
    declareNamedSchema _ = declareSchemaForDefinition "ApiPostPolicyKeyData"

instance ToSchema ApiAccountKeyShared where
    declareNamedSchema _ = declareSchemaForDefinition "ApiAccountKeyShared"

instance Typeable n => ToSchema (ApiStakeKeys n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiStakeKeys"

instance Typeable n => ToSchema (ApiOurStakeKey n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiOurStakeKey"

instance Typeable n => ToSchema (ApiForeignStakeKey n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiForeignStakeKey"

instance ToSchema ApiNullStakeKey where
    declareNamedSchema _ = declareSchemaForDefinition "ApiNullStakeKey"

instance ToSchema ApiTokenAmountFingerprint  where
    declareNamedSchema _ =
        declareSchemaForDefinition "ApiTokenAmountFingerprint"

instance ToSchema ApiTokens  where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        declareSchemaForDefinition "ApiTokens"

instance ToSchema ApiAssetMintBurn  where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        declareSchemaForDefinition "ApiAssetMintBurn"

instance Typeable n => ToSchema (ApiConstructTransactionData n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        addDefinition =<< declareSchemaForDefinition "ScriptTemplateValue"
        declareSchemaForDefinition "ApiConstructTransactionData"

instance Typeable n => ToSchema (ApiConstructTransaction n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiConstructTransaction"

instance ToSchema ApiMultiDelegationAction where
    declareNamedSchema _ = declareSchemaForDefinition "ApiMultiDelegationAction"

type ApiTxInputsGeneral n = [ApiTxInputGeneral n]

type ApiTxOutputsGeneral n = [ApiTxOutputGeneral n]

type ApiWithdrawalsGeneral n = [ApiWithdrawalGeneral n]

instance Typeable n => ToSchema (ApiTxInputsGeneral n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiInputsGeneral"

instance Typeable n => ToSchema (ApiTxOutputsGeneral n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiOutputsGeneral"

instance Typeable n => ToSchema (ApiWithdrawalsGeneral n) where
    declareNamedSchema _ = declareSchemaForDefinition "ApiWithdrawalsGeneral"

instance Typeable n => ToSchema (ApiDecodedTransaction n) where
    declareNamedSchema _ = do
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValue"
        addDefinition =<< declareSchemaForDefinition "TransactionMetadataValueNoSchema"
        addDefinition =<< declareSchemaForDefinition "ScriptValue"
        declareSchemaForDefinition "ApiDecodedTransaction"

instance ToSchema ApiBlockHeader where
    declareNamedSchema _ = declareSchemaForDefinition "ApiBlockHeader"

-- | Utility function to provide an ad-hoc 'ToSchema' instance for a definition:
-- we simply look it up within the Swagger specification.
declareSchemaForDefinition :: Text -> Declare (Definitions Schema) NamedSchema
declareSchemaForDefinition ref = do
    let json = foldl' unsafeLookupKey specification ["components","schemas",ref]
    case Aeson.eitherDecode' $ Aeson.encode json of
        Left err -> error $
            "unable to decode schema for definition '" <> T.unpack ref <> "': " <> show err
        Right schema ->
            return $ NamedSchema (Just ref) schema

-- | Add a known definition to the set of definitions, this may be necessary
-- when we can't inline a definition because it is recursive or, when a
-- definition is only used in an existing schema but has no top-level type for
-- which to define a 'ToSchema' instance.
addDefinition :: NamedSchema -> Declare (Definitions Schema) ()
addDefinition (NamedSchema Nothing _) =
    error "Trying to add definition for an unnamed NamedSchema!"
addDefinition (NamedSchema (Just k) s) = do
    defs <- look
    declare $ defs & at k ?~ s

unsafeLookupKey :: Aeson.Value -> Text -> Aeson.Value
unsafeLookupKey json k = case json of
    Aeson.Object m -> fromMaybe bombMissing (Aeson.lookup (Aeson.fromText k) m)
    m -> bombNotObject m
  where
    bombNotObject m =
        error $ "given JSON value is NOT an object: " <> show m
    bombMissing =
        error $ "no value found in map for key: " <> T.unpack k

data ApiEndpoint = ApiEndpoint
    { apiEndpointVerb :: String
    , apiEndpointPath :: String
    }

instance Show ApiEndpoint where
    show (ApiEndpoint verb path) = verb <> " " <> path

everyApiEndpoint :: ExtractEndpoints api => Proxy api -> [ApiEndpoint]
everyApiEndpoint p = extractEndpoints p []

-- | Verify that all servant endpoints are present and match the specification
class ExtractEndpoints api where
    extractEndpoints :: Proxy api -> [String] -> [ApiEndpoint]

instance {-# OVERLAPPING #-} (ExtractEndpoints sub, KnownSymbol path) =>
    ExtractEndpoints (path :> sub) where
        extractEndpoints _ prefixes = do
            let prefixes' = symbolVal (Proxy @path) : prefixes
            extractEndpoints (Proxy @sub) prefixes'

instance {-# OVERLAPPING #-} (ExtractEndpoints left, ExtractEndpoints right) =>
    ExtractEndpoints (left :<|> right) where
        extractEndpoints _ paths =
            extractEndpoints (Proxy @left) paths
            <> extractEndpoints (Proxy @right) paths

instance {-# OVERLAPPABLE #-} GetPath a => ExtractEndpoints a where
    extractEndpoints proxy prefixes = do
        let (verb, subPath) = first show (getPath proxy)
            path = "/" <> intercalate "/" (reverse prefixes) <> subPath
        [ApiEndpoint verb path]

-- | Extract the path of a given endpoint, in a format that is swagger-friendly
class GetPath api where
    getPath :: Proxy api -> (StdMethod, String)

instance (Method m) => GetPath (Verb m s ct a) where
    getPath _ = (method (Proxy @m), "")

instance (Method m) => GetPath (NoContentVerb m) where
    getPath _ = (method (Proxy @m), "")

instance (KnownSymbol path, GetPath sub) => GetPath (path :> sub) where
    getPath _ =
        getPath (Proxy @sub) & \(verb, sub) ->
            (verb, "/" <> symbolVal (Proxy @path) <> sub)

instance (KnownSymbol param, GetPath sub) => GetPath (Capture param t :> sub)
  where
    getPath _ =
         case symbolVal (Proxy :: Proxy param) of
            sym | sym == "*" ->
                getPath (Proxy @sub) & \(verb, sub) ->
                    (verb, "/" <> sym <> sub)
            sym ->
                getPath (Proxy @sub) & \(verb, sub) ->
                    (verb, "/{" <> sym <> "}" <> sub)

instance GetPath sub => GetPath (ReqBody a b :> sub) where
    getPath _ = getPath (Proxy @sub)

instance GetPath sub => GetPath (QueryParam a b :> sub) where
    getPath _ = getPath (Proxy @sub)

instance GetPath sub => GetPath (QueryFlag sym :> sub) where
    getPath _ = getPath (Proxy @sub)

instance GetPath sub => GetPath (Header' opts name ty :> sub) where
    getPath _ = getPath (Proxy @sub)

-- A way to demote 'StdMethod' back to the world of values. Servant provides a
-- 'reflectMethod' that does just that, but demote types to raw 'ByteString' for
-- an unknown reason :/
instance Method 'GET where method _ = GET
instance Method 'POST where method _ = POST
instance Method 'PUT where method _ = PUT
instance Method 'DELETE where method _ = DELETE
instance Method 'PATCH where method _ = PATCH
class Method (m :: StdMethod) where
    method :: Proxy m -> StdMethod

{-------------------------------------------------------------------------------
            Generating Golden Test Vectors For Address Encoding
-------------------------------------------------------------------------------}

-- SPENDINGKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
-- DELEGATIONKEY=$(jcli key generate --type Ed25519Extended | jcli key to-public)
--
-- SPENDINGKEYBYTES=$(echo $SPENDINGKEY | jcli key to-bytes)
-- DELEGATIONKEYBYTES=$(echo $DELEGATIONKEY | jcli key to-bytes)
--
-- MAINNETSINGLE=$(jcli address single $SPENDINGKEY --prefix addr)
-- TESTNETSINGLE=$(jcli address single $SPENDINGKEY --testing --prefix addr)
--
-- MAINNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY --prefix addr)
-- TESTNETGROUPED=$(jcli address single $SPENDINGKEY $DELEGATIONKEY --testing --prefix addr)
--
-- TESTVECTOR=test_vector_$(date +%s)
-- touch $TESTVECTOR
-- echo "spending key:        $SPENDINGKEYBYTES" >> $TESTVECTOR
-- echo "\ndelegation key:    $DELEGATIONKEYBYTES" >> $TESTVECTOR
-- echo "\nsingle (mainnet):  $MAINNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (mainnet): $MAINNETGROUPED" >> $TESTVECTOR
-- echo "\nsingle (testnet):  $TESTNETSINGLE" >> $TESTVECTOR
-- echo "\ngrouped (testnet): $TESTNETGROUPED" >> $TESTVECTOR
--
-- echo -e $(cat $TESTVECTOR)
-- echo "Saved as $TESTVECTOR."
