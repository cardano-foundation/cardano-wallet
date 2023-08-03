{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API type representations of various types. We define here pretty much all our
-- user-facing types that are mostly composed with internal / primitive types.
--
-- This module also define required API instances (JSON, HttpApiData...) for all
-- those types, making sure to match the specification document:
--
-- <https://github.com/cardano-foundation/cardano-wallet/blob/master/specifications/api/swagger.yaml Wallet API Specification>

module Cardano.Wallet.Api.Types
    (
    -- * Wallet Styles
      WalletStyle (..)
    , ByronWalletStyle (..)
    , StyleSymbol
    , AllowedMnemonics
    , fmtAllowedWords

    -- * API Types
    , ApiAddress (..)
    , AddressAmount (..)
    , AddressAmountNoAssets (..)
    , AnyAddress (..)
    , AnyAddressType (..)
    , ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiAddressWithPath (..)
    , ApiAddressData (..)
    , ApiAddressDataPayload (..)
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiAnyCertificate (..)
    , ApiAsset (..)
    , ApiAssetMetadata (..)
    , ApiAssetMintBurn (..)
    , ApiBalanceTransactionPostData (..)
    , ApiBase64
    , ApiBlockInfo (..)
    , ApiBlockReference (..)
    , ApiBurnData(..)
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
    , ApiForeignStakeKey (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , ApiMintBurnData (..)
    , ApiMintBurnOperation (..)
    , ApiMintData(..)
    , ApiMultiDelegationAction (..)
    , ApiNetworkClock (..)
    , ApiNetworkInfo (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters (..)
    , ApiNullStakeKey (..)
    , ApiOurStakeKey (..)
    , ApiPaymentDestination (..)
    , ApiPoolSpecifier (..)
    , ApiPolicyId (..)
    , ApiPolicyKey (..)
    , ApiPostAccountKeyData (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiPostPolicyIdData (..)
    , ApiPostPolicyKeyData (..)
    , ApiRedeemer (..)
    , ApiRegisterPool (..)
    , ApiScriptTemplateEntry (..)
    , ApiScriptTemplate (..)
    , ApiSealedTxEncoding (..)
    , ApiSelectCoinsAction (..)
    , ApiSelectCoinsData (..)
    , ApiSelectCoinsPayments (..)
    , ApiSelfWithdrawalPostData (..)
    , ApiSerialisedTransaction (..)
    , ApiSignTransactionPostData (..)
    , ApiSlotId (..)
    , ApiSlotReference (..)
    , ApiStakeKeyIndex (..)
    , ApiStakeKeys (..)
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
    , ApiValidityBound (..)
    , ApiValidityInterval (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeyShelley (..)
    , ApiWallet (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletBalance (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationNext (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletInput (..)
    , ApiWalletMigrationBalance (..)
    , ApiWalletMigrationPlan (..)
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletMode (..)
    , ApiWalletOutput (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWalletSignData (..)
    , ApiWalletUtxoSnapshot (..)
    , ApiWalletUtxoSnapshotEntry (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalGeneral (..)
    , ApiWithdrawalPostData (..)
    , ApiRewardAccount (..)
    , fromApiEra
    , Iso8601Time (..)
    , KeyFormat (..)
    , MaintenanceAction (..)
    , MinWithdrawal (..)
    , NtpSyncingStatus (..)
    , PostTransactionFeeOldData (..)
    , PostTransactionOldData (..)
    , ResourceContext (..)
    , SettingsPutData (..)
    , toApiAsset
    , toApiAssetMetadata
    , toApiEra
    , toApiNetworkParameters
    , toApiUtxoStatistics
    , VerificationKeyHashing (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , WalletPutPassphraseMnemonicData (..)
    , WalletPutPassphraseOldPassphraseData (..)
    , XPubOrSelf (..)

    -- * API Types (Byron)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ByronWalletPostData (..)
    , SomeByronWalletPostData (..)
    , ByronWalletFromXPrvPostData (..)
    , ByronWalletPutPassphraseData (..)
    , ApiPostRandomAddressData (..)
    , ApiWalletDiscovery (..)
    , KnownDiscovery(..)
    , ApiPutAddressesData (..)

    -- * API Types (Hardware)
    , AccountPostData (..)
    , ApiAccountPublicKey (..)
    , ApiAccountSharedPublicKey (..)
    , WalletOrAccountPostData (..)

    -- * Shared Wallets
    , ApiSharedWallet (..)
    , ApiIncompleteSharedWallet (..)
    , ApiActiveSharedWallet (..)
    , ApiSharedWalletPostData (..)
    , ApiSharedWalletPostDataFromMnemonics (..)
    , ApiSharedWalletPostDataFromAccountPubX (..)
    , ApiSharedWalletPatchData (..)

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)
    , ApiBytesT (..)

    -- * Type families
    , ApiAddressIdT
    , ApiAddressT
    , ApiBalanceTransactionPostDataT
    , ApiCoinSelectionT
    , ApiConstructTransactionDataT
    , ApiConstructTransactionT
    , ApiDecodedTransactionT
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiStakeKeysT
    , ApiTransactionT
    , ApiWalletMigrationPlanPostDataT
    , ApiWalletMigrationPostDataT
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT

    -- * Others
    , defaultRecordTypeOptions
    , strictRecordTypeOptions
    , HealthStatusSMASH (..)
    , ApiHealthCheck (..)
    , ApiAsArray (..)

    -- * Re-exports
    , Base (Base16, Base64)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xpubFromBytes, xpubToBytes )
import Cardano.Address.Script
    ( Cosigner (..)
    , KeyHash (..)
    , Script
    , ScriptHash (..)
    , ScriptTemplate
    , ValidationLevel (..)
    )
import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , StakeAddress
    , deserialiseFromBech32
    , proxyToAsType
    , serialiseToBech32
    )
import Cardano.Mnemonic
    ( MkSomeMnemonic (..)
    , MkSomeMnemonicError (..)
    , SomeMnemonic (..)
    , mnemonicToText
    , natVals
    )
import Cardano.Pool.Metadata
    ( HealthCheckSMASH, HealthStatusSMASH (..), SMASHPoolId (..) )
import Cardano.Pool.Metadata.Types
    ( PoolMetadataGCStatus (..), StakePoolMetadata (..) )
import Cardano.Pool.Types
    ( PoolId (..), decodePoolIdBech32, encodePoolIdBech32 )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), DerivationIndex (..), Index (..) )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap, SeqState, getAddressPoolGap )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..) )
import Cardano.Wallet.Api.Aeson
    ( eitherToParser )
import Cardano.Wallet.Api.Aeson.Variant
    ( variant, variants )
import Cardano.Wallet.Api.Hex
    ( fromHexText, hexText )
import Cardano.Wallet.Api.Lib.ApiAsArray
    ( ApiAsArray (..) )
import Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..), fromTextApiT, toTextApiT )
import Cardano.Wallet.Api.Lib.Options
    ( DefaultRecord (..)
    , DefaultSum (..)
    , TaggedObjectOptions (..)
    , defaultRecordTypeOptions
    , defaultSumTypeOptions
    , explicitNothingRecordTypeOptions
    , strictRecordTypeOptions
    , taggedSumTypeOptions
    )
import Cardano.Wallet.Api.Types.Certificate
    ( ApiAnyCertificate (..)
    , ApiCertificate (..)
    , ApiDeregisterPool (..)
    , ApiExternalCertificate (..)
    , ApiRegisterPool (..)
    , ApiRewardAccount (..)
    )
import Cardano.Wallet.Api.Types.Key
    ( ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiPolicyKey (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeyShelley (..)
    , KeyFormat (..)
    , VerificationKeyHashing (..)
    )
import Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn (..), ApiTokenAmountFingerprint (..), ApiTokens (..) )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataWithSchema )
import Cardano.Wallet.Api.Types.Transaction
    ( AddressAmount (..)
    , ApiAddress (..)
    , ApiDecodedTransaction (..)
    , ApiPostPolicyKeyData (..)
    , ApiTxInputGeneral (..)
    , ApiTxMetadata (..)
    , ApiTxOutput
    , ApiTxOutputGeneral (..)
    , ApiValidityIntervalExplicit (..)
    , ApiWalletInput (..)
    , ApiWalletOutput (..)
    , ApiWithdrawal (..)
    , ApiWithdrawalGeneral (..)
    , ResourceContext (..)
    )
import Cardano.Wallet.Pools
    ( EpochInfo, StakePool (..), StakePoolFlag, StakePoolMetrics )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..), NetworkDiscriminant )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..), PassphraseHash (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , EpochLength (..)
    , EpochNo (..)
    , ExecutionUnitPrices (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SmashServer (..)
    , StartTime (..)
    , WalletId (..)
    , WalletName (..)
    , getDecentralizationLevel
    , unsafeEpochNo
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..), TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , SerialisedTx (..)
    , TxMetadata
    , TxScriptValidity (..)
    , sealedTxFromBytes
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction, TxStatus )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( BoundType, HistogramBar (..), UTxOStatistics (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( decodeAddress, encodeAddress )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataError (..) )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import "cardano-addresses" Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, encode )
import Control.Applicative
    ( optional, (<|>) )
import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( guard, when, (<=<), (>=>) )
import Data.Aeson.Types
    ( FromJSON (..)
    , Parser
    , SumEncoding (..)
    , ToJSON (..)
    , Value (Null, Object, String)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , object
    , omitNothingFields
    , sumEncoding
    , tagSingleConstructors
    , withObject
    , withText
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteArray
    ( ByteArray, ByteArrayAccess )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( toLower )
import Data.Data
    ( Data )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Either.Extra
    ( eitherToMaybe, maybeToEither )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Hashable
    ( Hashable )
import Data.Kind
    ( Type )
import Data.List
    ( intercalate )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
import Data.String
    ( IsString )
import Data.Text
    ( Text, split )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Time.Text
    ( iso8601, iso8601ExtendedUtc, utcTimeFromText, utcTimeToText )
import Data.Traversable
    ( for )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word16, Word32, Word64, Word8 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Nat, Symbol )
import Network.Ntp
    ( NtpStatusWithOffset, NtpSyncingStatus (..) )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )
import Servant.API
    ( MimeRender (..), MimeUnrender (..), OctetStream )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Address.Derivation as AD
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

{-------------------------------------------------------------------------------
                               Styles of Wallets
-------------------------------------------------------------------------------}

data WalletStyle
    = Shelley
    | Byron
    | Shared

data ByronWalletStyle
    = Random
    | Icarus
    | Trezor
    | Ledger
    deriving (Show, Generic, Eq, Bounded, Enum)

instance FromText ByronWalletStyle where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText ByronWalletStyle where
    toText = toTextFromBoundedEnum SnakeLowerCase

data SndFactor
    = SndFactor

type family StyleSymbol (style :: ByronWalletStyle) :: Symbol where
    StyleSymbol 'Random  = "random"
    StyleSymbol 'Icarus  = "icarus"
    StyleSymbol 'Trezor  = "trezor"
    StyleSymbol 'Ledger  = "ledger"

type family AllowedMnemonics (style :: k) :: [Nat]

type instance AllowedMnemonics 'Random    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Icarus    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Trezor    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Ledger    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Shelley   = '[15,18,21,24]
type instance AllowedMnemonics 'SndFactor = '[9,12]

fmtAllowedWords :: ByronWalletStyle -> String
fmtAllowedWords =
    (++ " mnemonic words") . formatEnglishEnumeration . allowedWordLengths
  where
    allowedWordLengths = \case
        Random -> map show $ natVals $ Proxy @(AllowedMnemonics 'Random)
        Icarus -> map show $ natVals $ Proxy @(AllowedMnemonics 'Icarus)
        Trezor -> map show $ natVals $ Proxy @(AllowedMnemonics 'Trezor)
        Ledger -> map show $ natVals $ Proxy @(AllowedMnemonics 'Ledger)

      -- >>> formatEnglishEnumeration ["a", "b", "c"]
      -- "a, b or c"
      --
      -- >>> formatEnglishEnumeration ["a", "b"]
      -- "a or b"
      --
      -- >>> formatEnglishEnumeration ["a"]
      -- "a"
    formatEnglishEnumeration = formatEnglishEnumerationRev . reverse
    formatEnglishEnumerationRev [ult, penult]
       = penult ++ " or " ++ ult
    formatEnglishEnumerationRev (ult:penult:revBeginning)
       = intercalate ", " (reverse revBeginning)
           ++ ", "
           ++ penult
           ++ " or "
           ++ ult
    formatEnglishEnumerationRev xs = intercalate ", " (reverse xs)

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data MaintenanceAction = GcStakePools
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultSum MaintenanceAction

newtype ApiMaintenanceActionPostData = ApiMaintenanceActionPostData
    { maintenanceAction :: MaintenanceAction
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiMaintenanceActionPostData
    deriving Show via (Quiet ApiMaintenanceActionPostData)

newtype ApiMaintenanceAction = ApiMaintenanceAction
    { gcStakePools :: ApiT PoolMetadataGCStatus
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiMaintenanceAction
    deriving Show via (Quiet ApiMaintenanceAction)

newtype ApiPolicyId = ApiPolicyId
    { policyId :: ApiT W.TokenPolicyId
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiPolicyId

newtype ApiPostPolicyIdData = ApiPostPolicyIdData
    { policyScriptTemplate :: (ApiT (Script Cosigner))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiPostPolicyIdData

data ApiAsset = ApiAsset
    { policyId :: ApiT W.TokenPolicyId
    , assetName :: ApiT W.TokenName
    , fingerprint :: ApiT W.TokenFingerprint
    , metadata :: Maybe ApiAssetMetadata
    , metadataError :: Maybe ApiMetadataError
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiAsset
    deriving anyclass NFData

data ApiMetadataError = Fetch | Parse
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultSum ApiMetadataError
    deriving anyclass NFData

data ApiAssetMetadata = ApiAssetMetadata
    { name :: Text
    , description :: Text
    , ticker :: Maybe Text
    , url :: Maybe (ApiT W.AssetURL)
    , logo :: Maybe (ApiT W.AssetLogo)
    , decimals :: Maybe (ApiT W.AssetDecimals)
    }
    deriving (Eq, Generic, Ord, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiAssetMetadata
    deriving anyclass NFData

toApiAsset
    :: Either TokenMetadataError (Maybe W.AssetMetadata)
    -> AssetId
    -> ApiAsset
toApiAsset metadata_ (AssetId policyId_ assetName_) = ApiAsset
    { policyId = ApiT policyId_
    , assetName = ApiT assetName_
    , fingerprint = ApiT $ W.mkTokenFingerprint policyId_ assetName_
    , metadata = either (const Nothing) (fmap toApiAssetMetadata) metadata_
    , metadataError = either (Just . category) (const Nothing) metadata_
    }
  where
    category = \case
        TokenMetadataClientError _ -> Fetch
        TokenMetadataFetchError _ -> Fetch
        TokenMetadataJSONParseError _ _ -> Parse

toApiAssetMetadata :: W.AssetMetadata -> ApiAssetMetadata
toApiAssetMetadata W.AssetMetadata{name,description,ticker,url,logo,decimals} =
    ApiAssetMetadata name description ticker
        (ApiT <$> url) (ApiT <$> logo) (ApiT <$> decimals)

data ApiAddressWithPath (n :: NetworkDiscriminant) = ApiAddressWithPath
    { id :: !(ApiAddress n)
    , state :: !(ApiT AddressState)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiAddressWithPath n)
    deriving anyclass NFData

newtype ApiCosignerIndex = ApiCosignerIndex Word8
    deriving stock (Data, Eq, Generic, Show, Typeable)
    deriving newtype (FromJSON, ToJSON)
    deriving anyclass NFData

data ApiCredential =
      CredentialExtendedPubKey ByteString
    | CredentialPubKey ByteString
    | CredentialKeyHash ByteString
    | CredentialScript (Script KeyHash)
    | CredentialScriptHash ScriptHash
    deriving (Eq, Generic, Show)

newtype ApiCredentialType = ApiCredentialType
    { unApiCredentialType :: CredentialType
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultSum CredentialType
    deriving anyclass NFData

data ApiAddressData = ApiAddressData
    { address :: !ApiAddressDataPayload
    , validationLevel :: !(Maybe (ApiT ValidationLevel))
    }
    deriving (Eq, Generic, Show)

data ApiAddressDataPayload =
      AddrEnterprise ApiCredential
    | AddrRewardAccount ApiCredential
    | AddrBase ApiCredential ApiCredential
    deriving (Eq, Generic, Show)

data AnyAddressType =
      EnterpriseDelegating
    | RewardAccount
    deriving (Eq, Show, Bounded, Enum)

data AnyAddress = AnyAddress
    { payload :: ByteString
    , flavour :: AnyAddressType
    , network :: Int
    }
    deriving (Eq, Generic, Show)

data ApiSelectCoinsData (n :: NetworkDiscriminant)
    = ApiSelectForPayment (ApiSelectCoinsPayments n)
    | ApiSelectForDelegation ApiSelectCoinsAction
    deriving (Eq, Generic, Show, Typeable)

data ApiSelectCoinsPayments (n :: NetworkDiscriminant) = ApiSelectCoinsPayments
    { payments :: NonEmpty (ApiTxOutput n)
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiSelectCoinsPayments n)

newtype ApiSelectCoinsAction = ApiSelectCoinsAction
    { delegationAction :: ApiDelegationAction
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiSelectCoinsAction
    deriving Show via (Quiet ApiSelectCoinsAction)

data ApiDelegationAction = Join (ApiT PoolId) | Quit
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiCoinSelection (n :: NetworkDiscriminant) = ApiCoinSelection
    { inputs :: ![ApiWalletInput n]
    , outputs :: ![ApiCoinSelectionOutput n]
    , change :: ![ApiCoinSelectionChange n]
    , collateral :: ![ApiCoinSelectionCollateral n]
    , withdrawals :: ![ApiCoinSelectionWithdrawal n]
    , certificates :: Maybe (NonEmpty ApiCertificate)
    , depositsTaken :: ![Quantity "lovelace" Natural]
    , depositsReturned :: ![Quantity "lovelace" Natural]
    , metadata :: !(Maybe ApiBase64)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiCoinSelection n)
    deriving anyclass NFData

data ApiCoinSelectionChange (n :: NetworkDiscriminant) = ApiCoinSelectionChange
    { address :: !(ApiAddress n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT TokenMap)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiCoinSelectionChange n)
    deriving anyclass NFData

data ApiCoinSelectionOutput (n :: NetworkDiscriminant) = ApiCoinSelectionOutput
    { address :: !(ApiAddress n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT TokenMap)
    }
    deriving (Eq, Ord, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiCoinSelectionOutput n)
    deriving anyclass (NFData, Hashable)

data ApiCoinSelectionCollateral (n :: NetworkDiscriminant) =
    ApiCoinSelectionCollateral
        { id :: !(ApiT (Hash "Tx"))
        , index :: !Word32
        , address :: !(ApiAddress n)
        , derivationPath :: NonEmpty (ApiT DerivationIndex)
        , amount :: !(Quantity "lovelace" Natural)
        }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiCoinSelectionCollateral n)
    deriving anyclass NFData

data ApiWallet = ApiWallet
    { id :: !(ApiT WalletId)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , balance :: !ApiWalletBalance
    , assets :: !ApiWalletAssetsBalance
    , delegation :: !ApiWalletDelegation
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe ApiWalletPassphraseInfo)
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWallet
    deriving anyclass NFData

data ApiWalletBalance = ApiWalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    , reward :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletBalance
    deriving anyclass NFData

data ApiWalletAssetsBalance = ApiWalletAssetsBalance
    { available :: !(ApiT TokenMap)
    , total :: !(ApiT TokenMap)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletAssetsBalance
    deriving anyclass NFData

newtype ApiWalletPassphraseInfo = ApiWalletPassphraseInfo
    { lastUpdatedAt :: UTCTime
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletPassphraseInfo
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletPassphraseInfo)

data ApiWalletDelegation = ApiWalletDelegation
    { active :: !ApiWalletDelegationNext
    , next :: ![ApiWalletDelegationNext]
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletDelegation
    deriving anyclass NFData

data ApiWalletDelegationNext = ApiWalletDelegationNext
    { status :: !ApiWalletDelegationStatus
    , target :: !(Maybe (ApiT PoolId))
    , changesAt :: !(Maybe EpochInfo)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletDelegationNext
    deriving anyclass NFData

data ApiWalletDelegationStatus
    = NotDelegating
    | Delegating
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultSum ApiWalletDelegationStatus
    deriving anyclass NFData

newtype ApiWalletPassphrase = ApiWalletPassphrase
    { passphrase :: ApiT (Passphrase "lenient")
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletPassphrase
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletPassphrase)

newtype ApiWalletUtxoSnapshot = ApiWalletUtxoSnapshot
    { entries :: [ApiWalletUtxoSnapshotEntry]
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletUtxoSnapshot
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletUtxoSnapshot)

data ApiWalletUtxoSnapshotEntry = ApiWalletUtxoSnapshotEntry
    { ada :: !(Quantity "lovelace" Natural)
    , adaMinimum :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT TokenMap)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletUtxoSnapshotEntry
    deriving anyclass NFData

data ApiUtxoStatistics = ApiUtxoStatistics
    { total :: !(Quantity "lovelace" Natural)
    , scale :: !(ApiT BoundType)
    , distribution :: !(Map Word64 Word64)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiUtxoStatistics
    deriving anyclass NFData

toApiUtxoStatistics :: UTxOStatistics -> ApiUtxoStatistics
toApiUtxoStatistics (UTxOStatistics histo totalStakes bType) =
    ApiUtxoStatistics
    { total = Quantity (fromIntegral totalStakes)
    , scale = ApiT bType
    , distribution = Map.fromList $ map (\(HistogramBar k v)-> (k,v)) histo
    }

data WalletPostData = WalletPostData
    { addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    , mnemonicSentence :: !(ApiMnemonicT (AllowedMnemonics 'Shelley))
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT (AllowedMnemonics 'SndFactor)))
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "user"))
    }
    deriving (FromJSON, ToJSON) via DefaultRecord WalletPostData
    deriving (Eq, Generic, Show)

data SomeByronWalletPostData
    = RandomWalletFromMnemonic (ByronWalletPostData (AllowedMnemonics 'Random))
    | RandomWalletFromXPrv ByronWalletFromXPrvPostData
    | SomeIcarusWallet (ByronWalletPostData (AllowedMnemonics 'Icarus))
    | SomeTrezorWallet (ByronWalletPostData (AllowedMnemonics 'Trezor))
    | SomeLedgerWallet (ByronWalletPostData (AllowedMnemonics 'Ledger))
    | SomeAccount AccountPostData
    deriving (Eq, Generic, Show)

data ByronWalletPostData mw = ByronWalletPostData
    { mnemonicSentence :: !(ApiMnemonicT mw)
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "user"))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ByronWalletPostData mw)

data ByronWalletFromXPrvPostData = ByronWalletFromXPrvPostData
    { name :: !(ApiT WalletName)
    , encryptedRootPrivateKey :: !(ApiT XPrv)
    -- ^ A root private key hex-encoded, encrypted using a given passphrase.
    -- The underlying key should contain: private key, chain code, and public key
    , passphraseHash :: !(ApiT PassphraseHash)
    -- ^ A hash of master passphrase. The hash should be an output of a
    -- Scrypt function with the following parameters:
    -- - logN = 14
    -- - r = 8
    -- - p = 1
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ByronWalletFromXPrvPostData
    deriving anyclass NFData

newtype ApiAccountPublicKey = ApiAccountPublicKey
    { key :: (ApiT XPub)
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiAccountPublicKey)

newtype ApiAccountSharedPublicKey = ApiAccountSharedPublicKey
    { sharedKey :: (ApiT XPub)
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiAccountSharedPublicKey)

newtype WalletOrAccountPostData = WalletOrAccountPostData
    { postData :: Either WalletPostData AccountPostData
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet WalletOrAccountPostData)

data AccountPostData = AccountPostData
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !ApiAccountPublicKey
    , addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    }
    deriving (FromJSON, ToJSON) via DefaultRecord AccountPostData
    deriving (Eq, Generic, Show)

newtype WalletPutData = WalletPutData
    { name :: (Maybe (ApiT WalletName))
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord WalletPutData
    deriving Show via (Quiet WalletPutData)

newtype SettingsPutData = SettingsPutData
    { settings :: (ApiT W.Settings)
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord SettingsPutData
    deriving Show via (Quiet SettingsPutData)

data WalletPutPassphraseMnemonicData = WalletPutPassphraseMnemonicData
    { mnemonicSentence :: !(ApiMnemonicT (AllowedMnemonics 'Shelley))
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT (AllowedMnemonics 'SndFactor)))
    , newPassphrase :: !(ApiT (Passphrase "user"))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord WalletPutPassphraseMnemonicData

data WalletPutPassphraseOldPassphraseData = WalletPutPassphraseOldPassphraseData
    { oldPassphrase :: !(ApiT (Passphrase "user"))
    , newPassphrase :: !(ApiT (Passphrase "user"))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord WalletPutPassphraseOldPassphraseData

newtype WalletPutPassphraseData = WalletPutPassphraseData
    (  Either
            WalletPutPassphraseOldPassphraseData
            WalletPutPassphraseMnemonicData
    )
    deriving (Eq, Generic, Show)

data ByronWalletPutPassphraseData = ByronWalletPutPassphraseData
    { oldPassphrase :: !(Maybe (ApiT (Passphrase "lenient")))
    , newPassphrase :: !(ApiT (Passphrase "user"))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ByronWalletPutPassphraseData

data ApiSealedTxEncoding = HexEncoded | Base64Encoded
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance ToText ApiSealedTxEncoding where
    toText HexEncoded = "base16"
    toText Base64Encoded = "base64"

instance FromText ApiSealedTxEncoding where
    fromText txt = case txt of
        "base16" -> Right HexEncoded
        "base64" -> Right Base64Encoded
        _ -> Left $ TextDecodingError $ unwords
            [ "I couldn't parse the given sealed tx encoding."
            , "I am expecting one of the words 'base16' or"
            , "'base64'."]

instance FromJSON ApiSealedTxEncoding where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON ApiSealedTxEncoding where
    toJSON = toJSON . toText

data ApiConstructTransaction (n :: NetworkDiscriminant) = ApiConstructTransaction
    { transaction :: !ApiSerialisedTransaction
    , coinSelection :: !(ApiCoinSelection n)
    , fee :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

-- | Index of the stake key.
newtype ApiStakeKeyIndex = ApiStakeKeyIndex (ApiT DerivationIndex)
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Stake pool delegation certificates.
data ApiMultiDelegationAction
    = Joining !(ApiT PoolId) !ApiStakeKeyIndex
    -- ^ Delegate given staking index to a pool, possibly registering the stake
    -- key at the same time.
    | Leaving !ApiStakeKeyIndex
    -- ^ Undelegate the given staking index from its pool.
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Input parameters for transaction construction.
data ApiConstructTransactionData (n :: NetworkDiscriminant) =
    ApiConstructTransactionData
    { payments :: !(Maybe (ApiPaymentDestination n))
    , withdrawal :: !(Maybe ApiSelfWithdrawalPostData)
    , metadata :: !(Maybe TxMetadataWithSchema)
    , mintBurn :: !(Maybe (NonEmpty (ApiMintBurnData n)))
    , delegations :: !(Maybe (NonEmpty ApiMultiDelegationAction))
    , validityInterval :: !(Maybe ApiValidityInterval)
    , referencePolicyScriptTemplate :: !(Maybe (ApiT (Script Cosigner)))
    , encoding :: !(Maybe ApiSealedTxEncoding)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord (ApiConstructTransactionData n)
    deriving anyclass NFData

newtype ApiPaymentDestination (n :: NetworkDiscriminant)
    = ApiPaymentAddresses (NonEmpty (AddressAmount (ApiAddressIdT n)))
    -- ^ Pay amounts to one or more addresses.
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

-- | Times where transactions are valid.
data ApiValidityInterval = ApiValidityInterval
    { invalidBefore :: !(Maybe ApiValidityBound)
    -- ^ Tx is not valid before this time. Defaults to genesis.
    , invalidHereafter :: !(Maybe ApiValidityBound)
    -- ^ Tx is not valid at this time and after. Defaults to now + 2 hours.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiValidityInterval
    deriving anyclass NFData

-- | One side of the validity interval.
data ApiValidityBound
    = ApiValidityBoundUnspecified
    -- ^ Use the default.
    | ApiValidityBoundAsTimeFromNow !(Quantity "second" NominalDiffTime)
    -- ^ Time from transaction construction (not submission).
    | ApiValidityBoundAsSlot !(Quantity "slot" Word64)
    -- ^ Absolute slot number.
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiSignTransactionPostData = ApiSignTransactionPostData
    { transaction :: !(ApiT SealedTx)
    , passphrase :: !(ApiT (Passphrase "lenient"))
    , encoding :: !(Maybe ApiSealedTxEncoding)
    }
    deriving (Eq, Generic, Show)

-- | Legacy transaction API.
data PostTransactionOldData (n :: NetworkDiscriminant) = PostTransactionOldData
    { payments :: !(NonEmpty (ApiTxOutput n))
    , passphrase :: !(ApiT (Passphrase "lenient"))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe TxMetadataWithSchema)
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (PostTransactionOldData n)

-- | Legacy transaction API.
data PostTransactionFeeOldData (n :: NetworkDiscriminant) =
    PostTransactionFeeOldData
    { payments :: !(NonEmpty (ApiTxOutput n))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe TxMetadataWithSchema )
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (PostTransactionFeeOldData n)

type ApiBase64 = ApiBytesT 'Base64 ByteString

data ApiSerialisedTransaction = ApiSerialisedTransaction
    { serialisedTxSealed :: ApiT SealedTx
    , serialisedTxEncoding :: ApiSealedTxEncoding
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (NFData)

data ApiExternalInput (n :: NetworkDiscriminant) = ApiExternalInput
    { id :: !(ApiT (Hash "Tx"))
    , index :: !Word32
    , address :: !(ApiAddress n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT TokenMap)
    , datum :: !(Maybe (ApiT Write.DatumHash))
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiExternalInput n)
    deriving anyclass NFData

instance FromJSON (ApiT Write.DatumHash) where
    parseJSON = withText "DatumHash" $ \hex -> maybeToParser $ do
        bytes <- parseHex hex
        ApiT <$> Write.datumHashFromBytes bytes
      where
        maybeToParser = maybe failWithHelp pure
        failWithHelp = fail
            "expected hex-encoded 32-byte datum hash"

        parseHex :: Text -> Maybe ByteString
        parseHex = eitherToMaybe . fromHexText

instance ToJSON (ApiT Write.DatumHash) where
    toJSON (ApiT dh) = String $ hexText $ Write.datumHashToBytes dh

data ApiBalanceTransactionPostData (n :: NetworkDiscriminant) =
    ApiBalanceTransactionPostData
    { transaction :: !(ApiT SealedTx)
    , inputs :: ![ApiExternalInput n]
    , redeemers :: ![ApiRedeemer n]
    , encoding :: !(Maybe ApiSealedTxEncoding)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord (ApiBalanceTransactionPostData n)

type ApiRedeemerData = ApiBytesT 'Base16 ByteString

data ApiRedeemer (n :: NetworkDiscriminant)
    = ApiRedeemerSpending ApiRedeemerData (ApiT TxIn)
    | ApiRedeemerMinting ApiRedeemerData (ApiT W.TokenPolicyId)
    | ApiRedeemerRewarding ApiRedeemerData StakeAddress
    deriving (Eq, Generic, Show)

data ApiFee = ApiFee
    { estimatedMin :: !(Quantity "lovelace" Natural)
    , estimatedMax :: !(Quantity "lovelace" Natural)
    , minimumCoins :: ![Quantity "lovelace" Natural]
    , deposit :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiFee

data ApiNetworkParameters = ApiNetworkParameters
    { genesisBlockHash :: !(ApiT (Hash "Genesis"))
    , blockchainStartTime :: !(ApiT StartTime)
    , slotLength :: !(Quantity "second" NominalDiffTime)
    , epochLength :: !(Quantity "slot" Word32)
    , securityParameter :: !(Quantity "block" Word32)
    , activeSlotCoefficient :: !(Quantity "percent" Double)
    , decentralizationLevel :: !(Quantity "percent" Percentage)
    , desiredPoolNumber :: !Word16
    , maximumTokenBundleSize :: !(Quantity "byte" Natural)
    , eras :: !ApiEraInfo
    , maximumCollateralInputCount :: !Word16
    , minimumCollateralPercentage :: !Natural
    , executionUnitPrices :: !(Maybe ExecutionUnitPrices)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiNetworkParameters

data ApiEraInfo = ApiEraInfo
    { byron :: !(Maybe EpochInfo)
    , shelley :: !(Maybe EpochInfo)
    , allegra :: !(Maybe EpochInfo)
    , mary :: !(Maybe EpochInfo)
    , alonzo :: !(Maybe EpochInfo)
    , babbage :: !(Maybe EpochInfo)
    }
    deriving (Eq, Generic, Show)

toApiNetworkParameters
    :: Monad m
    => NetworkParameters
    -> (EpochNo -> m EpochInfo)
    -> m ApiNetworkParameters
toApiNetworkParameters (NetworkParameters gp sp pp) toEpochInfo = do
    byron <- traverse toEpochInfo (pp ^. #eras . #byron)
    shelley <- traverse toEpochInfo (pp ^. #eras . #shelley)
    allegra <- traverse toEpochInfo (pp ^. #eras . #allegra)
    mary <- traverse toEpochInfo (pp ^. #eras . #mary)
    alonzo <- traverse toEpochInfo (pp ^. #eras . #alonzo)
    babbage <- traverse toEpochInfo (pp ^. #eras . #babbage)

    let apiEras = ApiEraInfo { byron, shelley, allegra, mary, alonzo, babbage }

    return ApiNetworkParameters
        { genesisBlockHash = ApiT $ getGenesisBlockHash gp
        , blockchainStartTime = ApiT $ getGenesisBlockDate gp
        , slotLength = Quantity $ unSlotLength $ getSlotLength sp
        , epochLength = Quantity $ unEpochLength $ getEpochLength sp
        , securityParameter = getSecurityParameter sp
        , activeSlotCoefficient = Quantity
            $ (*100)
            $ unActiveSlotCoefficient
            $ getActiveSlotCoefficient sp
        , decentralizationLevel = Quantity
            $ getDecentralizationLevel
            $ view #decentralizationLevel pp
        , desiredPoolNumber = view #desiredNumberOfStakePools pp
        , eras = apiEras
        , maximumCollateralInputCount =
            view #maximumCollateralInputCount pp
        , minimumCollateralPercentage =
            view #minimumCollateralPercentage pp
        , maximumTokenBundleSize = Quantity $ pp ^.
            (#txParameters . #getTokenBundleMaxSize . #unTokenBundleMaxSize .
            #unTxSize)
        , executionUnitPrices = view #executionUnitPrices pp
        }

-- | This type is used in URLs where there is a '*' in place of a pool id,
-- which means "for all pool ids"
-- This is a hack to work around Servant's problem with capturing path params.
data ApiPoolSpecifier = AllPools | SpecificPool PoolId

instance FromHttpApiData ApiPoolSpecifier where
    parseUrlPiece t
        | t == "*" = Right AllPools
        | otherwise =
            SpecificPool <$> case fromText t of
                Left _ -> left (T.pack . show . ShowFmt) $ decodePoolIdBech32 t
                Right r -> Right r

instance ToHttpApiData ApiPoolSpecifier where
    toUrlPiece = \case
        AllPools -> "*"
        SpecificPool poolId -> encodePoolIdBech32 poolId

newtype ApiTxId = ApiTxId { id :: ApiT (Hash "Tx") }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiTxId
    deriving anyclass NFData
    deriving Show via (Quiet ApiTxId)

data ApiTransaction (n :: NetworkDiscriminant) = ApiTransaction
    { id :: !(ApiT (Hash "Tx"))
    , amount :: !(Quantity "lovelace" Natural)
    , fee :: !(Quantity "lovelace" Natural)
    , depositTaken :: !(Quantity "lovelace" Natural)
    , depositReturned :: !(Quantity "lovelace" Natural)
    , insertedAt :: !(Maybe ApiBlockReference)
    , pendingSince :: !(Maybe ApiBlockReference)
    , expiresAt :: !(Maybe ApiSlotReference)
    , depth :: !(Maybe (Quantity "block" Natural))
    , direction :: !(ApiT Direction)
    , inputs :: ![ApiTxInput n]
    , outputs :: ![ApiTxOutput n]
    , collateral :: ![ApiTxCollateral n]
    , collateralOutputs ::
        !(ApiAsArray "collateral_outputs" (Maybe (ApiTxOutput n)))
    , withdrawals :: ![ApiWithdrawal n]
    , status :: !(ApiT TxStatus)
    , metadata :: !(Maybe TxMetadataWithSchema)
    , scriptValidity :: !(Maybe (ApiT TxScriptValidity))
    , certificates :: [ApiAnyCertificate n]
    , mint :: ApiAssetMintBurn
    , burn :: ApiAssetMintBurn
    , validityInterval :: Maybe ApiValidityIntervalExplicit
    , scriptIntegrity :: Maybe (ApiT (Hash "ScriptIntegrity"))
    , extraSignatures :: [ApiT (Hash "ExtraSignature")]
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiTransaction n)
    deriving anyclass NFData

data ApiCoinSelectionWithdrawal (n :: NetworkDiscriminant) =
    ApiCoinSelectionWithdrawal
    { stakeAddress :: !(ApiRewardAccount n)
    , derivationPath :: !(NonEmpty (ApiT DerivationIndex))
    , amount :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiCoinSelectionWithdrawal n)
    deriving anyclass NFData

data ApiSelfWithdrawalPostData = SelfWithdraw
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiWithdrawalPostData
    = SelfWithdrawal
    | ExternalWithdrawal (ApiMnemonicT '[15,18,21,24])
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiTxInput (n :: NetworkDiscriminant) = ApiTxInput
    { source :: !(Maybe (ApiTxOutput n))
    , input :: !(ApiT TxIn)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

data ApiTxCollateral (n :: NetworkDiscriminant) = ApiTxCollateral
    { source :: !(Maybe (AddressAmountNoAssets (ApiAddress n)))
    , input :: !(ApiT TxIn)
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

data AddressAmountNoAssets addr = AddressAmountNoAssets
    { address :: !addr
    , amount :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (AddressAmountNoAssets addr)
    deriving anyclass NFData

newtype ApiAddressInspect = ApiAddressInspect
    { unApiAddressInspect :: Aeson.Value }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiAddressInspect)

newtype ApiAddressInspectData = ApiAddressInspectData
    { unApiAddressInspectData :: Text }
    deriving (Eq, Generic)
    deriving newtype (IsString)
    deriving anyclass NFData
    deriving Show via (Quiet ApiAddressInspectData)

data ApiSlotReference = ApiSlotReference
    { absoluteSlotNumber :: !(ApiT SlotNo)
    , slotId :: !ApiSlotId
    , time :: !UTCTime
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiSlotId = ApiSlotId
    { epochNumber :: !(ApiT EpochNo)
    , slotNumber :: !(ApiT SlotInEpoch)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiSlotId
    deriving anyclass NFData

data ApiBlockReference = ApiBlockReference
    { absoluteSlotNumber :: !(ApiT SlotNo)
    , slotId :: !ApiSlotId
    , time :: !UTCTime
    , block :: !ApiBlockInfo
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ApiBlockInfo = ApiBlockInfo
    { height :: Quantity "block" Natural
    }
    deriving (Eq, Generic)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiBlockInfo
    deriving anyclass NFData
    deriving Show via (Quiet ApiBlockInfo)

data ApiEra
    = ApiByron
    | ApiShelley
    | ApiAllegra
    | ApiMary
    | ApiAlonzo
    | ApiBabbage
    | ApiConway
    deriving (Data, Show, Eq, Generic, Enum, Ord, Bounded)
    deriving anyclass NFData

toApiEra :: AnyCardanoEra -> ApiEra
toApiEra (AnyCardanoEra ByronEra) = ApiByron
toApiEra (AnyCardanoEra ShelleyEra) = ApiShelley
toApiEra (AnyCardanoEra AllegraEra) = ApiAllegra
toApiEra (AnyCardanoEra MaryEra) = ApiMary
toApiEra (AnyCardanoEra AlonzoEra) = ApiAlonzo
toApiEra (AnyCardanoEra BabbageEra) = ApiBabbage
toApiEra (AnyCardanoEra ConwayEra) = ApiConway

fromApiEra :: ApiEra -> AnyCardanoEra
fromApiEra ApiByron = AnyCardanoEra ByronEra
fromApiEra ApiShelley = AnyCardanoEra ShelleyEra
fromApiEra ApiAllegra = AnyCardanoEra AllegraEra
fromApiEra ApiMary = AnyCardanoEra MaryEra
fromApiEra ApiAlonzo = AnyCardanoEra AlonzoEra
fromApiEra ApiBabbage = AnyCardanoEra BabbageEra
fromApiEra ApiConway = AnyCardanoEra ConwayEra

instance FromJSON ApiEra where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }
instance ToJSON ApiEra where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }

data ApiNetworkInfo = ApiNetworkInfo
    { networkId :: !Text
    , protocolMagic :: !Integer
    }
    deriving  (Eq, Show, Generic, NFData)

instance FromJSON ApiNetworkInfo where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { fieldLabelModifier =  camelTo2 '_' }

instance ToJSON ApiNetworkInfo where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { fieldLabelModifier =  camelTo2 '_' }

data ApiWalletMode = Light | Node
    deriving  (Eq, Show, Generic, NFData)

instance FromJSON ApiWalletMode where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { constructorTagModifier = fmap toLower }
instance ToJSON ApiWalletMode where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { constructorTagModifier = fmap toLower }

data ApiNetworkInformation = ApiNetworkInformation
    { syncProgress :: !(ApiT SyncProgress)
    , nextEpoch :: !(Maybe EpochInfo)
    , nodeTip :: !ApiBlockReference
    , networkTip :: !(Maybe ApiSlotReference)
    , nodeEra :: !ApiEra
    , networkInfo :: !ApiNetworkInfo
    , walletMode :: !ApiWalletMode
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiNetworkInformation
    deriving anyclass NFData

newtype ApiNetworkClock = ApiNetworkClock { ntpStatus :: NtpStatusWithOffset }
    deriving (Eq, Generic)
    deriving Show via (Quiet ApiNetworkClock)

data ApiPostRandomAddressData = ApiPostRandomAddressData
    { passphrase :: !(ApiT (Passphrase "lenient"))
    , addressIndex :: !(Maybe (ApiT (Index 'AD.Hardened 'CredFromKeyK)))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiPostRandomAddressData
    deriving anyclass NFData

newtype ApiWalletMigrationPlanPostData (n :: NetworkDiscriminant) =
    ApiWalletMigrationPlanPostData
    { addresses :: NonEmpty (ApiAddress n)
    }
    deriving (Eq, Generic, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord (ApiWalletMigrationPlanPostData n)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiWalletMigrationPlanPostData n))

data ApiWalletMigrationPostData (n :: NetworkDiscriminant) (s :: Symbol) =
    ApiWalletMigrationPostData
    { passphrase :: !(ApiT (Passphrase s))
    , addresses :: !(NonEmpty (ApiAddress n))
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord (ApiWalletMigrationPostData n s)
    deriving anyclass NFData

newtype ApiPutAddressesData (n :: NetworkDiscriminant) = ApiPutAddressesData
    { addresses :: [ApiAddress n]
    }
    deriving (Eq, Generic, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiPutAddressesData n)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiPutAddressesData n))

data ApiWalletMigrationBalance = ApiWalletMigrationBalance
    { ada :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT TokenMap)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletMigrationBalance
    deriving anyclass NFData

data ApiWalletMigrationPlan (n :: NetworkDiscriminant) = ApiWalletMigrationPlan
    { selections :: !(NonEmpty (ApiCoinSelection n))
    , totalFee :: Quantity "lovelace" Natural
    , balanceLeftover :: ApiWalletMigrationBalance
    , balanceSelected :: ApiWalletMigrationBalance
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiWalletMigrationPlan n)
    deriving anyclass NFData

newtype ApiWithdrawRewards = ApiWithdrawRewards Bool
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiWalletSignData = ApiWalletSignData
    { metadata :: ApiT TxMetadata
    , passphrase :: ApiT (Passphrase "lenient")
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiWalletSignData
    deriving anyclass NFData

instance FromHttpApiData KeyFormat where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

data ApiPostAccountKeyData = ApiPostAccountKeyData
    { passphrase :: ApiT (Passphrase "user")
    , format :: KeyFormat
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiPostAccountKeyData
    deriving anyclass NFData

data ApiPostAccountKeyDataWithPurpose = ApiPostAccountKeyDataWithPurpose
    { passphrase :: ApiT (Passphrase "user")
    , format :: KeyFormat
    , purpose :: Maybe (ApiT DerivationIndex)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiPostAccountKeyDataWithPurpose
    deriving anyclass NFData

data XPubOrSelf = SomeAccountKey XPub | Self
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiScriptTemplateEntry = ApiScriptTemplateEntry
    { cosigners :: Map Cosigner XPubOrSelf
    , template :: Script Cosigner
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ApiScriptTemplate = ApiScriptTemplate ScriptTemplate
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiSharedWalletPostDataFromMnemonics =
    ApiSharedWalletPostDataFromMnemonics
    { name :: !(ApiT WalletName)
    , mnemonicSentence :: !(ApiMnemonicT (AllowedMnemonics 'Shelley))
    , mnemonicSecondFactor
        :: !(Maybe (ApiMnemonicT (AllowedMnemonics 'SndFactor)))
    , passphrase :: !(ApiT (Passphrase "user"))
    , accountIndex :: !(ApiT DerivationIndex)
    , paymentScriptTemplate :: !ApiScriptTemplateEntry
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplateEntry)
    , scriptValidation :: !(Maybe (ApiT ValidationLevel))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiSharedWalletPostDataFromMnemonics

data ApiSharedWalletPostDataFromAccountPubX =
    ApiSharedWalletPostDataFromAccountPubX
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !ApiAccountSharedPublicKey
    , accountIndex :: !(ApiT DerivationIndex)
    , paymentScriptTemplate :: !ApiScriptTemplateEntry
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplateEntry)
    , scriptValidation :: !(Maybe (ApiT ValidationLevel))
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiSharedWalletPostDataFromAccountPubX

newtype ApiSharedWalletPostData = ApiSharedWalletPostData
    { wallet :: Either
        ApiSharedWalletPostDataFromMnemonics
        ApiSharedWalletPostDataFromAccountPubX
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet ApiSharedWalletPostData)

data ApiActiveSharedWallet = ApiActiveSharedWallet
    { id :: !(ApiT WalletId)
    , name :: !(ApiT WalletName)
    , accountIndex :: !(ApiT DerivationIndex)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , passphrase :: !(Maybe ApiWalletPassphraseInfo)
    , paymentScriptTemplate :: !ApiScriptTemplate
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplate)
    , delegation :: !ApiWalletDelegation
    , balance :: !ApiWalletBalance
    , assets :: !ApiWalletAssetsBalance
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiActiveSharedWallet
    deriving anyclass NFData

data ApiIncompleteSharedWallet = ApiIncompleteSharedWallet
    { id :: !(ApiT WalletId)
    , name :: !(ApiT WalletName)
    , accountIndex :: !(ApiT DerivationIndex)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , paymentScriptTemplate :: !ApiScriptTemplate
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplate)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ApiSharedWallet = ApiSharedWallet
    { wallet :: Either ApiIncompleteSharedWallet ApiActiveSharedWallet
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiSharedWallet)

data ApiSharedWalletPatchData = ApiSharedWalletPatchData
    { cosigner :: !(ApiT Cosigner)
    , accountPublicKey :: !ApiAccountSharedPublicKey
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Defines a point in time that can be formatted as and parsed from an
--   ISO 8601-compliant string.
--
newtype Iso8601Time = Iso8601Time
    { getIso8601Time :: UTCTime
    }
    deriving (Eq, Ord, Generic)
    deriving Show via (Quiet Iso8601Time)

instance ToText Iso8601Time where
    toText = utcTimeToText iso8601ExtendedUtc . getIso8601Time

instance FromText Iso8601Time where
    fromText t =
        Iso8601Time <$> maybeToEither err (utcTimeFromText iso8601 t)
      where
        err = TextDecodingError $ mempty
            <> "Unable to parse time argument: '"
            <> T.unpack t
            <> "'. Expecting ISO 8601 date-and-time format (basic or extended)"
            <> ", e.g. 2012-09-25T10:15:00Z."

instance FromJSON (ApiT Iso8601Time) where
    parseJSON = fromTextApiT "ISO-8601 Time"
instance ToJSON (ApiT Iso8601Time) where
    toJSON = toTextApiT

instance FromHttpApiData Iso8601Time where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

instance ToHttpApiData Iso8601Time where
    toUrlPiece = toText

newtype MinWithdrawal = MinWithdrawal
    { getMinWithdrawal :: Natural
    }
    deriving Generic
    deriving Show via (Quiet MinWithdrawal)

instance FromHttpApiData MinWithdrawal where
    parseUrlPiece = bimap (T.pack . getTextDecodingError) MinWithdrawal . fromText

instance ToHttpApiData MinWithdrawal where
    toUrlPiece = toText . getMinWithdrawal

instance ToText NtpSyncingStatus where
    toText NtpSyncingStatusUnavailable = "unavailable"
    toText NtpSyncingStatusPending = "pending"
    toText NtpSyncingStatusAvailable = "available"

instance FromText NtpSyncingStatus where
    fromText txt = case txt of
        "unavailable" -> Right NtpSyncingStatusUnavailable
        "pending" -> Right NtpSyncingStatusPending
        "available" -> Right NtpSyncingStatusAvailable
        _ -> Left $ TextDecodingError $ unwords
            [ "I couldn't parse the given ntp syncing status."
            , "I am expecting one of the words 'unavailable', 'pending' or"
            , "'available'."]

instance ToText (ApiT ValidationLevel) where
    toText (ApiT RequiredValidation) = "required"
    toText (ApiT RecommendedValidation) = "recommended"

instance FromText (ApiT ValidationLevel) where
    fromText txt = case txt of
        "required" -> Right $ ApiT RequiredValidation
        "recommended" -> Right $ ApiT RecommendedValidation
        _ -> Left $ TextDecodingError $ unwords
            [ "I couldn't parse the given validation level."
            , "I am expecting one of the words 'required' or"
            , "'recommended'."]

instance FromText ApiAccountPublicKey where
    fromText txt = case xpubFromText txt of
        Nothing ->
            Left $ TextDecodingError $ unwords
            [ "Invalid account public key: expecting a hex-encoded value"
            , "that is 64 bytes in length."]
        Just pubkey ->
            Right $ ApiAccountPublicKey $ ApiT pubkey
      where
        xpubFromText :: Text -> Maybe XPub
        xpubFromText = fmap eitherToMaybe fromHexText >=> xpubFromBytes

instance FromText ApiAccountSharedPublicKey where
    fromText txt =
        case detectEncoding (T.unpack txt) of
            Just EBech32{} -> do
                (hrp, dp) <- case Bech32.decodeLenient txt of
                    Left _ -> Left $ TextDecodingError "Extended account's Bech32 has invalid text."
                    Right res -> pure res
                let checkPayload bytes = case xpubFromBytes bytes of
                        Nothing -> Left $ TextDecodingError "Extended public key cannot be retrieved from a given bytestring"
                        Just validXPub -> pure $ ApiAccountSharedPublicKey $ ApiT validXPub
                let proceedWhenHrpCorrect = case Bech32.dataPartToBytes dp of
                        Nothing ->
                              Left $ TextDecodingError "Extended account has invalid Bech32 datapart."
                        Just bytes -> checkPayload bytes
                if Bech32.humanReadablePartToText hrp == "acct_shared_xvk"
                    then proceedWhenHrpCorrect
                    else Left $ TextDecodingError "Extended account must have 'acct_shared_xvk' prefix"
            _ -> Left $ TextDecodingError "Extended account must be must be encoded as Bech32."

instance FromText (ApiT XPrv) where
    fromText t = case convertFromBase Base16 $ T.encodeUtf8 t of
        Left _ ->
            textDecodingError
        Right (bytes :: ByteString) -> case CC.xprv bytes of
            Left _ -> textDecodingError
            Right val -> Right $ ApiT val
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid encrypted root private key:"
            , "expecting a hex-encoded value that is 128 "
            , "bytes in length."
            ]

instance {-# OVERLAPPING #-} Show (ApiT XPrv) where
    show _ = "<xprv>"

instance {-# OVERLAPPING #-} Eq (ApiT XPrv) where
    (ApiT val1) == (ApiT val2) = CC.unXPrv val1 == CC.unXPrv val2

instance ToText (ApiT XPrv) where
    toText = T.decodeUtf8
        . convertToBase Base16
        . CC.unXPrv
        . getApiT

instance FromText (ApiT PassphraseHash)  where
    fromText txt = case convertFromBase Base16 $ T.encodeUtf8 txt of
        Right bytes -> Right $ ApiT $ PassphraseHash bytes
        Left _ -> textDecodingError
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid encrypted passphrase:"
            , "expecting a hex-encoded value."
            ]

instance HasSNetworkId n => FromHttpApiData (ApiAddress n) where
    parseUrlPiece txt = do
        addr <- first (T.pack . getTextDecodingError)
            $ decodeAddress (sNetworkId @n) txt
        return (ApiAddress @n addr)

instance HasSNetworkId n => ToHttpApiData (ApiAddress n) where
    toUrlPiece = encodeAddress (sNetworkId @n) . apiAddress

{-------------------------------------------------------------------------------
                              API Types: Byron
-------------------------------------------------------------------------------}

data ApiByronWallet = ApiByronWallet
    { id :: !(ApiT WalletId)
    , balance :: !(ApiByronWalletBalance)
    , assets :: !ApiWalletAssetsBalance
    , discovery :: !ApiWalletDiscovery
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe ApiWalletPassphraseInfo)
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiByronWallet
    deriving anyclass NFData

data ApiWalletDiscovery
    = DiscoveryRandom
    | DiscoverySequential
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

class KnownDiscovery s where
    knownDiscovery :: ApiWalletDiscovery

instance KnownDiscovery (RndState network) where
    knownDiscovery = DiscoveryRandom

instance KnownDiscovery (SeqState network key) where
    knownDiscovery = DiscoverySequential

{-------------------------------------------------------------------------------
                              Polymorphic Types
-------------------------------------------------------------------------------}

-- | Polymorphic wrapper for byte arrays, parameterised by the desired string
-- encoding.
newtype ApiBytesT (base :: Base) bs = ApiBytesT { getApiBytesT :: bs }
    deriving (Generic, Eq, Functor)
    deriving newtype (Semigroup, Monoid, Hashable)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiBytesT base bs))

-- | Representation of mnemonics at the API-level, using a polymorphic type in
-- the lengths of mnemonics that are supported (and an underlying purpose). In
-- practice, mnemonics correspond to passphrases or seeds, and although they're
-- nice to manipulate as mnemonics from a user-perspective, carrying around a
-- list of words doesn't really make sense for the business logic, which prefers
-- manipulating scrubbed bytes directly.
--
-- @
-- data MyWallet
--     { mnemonic :: ApiMnemonicT '[15,18,21,24]
--     }
-- @
--
-- Note that the given 'Nat's **have** to be valid mnemonic sizes, otherwise the
-- underlying code won't even compile, with not-so-friendly error messages.
--
-- Also, the internal representation holds a @[Text]@ which contains the list of
-- mnemonic words that was parsed. This is only to be able to implement the
-- 'ToJSON' instances and roundtrip, which is a very dubious argument. In
-- practice, we'll NEVER peek at the mnemonic, output them and whatnot.
newtype ApiMnemonicT (sizes :: [Nat]) =
    ApiMnemonicT { getApiMnemonicT :: SomeMnemonic }
    deriving (Generic, Eq)
    deriving newtype NFData
    deriving Show via (Quiet (ApiMnemonicT sizes))

-- | A stake key belonging to the current wallet.
data ApiOurStakeKey (n :: NetworkDiscriminant) = ApiOurStakeKey
     { _index :: !Natural
    , _key :: !(ApiRewardAccount n)
    , _stake :: !(Quantity "lovelace" Natural)
      -- ^ The total ada this stake key controls / is associated with. This
      -- also includes the reward balance.
    , _rewardBalance :: !(Quantity "lovelace" Natural)
      -- ^ The current reward balance (not lifetime).
    , _delegation :: !ApiWalletDelegation
      -- ^ The delegation of this stake key
    }
    deriving (Generic, Eq, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiOurStakeKey n)

-- | A stake key found in the wallet UTxO, but which isn't ours.
--
-- We /could/ provide the current delegation status for foreign stake
-- keys.
data ApiForeignStakeKey (n :: NetworkDiscriminant) = ApiForeignStakeKey
    { _key :: !(ApiRewardAccount n)
    , _stake :: !(Quantity "lovelace" Natural)
      -- ^ The total ada this stake key controls / is associated with. This
      -- also includes the reward balance.
    , _rewardBalance :: !(Quantity "lovelace" Natural)
      -- ^ The current reward balance (not lifetime).
    }
    deriving (Generic, Eq, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiForeignStakeKey n)

-- | For describing how much stake is associated with no stake key.
newtype ApiNullStakeKey = ApiNullStakeKey
    { _stake :: Quantity "lovelace" Natural
      -- ^ The total stake of the wallet UTxO that is not associated with a
      -- stake key, because it's part of an enterprise address.
    }
    deriving (Generic, Eq)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiNullStakeKey
    deriving Show via (Quiet ApiNullStakeKey)

-- | Collection of stake keys associated with a wallet.
data ApiStakeKeys (n :: NetworkDiscriminant) = ApiStakeKeys
    { _ours :: ![ApiOurStakeKey n]
    , _foreign :: ![ApiForeignStakeKey n]
    , _none :: !ApiNullStakeKey
    }
    deriving (Generic, Eq, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiStakeKeys n)

{-------------------------------------------------------------------------------
                               JSON Instances
-------------------------------------------------------------------------------}

instance FromJSON (ApiT W.AssetURL) where
    parseJSON value = parseJSON value >>= either fail (pure . ApiT) . W.validateMetadataURL
instance ToJSON (ApiT W.AssetURL) where
    toJSON = toJSON . show . W.unAssetURL . getApiT

-- TODO: clean up duplication with TokenMetadata
instance FromJSON (ApiT W.AssetLogo) where
    parseJSON = withText "base64 bytestring" $
        either fail (pure . ApiT . W.AssetLogo) . convertFromBase Base64 . T.encodeUtf8
instance ToJSON (ApiT W.AssetLogo) where
    toJSON = toJSON . B8.unpack . convertToBase Base64 . W.unAssetLogo . getApiT

instance FromJSON (ApiT W.AssetDecimals) where
    parseJSON = parseJSON >=> (pure . ApiT . W.AssetDecimals)
instance ToJSON (ApiT W.AssetDecimals) where
    toJSON = toJSON . W.unAssetDecimals . getApiT

instance HasSNetworkId n => FromJSON (ApiSelectCoinsData n) where
    parseJSON = withObject "DelegationAction" $ \o -> do
        p <- o .:? "payments"
        a <- o .:? "delegation_action"
        case (p :: Maybe Value, a :: Maybe Value) of
            (Just _, Just _) ->
                fail "Specified both payments and action, pick one"
            (Nothing, Just{}) ->
                ApiSelectForDelegation <$> parseJSON (Object o)
            (Just{}, Nothing) ->
                ApiSelectForPayment <$> parseJSON (Object o)
            _ ->
                fail "No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"

instance HasSNetworkId n => ToJSON (ApiSelectCoinsData n) where
    toJSON (ApiSelectForPayment v) = toJSON v
    toJSON (ApiSelectForDelegation v) = toJSON v

apiDelegationActionOptions :: Aeson.Options
apiDelegationActionOptions = Aeson.defaultOptions
      { tagSingleConstructors = True
      , omitNothingFields = True
      , constructorTagModifier = camelTo2 '_'
      , fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
      , sumEncoding = TaggedObject
          { tagFieldName = "action"
          , contentsFieldName = "pool"
          }
      }

instance FromJSON ApiDelegationAction where
    parseJSON = genericParseJSON apiDelegationActionOptions
instance ToJSON ApiDelegationAction where
    toJSON = genericToJSON apiDelegationActionOptions

instance ToJSON ApiStakeKeyIndex where
    toJSON (ApiStakeKeyIndex ix) = toJSON ix
instance FromJSON ApiStakeKeyIndex where
    parseJSON val = ApiStakeKeyIndex <$> parseJSON val

instance ToJSON ApiMultiDelegationAction where
    toJSON (Joining poolId stakeKey) =
        object [ "join" .=
                   object [
                         "pool" .= toJSON poolId
                       , "stake_key_index" .= toJSON stakeKey
                       ]
               ]
    toJSON (Leaving stakeKey) =
        object [ "quit" .= object [ "stake_key_index" .= toJSON stakeKey ] ]
instance FromJSON ApiMultiDelegationAction where
    parseJSON = withObject "ApiMultiDelegationAction" $ \obj -> do
        actionJoin <- obj .:? "join"
        actionQuit <- obj .:? "quit"
        case (actionJoin, actionQuit) of
            (Just o, Nothing) ->
                Joining <$> o .: "pool" <*> o .: "stake_key_index"
            (Nothing, Just o) ->
                Leaving <$> o .: "stake_key_index"
            _ -> fail "ApiMultiDelegationAction needs either 'join' or 'quit', but not both"

deriving via DefaultSum AddressState instance FromJSON (ApiT AddressState)
deriving via DefaultSum AddressState instance ToJSON (ApiT AddressState)

instance FromJSON ApiAccountPublicKey where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON ApiAccountPublicKey where
    toJSON =
        toJSON . hexText . xpubToBytes . getApiT . key

instance FromJSON ApiAccountSharedPublicKey where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON ApiAccountSharedPublicKey where
    toJSON (ApiAccountSharedPublicKey (ApiT xpub)) =
        let hrp = [Bech32.humanReadablePart|acct_shared_xvk|]
        in String $ T.decodeUtf8 $ encode (EBech32 hrp) $ xpubToBytes xpub

instance FromJSON WalletOrAccountPostData where
    parseJSON obj = do
        passwd <-
            (withObject "postData" $
             \o -> o .:? "passphrase" :: Aeson.Parser (Maybe Text)) obj
        mnemonic <-
            (withObject "postData" $
             \o -> o .:? "mnemonic_sentence" :: Aeson.Parser (Maybe [Text])) obj
        case (passwd, mnemonic) of
            (Nothing, Nothing) -> do
                xs <- parseJSON obj :: Aeson.Parser AccountPostData
                pure $ WalletOrAccountPostData $ Right xs
            _ -> do
                xs <- parseJSON obj :: Aeson.Parser WalletPostData
                pure $ WalletOrAccountPostData $ Left xs

instance ToJSON WalletOrAccountPostData where
    toJSON (WalletOrAccountPostData (Left c))= toJSON c
    toJSON (WalletOrAccountPostData (Right c))= toJSON c

instance ToJSON SomeByronWalletPostData where
    toJSON = \case
        RandomWalletFromMnemonic w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Random)
        RandomWalletFromXPrv w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Random)
        SomeIcarusWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Icarus)
        SomeTrezorWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Trezor)
        SomeLedgerWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Ledger)
        SomeAccount w -> toJSON w
      where
        fieldName :: Text
        fieldName = "style"

instance FromJSON SomeByronWalletPostData where
    parseJSON = withObject "SomeByronWallet" $ \obj -> do
        choice <- (,) <$> obj .:? "account_public_key" <*> obj .:? "style"
        case choice of
            (Nothing, Just t) | t == toText Random ->
                (obj .:? "passphrase_hash" :: Aeson.Parser (Maybe Text)) >>= \case
                    Nothing ->
                        RandomWalletFromMnemonic <$> parseJSON (Aeson.Object obj)
                    Just _ ->
                        RandomWalletFromXPrv <$> parseJSON (Aeson.Object obj)

            (Nothing, Just t) | t == toText Icarus ->
                SomeIcarusWallet <$> parseJSON (Aeson.Object obj)

            (Nothing, Just t) | t == toText Trezor ->
                    SomeTrezorWallet <$> parseJSON (Aeson.Object obj)

            (Nothing, Just t) | t == toText Ledger ->
                    SomeLedgerWallet <$> parseJSON (Aeson.Object obj)

            (Just (_ :: ApiAccountPublicKey), _) ->
                SomeAccount <$> parseJSON (Aeson.Object obj)

            _ ->
                fail "unrecognized wallet's style."

withExtraField
    :: (Text, Value)
    -> Value
    -> Value
withExtraField (k,v) = \case
    Aeson.Object m -> Aeson.Object (Aeson.insert (Aeson.fromText k) v m)
    json -> json

instance FromJSON (ApiT PassphraseHash) where
    parseJSON = parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT PassphraseHash) where
    toJSON = toTextApiT

instance FromJSON (ApiT XPrv) where
    parseJSON = parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT XPrv) where
    toJSON = toJSON . toText

instance FromJSON (ApiT (Hash "VerificationKey")) where
    parseJSON = fromTextApiT "VerificationKey Hash"
instance ToJSON (ApiT (Hash "VerificationKey")) where
    toJSON = toTextApiT

instance FromJSON (ApiT (Hash "TokenPolicy")) where
    parseJSON = fromTextApiT "TokenPolicy Hash"
instance ToJSON (ApiT (Hash "TokenPolicy")) where
    toJSON = toTextApiT

instance FromJSON WalletPutPassphraseData where
    parseJSON  =
        fmap WalletPutPassphraseData . variants "PutPassphrase data"
            [ variant "old passphrase"
                    (Aeson.member "old_passphrase")
                    $ fmap Left <$> parseJSON
            , variant "mnemonic"
                    (Aeson.member "mnemonic_sentence")
                    $ fmap Right <$> parseJSON
            ]

instance ToJSON  WalletPutPassphraseData where
    toJSON (WalletPutPassphraseData x) = either
        (genericToJSON defaultRecordTypeOptions)
        (genericToJSON defaultRecordTypeOptions)
        x

instance FromJSON (ApiT PoolMetadataGCStatus) where
    parseJSON = withObject "PoolMetadataGCStatus" $ \o -> do
        (status' :: String) <- o .: "status"
        last_run <- o .:? "last_run"
        case (status', last_run) of
            ("restarting", Just (ApiT (Iso8601Time gctime)))
                -> pure $ ApiT (Restarting $ utcTimeToPOSIXSeconds gctime)
            ("has_run", Just (ApiT (Iso8601Time gctime)))
                -> pure $ ApiT (HasRun $ utcTimeToPOSIXSeconds gctime)
            ("restarting", Nothing)
                -> fail "missing field last_run"
            ("has_run", Nothing)
                -> fail "missing field last_run"
            ("not_applicable", _)
                -> pure $ ApiT NotApplicable
            ("not_started", _)
                -> pure $ ApiT NotStarted
            _ -> fail ("Unknown status: " <> status')

instance ToJSON (ApiT PoolMetadataGCStatus) where
    toJSON (ApiT (NotApplicable)) =
        object [ "status" .= String "not_applicable" ]
    toJSON (ApiT (NotStarted)) =
        object [ "status" .= String "not_started" ]
    toJSON (ApiT (Restarting gctime)) =
        object [ "status" .= String "restarting"
            , "last_run" .= ApiT (Iso8601Time (posixSecondsToUTCTime gctime)) ]
    toJSON (ApiT (HasRun gctime)) =
        object [ "status" .= String "has_run"
            , "last_run" .= ApiT (Iso8601Time (posixSecondsToUTCTime gctime)) ]

instance FromJSON ApiCredential where
    parseJSON v =
        (CredentialScriptHash . ScriptHash <$> parseCredential 28 ["script"] v) <|>
        (CredentialKeyHash <$> parseCredential 28 ["stake_vkh","addr_vkh"] v) <|>
        (CredentialPubKey <$> parseCredential 32 ["stake_vk","addr_vk"] v) <|>
        (CredentialExtendedPubKey <$> parseCredential 64 ["stake_xvk","addr_xvk"] v) <|>
        (CredentialScript <$> parseJSON v)

parseCredential
    :: Int
    -> [Text]
    -> Aeson.Value
    -> Aeson.Parser ByteString
parseCredential payloadLength prefixes = withText "Credential" $ \txt ->
    case detectEncoding (T.unpack txt) of
        Just EBech32{} -> do
            (hrp, dp) <- case Bech32.decodeLenient txt of
                Left _ -> fail "Credential's Bech32 has invalid text."
                Right res -> pure res
            let checkPayload bytes
                    | BS.length bytes /= payloadLength =
                          fail $ "Credential must be "
                          <> show payloadLength <> " bytes."
                    | otherwise = pure bytes
            let proceedWhenHrpCorrect = case  Bech32.dataPartToBytes dp of
                    Nothing ->
                          fail "Credential has invalid Bech32 datapart."
                    Just bytes -> checkPayload bytes
            if Bech32.humanReadablePartToText hrp `L.elem` prefixes
                then proceedWhenHrpCorrect
                else fail $ "Credential must have following prefixes: "
                    <> show prefixes
        _ -> fail "Credential must be must be encoded as Bech32."

instance ToJSON ApiCredential where
    toJSON (CredentialPubKey key') = do
        let hrp = [Bech32.humanReadablePart|addr_vk|]
        String $ T.decodeUtf8 $ encode (EBech32 hrp) key'
    toJSON (CredentialExtendedPubKey key') = do
        let hrp = [Bech32.humanReadablePart|addr_xvk|]
        String $ T.decodeUtf8 $ encode (EBech32 hrp) key'
    toJSON (CredentialKeyHash key') = do
        let hrp = [Bech32.humanReadablePart|addr_vkh|]
        String $ T.decodeUtf8 $ encode (EBech32 hrp) key'
    toJSON (CredentialScriptHash (ScriptHash script)) = do
        let hrp = [Bech32.humanReadablePart|script|]
        String $ T.decodeUtf8 $ encode (EBech32 hrp) script
    toJSON (CredentialScript s) = toJSON s

instance FromJSON ApiAddressData where
    parseJSON v =
        parseBaseAddr v <|>
        parseEnterprise v <|>
        parseRewardAccount v <|>
        fail msgError
      where
         msgError =
             "Address must have at least one valid credential. When script is\
             \ used as a credential it must have only bech32 encoded verification keys\
             \ with possible prefixes: 'stake_shared_vkh', 'stake_shared_vk', 'stake_shared_xvk', \
             \'addr_shared_vkh', 'addr_shared_vk' or 'addr_shared_xvk' and proper \
             \payload size. 'at_least' cannot exceed 255. When public key is used as a credential \
             \then bech32 encoded public keys are expected to be used with possible prefixes: \
             \'stake_xvk', 'addr_xvk', 'stake_vk' or 'addr_vk', always with proper payload size \
             \(32-byte and 64-byte payload for non-extended and extended credential, respectively). \
             \When key hash is used as a credential then bech32 encoded public keys are expected \
             \to be used with possible prefixes: 'stake_vkh' or 'addr_vkh', always with 28-byte \
             \payload size."
         parseBaseAddr = withObject "AddrBase" $ \o -> do
             addr <- AddrBase <$> o .: "payment" <*> o .: "stake"
             ApiAddressData addr <$> o .:? "validation"
         parseEnterprise = withObject "AddrEnterprise" $ \o -> do
             addr <- AddrEnterprise <$> o .: "payment"
             ApiAddressData addr <$> o .:? "validation"
         parseRewardAccount = withObject "AddrRewardAccount" $ \o -> do
             addr <- AddrRewardAccount <$> o .: "stake"
             ApiAddressData addr <$> o .:? "validation"

instance ToJSON ApiAddressData where
    toJSON (ApiAddressData (AddrEnterprise payment') validation') =
        object $ ("payment" .= payment') : addOptionally validation'
    toJSON (ApiAddressData (AddrRewardAccount stake') validation') =
        object $ ("stake" .= stake') : addOptionally validation'
    toJSON (ApiAddressData (AddrBase payment' stake') validation') =
        object $ [ "payment" .= payment', "stake" .= stake'] ++ addOptionally validation'

addOptionally :: (Aeson.KeyValue a, ToJSON v) => Maybe v -> [a]
addOptionally v = case v of
    Just v' -> ["validation" .= v']
    Nothing -> []

instance FromJSON (ApiT ValidationLevel) where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT ValidationLevel) where
    toJSON = toJSON . toText

instance FromJSON AnyAddress where
    parseJSON = parseFromText "AnyAddress" "address"

parseFromText :: FromText a => String -> Text -> Aeson.Value -> Aeson.Parser a
parseFromText typeName k = withObject typeName $ \o -> do
    v <- o .: (Aeson.fromText k)
    case fromText v of
        Right bytes -> pure bytes
        Left (TextDecodingError err) -> fail err

instance ToJSON AnyAddress where
    toJSON (AnyAddress p addrType net) =
        object [ "address" .= T.decodeUtf8 (encode (EBech32 hrp) p) ]
      where
        hrp = case Bech32.humanReadablePartFromText (prefix <> suffix) of
            Right hrp' -> hrp'
            Left e -> error $ "Bech32.humanReadablePartFromText: " <> show e
        prefix = case addrType of
                EnterpriseDelegating -> "addr"
                RewardAccount -> "stake"
        suffix = if net == mainnetId then "" else "_test"
        mainnetId = 1 :: Int

instance MkSomeMnemonic sizes => FromJSON (ApiMnemonicT sizes)
  where
    parseJSON bytes = do
        xs <- parseJSON bytes
        m <- eitherToParser $ left (ShowFmt . getMkSomeMnemonicError) $ mkSomeMnemonic @sizes xs
        return $ ApiMnemonicT m

instance ToJSON (ApiMnemonicT sizes) where
    toJSON (ApiMnemonicT (SomeMnemonic mw)) = toJSON (mnemonicToText mw)

instance FromJSON (ApiT WalletId) where
    parseJSON = fromTextApiT "WalletId"
instance ToJSON (ApiT WalletId) where
    toJSON = toTextApiT

instance FromJSON (ApiT AddressPoolGap) where
    parseJSON = parseJSON >=>
        eitherToParser . bimap ShowFmt ApiT . fromText . T.pack . show @Integer
instance ToJSON (ApiT AddressPoolGap) where
    toJSON = toJSON . getAddressPoolGap . getApiT

data ApiByronWalletBalance = ApiByronWalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiByronWalletBalance
    deriving anyclass NFData

instance FromJSON EpochNo where
    parseJSON = fmap unsafeEpochNo . parseJSON
instance ToJSON EpochNo where
    toJSON (EpochNo en) = toJSON $ fromIntegral @Word31 @Word32 en

deriving via DefaultRecord EpochInfo instance FromJSON EpochInfo
deriving via DefaultRecord EpochInfo instance ToJSON EpochInfo

instance FromJSON (ApiT StakePool) where
    parseJSON = fmap (ApiT <$>) . withObject "StakePool" $ \o -> do
        ApiT poolId <- o .: "id"
        metrics <- o .: "metrics" >>= parseJsonStakePoolMetrics
        metadata <- o .:? "metadata"
        cost <- o .: "cost"
        margin <- o .: "margin"
        pledge <- o .: "pledge"
        retirement <- o .:? "retirement"
        flags <- o .: "flags"
        pure StakePool{ id=poolId, .. }

instance ToJSON (ApiT StakePool) where
    toJSON (ApiT pool) = Aeson.object
        $ filter ((/= Null) . snd)
        [ "id" .= ApiT (view #id pool)
        , "metrics" .= toJsonStakePoolMetrics (view #metrics pool)
        , "metadata" .= view #metadata pool
        , "cost" .= view #cost pool
        , "margin" .= view #margin pool
        , "pledge" .= view #pledge pool
        , "retirement" .= view #retirement pool
        , "flags" .= view #flags pool
        ]

instance FromJSON (ApiT StakePoolMetrics) where
    parseJSON = fmap ApiT . parseJsonStakePoolMetrics

parseJsonStakePoolMetrics :: Aeson.Value -> Aeson.Parser StakePoolMetrics
parseJsonStakePoolMetrics = genericParseJSON defaultRecordTypeOptions

instance ToJSON (ApiT StakePoolMetrics) where
    toJSON = toJsonStakePoolMetrics . getApiT

toJsonStakePoolMetrics :: StakePoolMetrics -> Aeson.Value
toJsonStakePoolMetrics = genericToJSON defaultRecordTypeOptions

deriving via DefaultRecord StakePoolMetadata instance ToJSON StakePoolMetadata

deriving via DefaultSum StakePoolFlag instance FromJSON StakePoolFlag
deriving via DefaultSum StakePoolFlag instance ToJSON StakePoolFlag

instance FromJSON (ApiT WalletName) where
    parseJSON = fromTextApiT "WalletName"
instance ToJSON (ApiT WalletName) where
    toJSON = toTextApiT

deriving via DefaultRecord W.Settings instance FromJSON (ApiT W.Settings)
deriving via DefaultRecord W.Settings instance ToJSON (ApiT W.Settings)

instance FromJSON (ApiT SyncProgress) where
    parseJSON = fmap ApiT . genericParseJSON syncProgressOptions
instance ToJSON (ApiT SyncProgress) where
    toJSON = genericToJSON syncProgressOptions . getApiT

deriving via DefaultSum BoundType instance FromJSON (ApiT BoundType)
deriving via DefaultSum BoundType instance ToJSON (ApiT BoundType)

instance (HasBase base, ByteArray bs) => FromJSON (ApiBytesT base bs) where
    parseJSON = withText (show (typeRep (Proxy @base)) ++ " ByteString") $
        eitherToParser . first ShowFmt . fromText @(ApiBytesT base bs)

instance (HasBase base, ByteArrayAccess bs) => ToJSON (ApiBytesT base bs) where
    toJSON = String . toText @(ApiBytesT base bs)

instance FromJSON (ApiT SealedTx) where
    parseJSON v = do
        tx <- parseSealedTxBytes @'Base16 v <|> parseSealedTxBytes @'Base64 v
        pure $ ApiT tx

instance ToJSON (ApiT SealedTx) where
    toJSON = sealedTxBytesValue @'Base64 . getApiT

parseSealedTxBytes
    :: forall (base :: Base). HasBase base => Value -> Parser SealedTx
parseSealedTxBytes =
    (eitherToParser . first ShowFmt . sealedTxFromBytes)
    <=< (fmap getApiBytesT . parseJSON @(ApiBytesT base ByteString))

sealedTxBytesValue :: forall (base :: Base). HasBase base => SealedTx -> Value
sealedTxBytesValue = toJSON . ApiBytesT @base . view #serialisedTx

instance FromJSON ApiSerialisedTransaction where
    parseJSON = withObject "ApiSerialisedTransaction object" $ \o -> do
        txTxt <- o .: "transaction"
        (tx, enc) <- (,HexEncoded) <$> parseSealedTxBytes @'Base16 txTxt <|>
              (,Base64Encoded) <$> parseSealedTxBytes @'Base64 txTxt
        pure $ ApiSerialisedTransaction (ApiT tx) enc

instance ToJSON ApiSerialisedTransaction where
    toJSON (ApiSerialisedTransaction tx encoding) =
        object [ "transaction" .= case encoding of
                       HexEncoded ->
                           sealedTxBytesValue @'Base16 . getApiT $ tx
                       Base64Encoded ->
                           sealedTxBytesValue @'Base64 . getApiT $ tx
               ]

instance FromJSON ApiSignTransactionPostData where
    parseJSON = genericParseJSON strictRecordTypeOptions
instance ToJSON ApiSignTransactionPostData where
    toJSON = genericToJSON strictRecordTypeOptions

instance HasSNetworkId t => FromJSON (ApiPaymentDestination t) where
    parseJSON obj = parseAddrs
      where
        parseAddrs = ApiPaymentAddresses <$> parseJSON obj

instance HasSNetworkId n => ToJSON (ApiPaymentDestination n) where
    toJSON (ApiPaymentAddresses addrs) = toJSON addrs

instance HasSNetworkId n => FromJSON (ApiRedeemer n) where
    parseJSON = withObject "ApiRedeemer" $ \o -> do
        purpose <- o .: "purpose"
        bytes <- o .: "data"
        case (purpose :: Text) of
            "spending" ->
                ApiRedeemerSpending bytes <$> (o .: "input")
            "minting" ->
                ApiRedeemerMinting bytes <$> (o .: "policy_id")
            "rewarding" -> do
                text <- o .: "stake_address"
                case deserialiseFromBech32 (proxyToAsType Proxy) text of
                    Left e -> fail (show e)
                    Right addr -> pure $ ApiRedeemerRewarding bytes addr
            _ ->
                fail "unknown purpose for redeemer."
instance HasSNetworkId n => ToJSON (ApiRedeemer n) where
    toJSON = \case
        ApiRedeemerSpending bytes input -> object
            [ "purpose" .= ("spending" :: Text)
            , "data" .= bytes
            , "input" .= input
            ]
        ApiRedeemerMinting bytes policy -> object
            [ "purpose" .= ("minting" :: Text)
            , "data" .= bytes
            , "policy_id" .= policy
            ]
        ApiRedeemerRewarding bytes addr -> object
            [ "purpose" .= ("rewarding" :: Text)
            , "data" .= bytes
            , "stake_address" .= serialiseToBech32 addr
            ]

instance ToJSON ApiValidityBound where
    toJSON ApiValidityBoundUnspecified = Aeson.Null
    toJSON (ApiValidityBoundAsTimeFromNow from) = toJSON from
    toJSON (ApiValidityBoundAsSlot sl) = toJSON sl
instance FromJSON ApiValidityBound where
    parseJSON obj = processNull <|> processObject obj
      where
        processNull =
            if obj == Aeson.Null then
                pure ApiValidityBoundUnspecified
            else
                fail "invalid string of ApiValidityBound"
        processObject = withObject "ApiValidityBound object" $ \o -> do
            unit <- o .:? "unit"
            case unit of
                Just (String unitType) -> case unitType of
                    "second" -> ApiValidityBoundAsTimeFromNow <$> parseJSON obj
                    "slot" -> ApiValidityBoundAsSlot <$> parseJSON obj
                    _ -> fail "ApiValidityBound string must have either 'second' or 'slot' unit."
                _ -> fail "ApiValidityBound string must have 'unit' field."

instance HasSNetworkId n => FromJSON (ApiConstructTransaction n) where
    parseJSON = withObject "ApiConstructTransaction object" $ \o -> do
        txTxt <- o .: "transaction"
        (tx, enc) <- (,HexEncoded) <$> parseSealedTxBytes @'Base16 txTxt <|>
            (,Base64Encoded) <$> parseSealedTxBytes @'Base64 txTxt
        sel <- o .: "coin_selection"
        fee <- o .: "fee"
        pure $ ApiConstructTransaction (ApiSerialisedTransaction (ApiT tx) enc) sel fee

instance HasSNetworkId n => ToJSON (ApiConstructTransaction n) where
    toJSON (ApiConstructTransaction (ApiSerialisedTransaction tx encoding) sel fee) =
        object [ "transaction" .= case encoding of
                       HexEncoded ->
                           sealedTxBytesValue @'Base16 . getApiT $ tx
                       Base64Encoded ->
                           sealedTxBytesValue @'Base64 . getApiT $ tx
               , "coin_selection" .= toJSON sel
               , "fee" .= toJSON fee
               ]

instance FromJSON ApiSelfWithdrawalPostData where
    parseJSON obj = do
        str <- parseJSON obj
        SelfWithdraw <$ guard (str == ("self" :: String))

instance ToJSON ApiSelfWithdrawalPostData where
    toJSON SelfWithdraw = toJSON ("self" :: String)

instance FromJSON ApiWithdrawalPostData where
    parseJSON obj =
        parseSelfWithdrawal <|> fmap ExternalWithdrawal (parseJSON obj)
      where
        parseSelfWithdrawal = do
            str <- parseJSON obj
            SelfWithdrawal <$ guard (str == ("self" :: String))
instance ToJSON ApiWithdrawalPostData where
    toJSON = \case
        SelfWithdrawal -> toJSON ("self" :: String)
        ExternalWithdrawal mw -> toJSON mw

-- Note: These custom JSON instances are for compatibility with the existing API
-- schema. At some point, we can switch to the generic instances.
instance FromJSON ApiSlotReference where
    parseJSON = withObject "SlotReference" $ \o ->
        ApiSlotReference
        <$> o .: "absolute_slot_number"
        <*> parseJSON (Aeson.Object o)
        <*> o .: "time"
instance ToJSON ApiSlotReference where
    toJSON (ApiSlotReference sln sli t) =
        case toJSON sli of
            Aeson.Object rest ->
                Aeson.Object $
                    "absolute_slot_number" .= sln <> "time" .= t <> rest
            _ -> error "ApiSlotId isn't an object."

-- Note: These custom JSON instances are for compatibility with the existing API
-- schema. At some point, we can switch to the generic instances.
-- A BlockReference is just a SlotReference with the block height included.
instance FromJSON ApiBlockReference where
    parseJSON v = do
        ApiSlotReference sln sli t <- parseJSON v
        ApiBlockReference sln sli t <$> parseJSON v
instance ToJSON ApiBlockReference where
    toJSON (ApiBlockReference sln sli t (ApiBlockInfo bh)) =
        case toJSON (ApiSlotReference sln sli t) of
            Aeson.Object rest -> Aeson.Object ("height" .= bh <> rest)
            _ -> error "ApiSlotReference isn't an object."

instance HasSNetworkId n => FromJSON (ApiTxInput n) where
    parseJSON v = ApiTxInput <$> optional (parseJSON v) <*> parseJSON v

instance HasSNetworkId n => ToJSON (ApiTxInput n) where
    toJSON (ApiTxInput s i) =
        Object (maybe mempty (fromValue . toJSON) s <> fromValue (toJSON i))
      where
        fromValue (Object o) = o
        fromValue _ = mempty

instance HasSNetworkId n => FromJSON (ApiTxCollateral n) where
    parseJSON v = ApiTxCollateral <$> optional (parseJSON v) <*> parseJSON v

instance HasSNetworkId n => ToJSON (ApiTxCollateral n) where
    toJSON (ApiTxCollateral s i) =
        Object (maybe mempty (fromValue . toJSON) s <> fromValue (toJSON i))
      where
        fromValue (Object o) = o
        fromValue _ = mempty

instance FromJSON (ApiT (Hash "Datum")) where
    parseJSON = fromTextApiT "Datum Hash"
instance ToJSON (ApiT (Hash "Datum")) where
    toJSON = toTextApiT

deriving via DefaultSum Direction instance FromJSON (ApiT Direction)
deriving via DefaultSum Direction instance ToJSON (ApiT Direction)

deriving via DefaultSum TxStatus instance FromJSON (ApiT TxStatus)
deriving via DefaultSum TxStatus instance ToJSON (ApiT TxStatus)

instance FromJSON NtpSyncingStatus where
    parseJSON = parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON NtpSyncingStatus where
    toJSON = toJSON . toText

deriving via DefaultRecord NtpStatusWithOffset
    instance FromJSON NtpStatusWithOffset
deriving via DefaultRecord NtpStatusWithOffset
    instance ToJSON NtpStatusWithOffset

deriving newtype instance FromJSON ApiNetworkClock
deriving newtype instance ToJSON ApiNetworkClock

deriving via DefaultRecord StakePoolMetadata
    instance FromJSON (ApiT StakePoolMetadata)
deriving via DefaultRecord StakePoolMetadata
    instance ToJSON (ApiT StakePoolMetadata)

instance FromJSON (ApiT StartTime) where
    parseJSON = fmap (ApiT . StartTime) . parseJSON
instance ToJSON (ApiT StartTime) where
    toJSON (ApiT (StartTime sn)) = toJSON sn

instance FromJSON (ApiT SlotLength) where
    parseJSON = fmap (ApiT . SlotLength) . parseJSON
instance ToJSON (ApiT SlotLength) where
    toJSON (ApiT (SlotLength sn)) = toJSON sn

instance FromJSON (ApiT EpochLength) where
    parseJSON = fmap (ApiT . EpochLength) . parseJSON
instance ToJSON (ApiT EpochLength) where
    toJSON (ApiT (EpochLength sn)) = toJSON sn

instance FromJSON (ApiT ActiveSlotCoefficient) where
    parseJSON = fmap (ApiT . ActiveSlotCoefficient) . parseJSON
instance ToJSON (ApiT ActiveSlotCoefficient) where
    toJSON (ApiT (ActiveSlotCoefficient sn)) = toJSON sn

instance FromJSON (ApiT (Hash "Genesis")) where
    parseJSON = fromTextApiT "Genesis Hash"
instance ToJSON (ApiT (Hash "Genesis")) where
    toJSON = toTextApiT

instance FromJSON ApiEraInfo where
    parseJSON = genericParseJSON explicitNothingRecordTypeOptions
instance ToJSON ApiEraInfo where
    toJSON = genericToJSON explicitNothingRecordTypeOptions

instance ToJSON XPubOrSelf where
    toJSON (SomeAccountKey xpub) =
        let hrp = [Bech32.humanReadablePart|acct_shared_xvk|]
        in String $ T.decodeUtf8 $ encode (EBech32 hrp) $ xpubToBytes xpub
    toJSON Self = "self"

instance FromJSON XPubOrSelf where
    parseJSON t = parseXPub t <|> parseSelf t
      where
        parseXPub = withText "XPub" $ \txt ->
            case fromText @ApiAccountSharedPublicKey txt of
                Left (TextDecodingError err) -> fail err
                Right (ApiAccountSharedPublicKey (ApiT xpub)) ->
                    pure $ SomeAccountKey xpub
        parseSelf = withText "Self" $ \txt ->
            if txt == "self" then
                pure Self
            else
                fail "'self' is expected."

instance FromJSON ApiScriptTemplateEntry where
    parseJSON = withObject "ApiScriptTemplateEntry" $ \o -> do
        template' <- parseJSON <$> o .: "template"
        cosigners' <- parseCosignerPairs <$> o .: "cosigners"
        ApiScriptTemplateEntry <$> (Map.fromList <$> cosigners') <*> template'
      where
        parseCosignerPairs = withObject "Cosigner pairs" $ \o ->
            case Aeson.toList o of
                [] -> fail "Cosigners object array should not be empty"
                cs -> for (reverse cs) $ \(numTxt, str) -> do
                    cosigner' <- parseJSON @Cosigner $
                        String $ Aeson.toText numTxt
                    xpubOrSelf <- parseJSON str
                    pure (cosigner', xpubOrSelf)

instance ToJSON ApiScriptTemplateEntry where
    toJSON (ApiScriptTemplateEntry cosigners' template') =
        object [ "cosigners" .= object (fmap toPair (Map.toList cosigners'))
               , "template" .= toJSON template']
      where
        cosignerToKey (Cosigner ix) =
            Aeson.fromText $ "cosigner#"<> T.pack (show ix)
        toPair (cosigner', xpubOrSelf) =
            ( cosignerToKey cosigner'
            , toJSON xpubOrSelf
            )

instance ToJSON ApiScriptTemplate where
    toJSON (ApiScriptTemplate (CA.ScriptTemplate cosigners' template')) =
        object [ "cosigners" .= object (fmap toPair (Map.toList cosigners'))
               , "template" .= toJSON template' ]
      where
        cosignerToKey (Cosigner ix) =
            Aeson.fromText $ "cosigner#"<> T.pack (show ix)
        hrp = [Bech32.humanReadablePart|acct_shared_xvk|]
        toPair (cosigner', xpub) =
            ( cosignerToKey cosigner'
            , String $ T.decodeUtf8 $ encode (EBech32 hrp) $ xpubToBytes xpub
            )

instance FromJSON ApiScriptTemplate where
    parseJSON = withObject "ApiScriptTemplate" $ \o -> do
        template' <- parseJSON <$> o .: "template"
        cosigners' <- parseCosignerPairs <$> o .: "cosigners"
        scriptTemplate <- CA.ScriptTemplate
            <$> (Map.fromList <$> cosigners')
            <*> template'
        pure $ ApiScriptTemplate scriptTemplate
      where
        parseXPub = withText "XPub" $ \txt ->
            case fromText @ApiAccountSharedPublicKey txt of
                Left (TextDecodingError err) -> fail err
                Right (ApiAccountSharedPublicKey (ApiT xpub)) -> pure xpub
        parseCosignerPairs = withObject "Cosigner pairs" $ \o ->
            case Aeson.toList o of
                [] -> fail "Cosigners object array should not be empty"
                cs -> for (reverse cs) $ \(numTxt, str) -> do
                    cosigner' <- parseJSON @Cosigner $
                        String $ Aeson.toText numTxt
                    xpub <- parseXPub str
                    pure (cosigner', xpub)

instance FromJSON ApiSharedWalletPostData where
    parseJSON obj = do
        mnemonic <-
            (withObject "postData" $
             \o -> o .:? "mnemonic_sentence" :: Aeson.Parser (Maybe [Text])) obj
        case mnemonic of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser ApiSharedWalletPostDataFromAccountPubX
                pure $ ApiSharedWalletPostData $ Right xs
            _ -> do
                xs <- parseJSON obj :: Aeson.Parser ApiSharedWalletPostDataFromMnemonics
                pure $ ApiSharedWalletPostData $ Left xs

instance ToJSON ApiSharedWalletPostData where
    toJSON (ApiSharedWalletPostData (Left c))= toJSON c
    toJSON (ApiSharedWalletPostData (Right c))= toJSON c

instance FromJSON (ApiT Cosigner) where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT Cosigner) where
    toJSON = toJSON . toText

instance FromJSON ApiSharedWalletPatchData where
    parseJSON = withObject "ApiSharedWalletPatchData" $ \o ->
        case Aeson.toList o of
            [] -> fail "ApiSharedWalletPatchData should not be empty"
            [(numTxt, str)] -> do
                cosigner' <- parseJSON @(ApiT Cosigner)
                    (String $ Aeson.toText numTxt)
                xpub <- parseJSON @ApiAccountSharedPublicKey str
                pure $ ApiSharedWalletPatchData cosigner' xpub
            _ -> fail "ApiSharedWalletPatchData should have one pair"

instance ToJSON ApiSharedWalletPatchData where
    toJSON (ApiSharedWalletPatchData cosigner accXPub) =
        object [ Aeson.fromText (toText cosigner) .= toJSON accXPub ]

instance FromJSON ApiIncompleteSharedWallet where
    parseJSON val = case val of
        Aeson.Object obj -> do
            let obj' = Aeson.delete (Aeson.fromText "state") obj
            genericParseJSON defaultRecordTypeOptions (Aeson.Object obj')
        _ -> fail "ApiIncompleteSharedWallet should be object"

instance ToJSON ApiIncompleteSharedWallet where
    toJSON wal = Aeson.Object $ Aeson.insert
        (Aeson.fromText "state")
        (object ["status" .= String "incomplete"])
        ( case genericToJSON defaultRecordTypeOptions wal of
            Aeson.Object obj -> obj
            _ -> error "ApiIncompleteSharedWallet should be object"
        )

instance FromJSON ApiSharedWallet where
    parseJSON obj = do
        balance <-
            (withObject "ActiveSharedWallet" $
             \o -> o .:? "balance" :: Aeson.Parser (Maybe ApiWalletBalance)) obj
        case balance of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser ApiIncompleteSharedWallet
                pure $ ApiSharedWallet $ Left xs
            _ -> do
                xs <- parseJSON obj :: Aeson.Parser ApiActiveSharedWallet
                pure $ ApiSharedWallet $ Right xs

instance ToJSON ApiSharedWallet where
    toJSON (ApiSharedWallet (Left c))= toJSON c
    toJSON (ApiSharedWallet (Right c))= toJSON c

-- | Options for encoding synchronization progress. It can be serialized to
-- and from JSON as follows:
--
-- >>> Aeson.encode Ready
-- {"status":"ready"}
--
-- >>> Aeson.encode $ Restoring (Quantity 14)
-- {"status":"restoring","progress":{"quantity":14,"unit":"percent"}}
syncProgressOptions :: Aeson.Options
syncProgressOptions = taggedSumTypeOptions defaultSumTypeOptions $
    TaggedObjectOptions
        { _tagFieldName = "status"
        , _contentsFieldName = "progress"
        }

{-------------------------------------------------------------------------------
                             JSON Instances: Byron
-------------------------------------------------------------------------------}

instance
  ( Enum (Index derivation level)
  , Bounded (Index derivation level)
  ) => FromJSON (ApiT (Index derivation level)) where
    parseJSON bytes = do
        n <- parseJSON @Int bytes
        eitherToParser . bimap ShowFmt ApiT . fromText . T.pack $ show n

instance
  ( Enum (Index derivation level)
  ) => ToJSON (ApiT (Index derivation level)) where
    toJSON = toJSON . fromEnum . getApiT

instance FromJSON ApiWalletDiscovery where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 1 . dropWhile (/= '_') . camelTo2 '_' }

instance ToJSON ApiWalletDiscovery where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 1 . dropWhile (/= '_') . camelTo2 '_' }

instance ToJSON ApiAddressInspect where
    toJSON = unApiAddressInspect

instance FromJSON ApiAddressInspect where
    parseJSON = pure . ApiAddressInspect

{-------------------------------------------------------------------------------
                             FromText/ToText instances
-------------------------------------------------------------------------------}

instance (HasBase b, ByteArray bs) => FromText (ApiBytesT b bs) where
    fromText = fmap ApiBytesT . fromTextBytes (baseFor @b)

instance (HasBase b, ByteArrayAccess bs) => ToText (ApiBytesT b bs) where
    toText = toTextBytes (baseFor @b) . getApiBytesT

class Typeable a => HasBase a where
    baseFor :: Base
instance HasBase 'Base16 where
    baseFor = Base16
instance HasBase 'Base64 where
    baseFor = Base64

fromTextBytes :: ByteArray bs => Base -> Text -> Either TextDecodingError bs
fromTextBytes base = first (const errMsg) . convertFromBase base . T.encodeUtf8
  where
    errMsg = TextDecodingError $ mconcat
        [ "Parse error. Expecting ", show base, "-encoded format." ]

toTextBytes :: ByteArrayAccess bs => Base -> bs -> Text
toTextBytes base = T.decodeLatin1 . convertToBase base

instance FromText (AddressAmount Text) where
    fromText text = do
        let err = Left . TextDecodingError $ "Parse error. Expecting format \
            \\"<amount>@<address>\" but got " <> show text
        case split (=='@') text of
            [] -> err
            [_] -> err
            [l, r] -> AddressAmount r <$> fromText l <*> pure mempty
            _ -> err

instance FromText AnyAddress where
    fromText txt = case detectEncoding (T.unpack txt) of
        Just EBech32{} -> do
            (hrp, dp) <- either
                (const $ Left $ TextDecodingError "AnyAddress's Bech32 has invalid text.")
                Right (Bech32.decodeLenient txt)
            let err1 = TextDecodingError "AnyAddress has invalid Bech32 datapart."
            let proceedWhenHrpCorrect ctr net = do
                    bytes <- maybeToRight err1 (Bech32.dataPartToBytes dp)
                    Right $ AnyAddress bytes ctr net
            case Bech32.humanReadablePartToText hrp of
                "addr" -> proceedWhenHrpCorrect EnterpriseDelegating 1
                "addr_test" -> proceedWhenHrpCorrect EnterpriseDelegating 0
                "stake" -> proceedWhenHrpCorrect RewardAccount 1
                "stake_test" -> proceedWhenHrpCorrect RewardAccount 0
                _ -> Left $ TextDecodingError "AnyAddress is not correctly prefixed."
        _ -> Left $ TextDecodingError "AnyAddress must be must be encoded as Bech32."

instance ToText (ApiT Cosigner) where
    toText (ApiT (Cosigner ix)) = "cosigner#"<> T.pack (show ix)

instance FromText (ApiT Cosigner) where
    fromText txt = case T.splitOn "cosigner#" txt of
        ["",numTxt] ->  case T.decimal @Integer numTxt of
            Right (num,"") -> do
                when (num < 0 || num > 255) $
                        Left $ TextDecodingError "Cosigner number should be between '0' and '255'"
                pure $ ApiT $ Cosigner $ fromIntegral num
            _ -> Left $ TextDecodingError "Cosigner should be enumerated with number"
        _ -> Left $ TextDecodingError "Cosigner should be of form: cosigner#num"

{-------------------------------------------------------------------------------
                             HTTPApiData instances
-------------------------------------------------------------------------------}

instance MimeUnrender OctetStream (ApiBytesT base ByteString) where
    mimeUnrender _ = pure . ApiBytesT . BL.toStrict
instance MimeRender OctetStream (ApiBytesT base ByteString) where
   mimeRender _ = BL.fromStrict . getApiBytesT

instance MimeUnrender OctetStream (ApiBytesT base SerialisedTx) where
    mimeUnrender _ = pure . ApiBytesT . SerialisedTx . BL.toStrict
instance MimeRender OctetStream (ApiBytesT base SerialisedTx) where
   mimeRender _ = BL.fromStrict . view #payload . getApiBytesT

instance MimeUnrender OctetStream (ApiT SealedTx) where
    mimeUnrender _ = bimap show ApiT . sealedTxFromBytes . BL.toStrict
instance MimeRender OctetStream (ApiT SealedTx) where
   mimeRender _ = BL.fromStrict . view #serialisedTx . getApiT

instance {-# OVERLAPPABLE #-} FromText a => FromHttpApiData (ApiT a) where
    parseUrlPiece = bimap pretty ApiT . fromText
instance ToText a => ToHttpApiData (ApiT a) where
    toUrlPiece = toText . getApiT

instance MimeRender OctetStream ApiSerialisedTransaction where
   mimeRender ct = mimeRender ct . view #serialisedTxSealed

instance FromHttpApiData ApiTxId where
    parseUrlPiece txt = case fromText txt of
        Left (TextDecodingError err) -> Left $ T.pack err
        Right tid -> Right $ ApiTxId $ ApiT tid
instance ToHttpApiData ApiTxId where
    toUrlPiece (ApiTxId (ApiT tid)) = toText tid

instance {-# OVERLAPPING #-} FromHttpApiData (ApiT PoolId) where
    parseUrlPiece t = ApiT <$> case fromText t of
        Left _ -> left (T.pack . show . ShowFmt) $ decodePoolIdBech32 t
        Right r -> Right r
instance {-# OVERLAPPING #-} ToHttpApiData (ApiT PoolId) where
    toUrlPiece = encodePoolIdBech32 . getApiT

instance FromHttpApiData ApiAddressInspectData where
    parseUrlPiece = pure . ApiAddressInspectData
instance ToHttpApiData ApiAddressInspectData where
    toUrlPiece = unApiAddressInspectData

-- NOTE:
-- The type families below are useful to allow building more flexible API
-- implementation from the definition above. In particular, the API client we
-- use for the command-line doesn't really _care much_ about how addresses are
-- serialized / deserialized. So, we use a poly-kinded type family here to allow
-- defining custom types in the API client with a minimal overhead and, without
-- having to actually rewrite any of the API definition.
--
-- We use an open type family so it can be extended by other module in places.
type family ApiAddressT (n :: k) :: Type
type family ApiStakeKeysT (n :: k) :: Type
type family ApiAddressIdT (n :: k) :: Type
type family ApiCoinSelectionT (n :: k) :: Type
type family ApiSelectCoinsDataT (n :: k) :: Type
type family ApiTransactionT (n :: k) :: Type
type family ApiConstructTransactionT (n :: k) :: Type
type family ApiConstructTransactionDataT (n :: k) :: Type
type family PostTransactionOldDataT (n :: k) :: Type
type family PostTransactionFeeOldDataT (n :: k) :: Type
type family ApiWalletMigrationPlanPostDataT (n :: k) :: Type
type family ApiWalletMigrationPostDataT (n :: k1) (s :: k2) :: Type
type family ApiPutAddressesDataT (n :: k) :: Type
type family ApiBalanceTransactionPostDataT (n :: k) :: Type
type family ApiDecodedTransactionT (n :: k) :: Type

type instance ApiAddressT (n :: NetworkDiscriminant) =
    ApiAddressWithPath n

type instance ApiStakeKeysT (n :: NetworkDiscriminant) =
    ApiStakeKeys n

type instance ApiPutAddressesDataT (n :: NetworkDiscriminant) =
    ApiPutAddressesData n

type instance ApiAddressIdT (n :: NetworkDiscriminant) =
    (ApiAddress n)

type instance ApiCoinSelectionT (n :: NetworkDiscriminant) =
    ApiCoinSelection n

type instance ApiSelectCoinsDataT (n :: NetworkDiscriminant) =
    ApiSelectCoinsData n

type instance ApiTransactionT (n :: NetworkDiscriminant) =
    ApiTransaction n

type instance ApiConstructTransactionT (n :: NetworkDiscriminant) =
    ApiConstructTransaction n

type instance ApiConstructTransactionDataT (n :: NetworkDiscriminant) =
    ApiConstructTransactionData n

type instance PostTransactionOldDataT (n :: NetworkDiscriminant) =
    PostTransactionOldData n
type instance PostTransactionFeeOldDataT (n :: NetworkDiscriminant) =
    PostTransactionFeeOldData n

type instance ApiWalletMigrationPlanPostDataT (n :: NetworkDiscriminant) =
    ApiWalletMigrationPlanPostData n

type instance ApiWalletMigrationPostDataT
    (n :: NetworkDiscriminant) (s :: Symbol) = ApiWalletMigrationPostData n s

type instance ApiBalanceTransactionPostDataT (n :: NetworkDiscriminant) =
    ApiBalanceTransactionPostData n

type instance ApiDecodedTransactionT (n :: NetworkDiscriminant) =
    ApiDecodedTransaction n

{-------------------------------------------------------------------------------
                         SMASH types
-------------------------------------------------------------------------------}

instance ToJSON SMASHPoolId where
    toJSON = genericToJSON defaultRecordTypeOptions
        { fieldLabelModifier = Prelude.id }

instance ToJSON HealthStatusSMASH where
    toJSON = genericToJSON defaultRecordTypeOptions

newtype ApiHealthCheck = ApiHealthCheck { health :: HealthCheckSMASH }
    deriving (Generic, Eq, Ord)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiHealthCheck
    deriving Show via (Quiet ApiHealthCheck)

instance FromJSON HealthCheckSMASH where
    parseJSON = genericParseJSON defaultSumTypeOptions
        { sumEncoding = UntaggedValue }
instance ToJSON HealthCheckSMASH where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON (ApiT SmashServer) where
    parseJSON = fromTextApiT "SmashServer"
instance ToJSON (ApiT SmashServer) where
    toJSON = toTextApiT

{-------------------------------------------------------------------------------
                         Token minting types
-------------------------------------------------------------------------------}

-- | Core minting and burning request information.
--
-- Assets are minted and burned under a "policy". The policy defines under what
-- circumstances a token may be minted and burned. The policy is the hash of a
-- serialized script that contains verification keys and timelocks combined in
-- conditions, possibly nested, to accommodate non-trivial time conditions.
-- In the non-multisig case the script regulating minting/burning will
-- contain a verification key via cosigner#0 of the wallet with optional
-- time predicates.
-- In the multisig case the script regulating minting/burning will contain
-- verification keys of signers (via cosigner#N) with optional time predicates.
-- The used key derivation index is the same for all engaged derivation keys and
-- ix=0 is assumed to be used. The verification key derivation is performed
-- according to CIP 1855.
data ApiMintBurnData (n :: NetworkDiscriminant) = ApiMintBurnData
    { policyScriptTemplate
        :: !(ApiT (Script Cosigner))
        -- ^ A script regulating minting/burning policy. 'self' is expected
        -- in place of verification key.
    , assetName
        :: !(Maybe (ApiT W.TokenName))
        -- ^ The name of the asset to mint/burn.
    , operation
        :: !(ApiMintBurnOperation n)
        -- ^ The minting or burning operation to perform.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiMintBurnData n)
    deriving anyclass NFData

-- | A user may choose to either mint tokens or burn tokens with each operation.
data ApiMintBurnOperation (n :: NetworkDiscriminant)
    = ApiMint (ApiMintData n)
    -- ^ Mint tokens.
    | ApiBurn ApiBurnData
    -- ^ Burn tokens.
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | The format of a minting request: mint "amount" and send it to the
-- "address".
data ApiMintData (n :: NetworkDiscriminant) = ApiMintData
    { receivingAddress
        :: Maybe (ApiAddress n)
        -- ^ An optional address to which minted assets should be paid.
        --
        -- If no address is specified, then minted assets will be returned to
        -- the wallet as change, and change output addresses will be assigned
        -- automatically.
    , quantity
        :: Natural
        -- ^ Amount of assets to mint.
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord (ApiMintData n)
    deriving anyclass NFData

-- | The format of a burn request: burn "amount". The user can only specify the
-- type of tokens to burn (policyId, assetName), and the amount, the exact
-- tokens selected are up to the implementation.
newtype ApiBurnData = ApiBurnData
    { quantity :: Natural
    }
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiBurnData
    deriving anyclass NFData

instance HasSNetworkId n => ToJSON (ApiMintBurnOperation n) where
    toJSON = object . pure . \case
        ApiMint mint -> "mint" .= mint
        ApiBurn burn -> "burn" .= burn

instance HasSNetworkId n => FromJSON (ApiMintBurnOperation n) where
    parseJSON = Aeson.withObject "ApiMintBurnOperation" $ \o ->
        case Aeson.keys o of
            ["mint"] -> ApiMint <$> o .: "mint"
            ["burn"] -> ApiBurn <$> o .: "burn"
            [] -> fail "Must include a \"mint\" or \"burn\" property."
            _ -> fail "May be either a \"mint\" or a \"burn\"."

instance FromJSON (ApiT (Script KeyHash)) where
    parseJSON = fmap ApiT . parseJSON
instance ToJSON (ApiT (Script KeyHash)) where
    toJSON = toJSON . getApiT

instance FromJSON (ApiT (Script Cosigner)) where
    parseJSON = fmap ApiT . parseJSON
instance ToJSON (ApiT (Script Cosigner)) where
    toJSON = toJSON . getApiT
