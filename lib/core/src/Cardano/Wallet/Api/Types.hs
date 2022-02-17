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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

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
-- <https://github.com/input-output-hk/cardano-wallet/blob/master/specifications/api/swagger.yaml Wallet API Specification>

module Cardano.Wallet.Api.Types
    (
    -- * Wallet Styles
      WalletStyle (..)
    , ByronWalletStyle (..)
    , StyleSymbol
    , AllowedMnemonics
    , fmtAllowedWords

    -- * API Types
    , ApiAsset (..)
    , toApiAsset
    , ApiAssetMetadata (..)
    , toApiAssetMetadata
    , ApiAddress (..)
    , ApiCredential (..)
    , ApiAddressData (..)
    , ApiAddressDataPayload (..)
    , AnyAddress (..)
    , AnyAddressType (..)
    , ApiCertificate (..)
    , ApiDelegationAction (..)
    , ApiEpochInfo (..)
    , toApiEpochInfo
    , ApiSelectCoinsData (..)
    , ApiSelectCoinsPayments (..)
    , ApiSelectCoinsAction (..)
    , ApiMintBurnOperation (..)
    , ApiMintData(..)
    , ApiBurnData(..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionChange (..)
    , ApiCoinSelectionCollateral (..)
    , ApiCoinSelectionOutput (..)
    , ApiCoinSelectionWithdrawal (..)
    , ApiBase64
    , ApiMintBurnData (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiStakePoolFlag (..)
    , ApiWallet (..)
    , ApiWalletBalance (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiWalletUtxoSnapshot (..)
    , ApiWalletUtxoSnapshotEntry (..)
    , ApiUtxoStatistics (..)
    , toApiUtxoStatistics
    , WalletPostData (..)
    , WalletPutData (..)
    , SettingsPutData (..)
    , WalletPutPassphraseData (..)
    , ApiSignTransactionPostData (..)
    , PostTransactionOldData (..)
    , PostTransactionFeeOldData (..)
    , ApiSerialisedTransaction (..)
    , ApiTransaction (..)
    , ApiMintedBurnedTransaction (..)
    , ApiMintedBurnedInfo (..)
    , ApiWithdrawalPostData (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , MaintenanceAction (..)
    , ApiFee (..)
    , ApiTxCollateral (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxMetadata (..)
    , AddressAmount (..)
    , AddressAmountNoAssets (..)
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiErrorCode (..)
    , ApiNetworkInformation (..)
    , ApiEra (..)
    , ApiNtpStatus (..)
    , NtpSyncingStatus (..)
    , ApiNetworkClock (..)
    , ApiSlotReference (..)
    , ApiSlotId (..)
    , ApiBlockReference (..)
    , ApiBlockInfo (..)
    , ApiStakeKeys (..)
    , ApiOurStakeKey (..)
    , ApiForeignStakeKey (..)
    , ApiNullStakeKey (..)
    , Iso8601Time (..)
    , MinWithdrawal (..)
    , ApiNetworkParameters (..)
    , toApiNetworkParameters
    , ApiEraInfo (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletDelegationNext (..)
    , ApiPoolId (..)
    , ApiWalletMigrationPlanPostData (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletMigrationBalance (..)
    , ApiWalletMigrationPlan (..)
    , ApiWithdrawal (..)
    , ApiWalletSignData (..)
    , ApiVerificationKeyShelley (..)
    , ApiVerificationKeyShared (..)
    , ApiScriptTemplateEntry (..)
    , XPubOrSelf (..)
    , VerificationKeyHashing (..)
    , ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , KeyFormat (..)
    , ApiPostAccountKeyData (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiConstructTransaction (..)
    , ApiConstructTransactionData (..)
    , ApiMultiDelegationAction (..)
    , ApiStakeKeyIndex (..)
    , ApiPaymentDestination (..)
    , ApiValidityInterval (..)
    , ApiValidityBound (..)
    , PostMintBurnAssetData(..)
    , ApiBalanceTransactionPostData (..)
    , ApiExternalInput (..)
    , ApiRedeemer (..)
    , ApiDecodedTransaction (..)
    , ApiWalletInput (..)
    , ApiTxInputGeneral (..)
    , ResourceContext (..)
    , ApiWithdrawalGeneral (..)
    , ApiWalletOutput (..)
    , ApiTxOutputGeneral (..)
    , ApiAnyCertificate (..)
    , ApiExternalCertificate (..)
    , ApiRegisterPool (..)
    , ApiDeregisterPool (..)

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
    , WalletOrAccountPostData (..)

    -- * User-Facing Address Encoding/Decoding
    , EncodeAddress (..)
    , DecodeAddress (..)
    , EncodeStakeAddress (..)
    , DecodeStakeAddress (..)

    -- * Shared Wallets
    , ApiSharedWallet (..)
    , ApiPendingSharedWallet (..)
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
    , ApiAddressT
    , ApiStakeKeysT
    , ApiPutAddressesDataT
    , ApiAddressIdT
    , ApiCoinSelectionT
    , ApiSelectCoinsDataT
    , ApiTransactionT
    , ApiConstructTransactionT
    , ApiConstructTransactionDataT
    , PostTransactionOldDataT
    , PostTransactionFeeOldDataT
    , ApiMintedBurnedTransactionT
    , ApiWalletMigrationPlanPostDataT
    , ApiWalletMigrationPostDataT
    , PostMintBurnAssetDataT
    , ApiBalanceTransactionPostDataT
    , ApiDecodedTransactionT

    -- * API Type Conversions
    , coinToQuantity
    , coinFromQuantity

    -- * Others
    , defaultRecordTypeOptions
    , strictRecordTypeOptions
    , HealthStatusSMASH (..)
    , HealthCheckSMASH (..)
    , ApiHealthCheck (..)

    -- * Re-exports
    , Base (Base16, Base64)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xpubFromBytes, xpubToBytes )
import Cardano.Address.Script
    ( Cosigner (..), KeyHash, Script, ScriptTemplate, ValidationLevel (..) )
import Cardano.Api
    ( StakeAddress
    , TxMetadataJsonSchema (..)
    , deserialiseFromBech32
    , displayError
    , metadataFromJson
    , metadataToJson
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
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , Role (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( purposeCIP1854 )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, SeqState, getAddressPoolGap, purposeCIP1852 )
import Cardano.Wallet.Primitive.Slotting
    ( Qry, timeOfEpoch )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , EpochNo (..)
    , ExecutionUnitPrices (..)
    , GenesisParameters (..)
    , MinimumUTxOValue (..)
    , NetworkParameters (..)
    , NonWalletCertificate (..)
    , PoolId (..)
    , PoolMetadataGCStatus (..)
    , SlotInEpoch (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , SmashServer (..)
    , StakePoolMetadata
    , StartTime (..)
    , WalletId (..)
    , WalletName (..)
    , decodePoolIdBech32
    , encodePoolIdBech32
    , unsafeEpochNo
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , SerialisedTx (..)
    , TxConstraints (..)
    , TxIn (..)
    , TxMetadata
    , TxScriptValidity (..)
    , TxStatus (..)
    , coinIsValidForTxOut
    , sealedTxFromBytes
    , txMetadataIsNull
    , txOutMaxCoin
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( BoundType, HistogramBar (..), UTxOStatistics (..) )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataError (..) )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import "cardano-addresses" Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, encode, fromBase16 )
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
    , Value (Object, String)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , object
    , omitNothingFields
    , prependFailure
    , rejectUnknownFields
    , sumEncoding
    , tagSingleConstructors
    , withObject
    , withText
    , (.!=)
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
    ( Word16, Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic, Rep )
import GHC.TypeLits
    ( Nat, Symbol )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )
import Servant.API
    ( MimeRender (..), MimeUnrender (..), OctetStream )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDerivation as AD
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
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

newtype ApiMaintenanceActionPostData = ApiMaintenanceActionPostData
    { maintenanceAction :: MaintenanceAction
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet ApiMaintenanceActionPostData)

newtype ApiMaintenanceAction = ApiMaintenanceAction
    { gcStakePools :: ApiT PoolMetadataGCStatus
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet ApiMaintenanceAction)

data ApiAsset = ApiAsset
    { policyId :: ApiT W.TokenPolicyId
    , assetName :: ApiT W.TokenName
    , fingerprint :: ApiT W.TokenFingerprint
    , metadata :: Maybe ApiAssetMetadata
    , metadataError :: Maybe ApiMetadataError
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiMetadataError = Fetch | Parse
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiAssetMetadata = ApiAssetMetadata
    { name :: Text
    , description :: Text
    , ticker :: Maybe Text
    , url :: Maybe (ApiT W.AssetURL)
    , logo :: Maybe (ApiT W.AssetLogo)
    , decimals :: Maybe (ApiT W.AssetDecimals)
    } deriving (Eq, Generic, Ord, Show)
      deriving anyclass NFData

toApiAsset
    :: Either TokenMetadataError (Maybe W.AssetMetadata)
    -> W.AssetId
    -> ApiAsset
toApiAsset metadata_ (W.AssetId policyId_ assetName_) = ApiAsset
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

data ApiAddress (n :: NetworkDiscriminant) = ApiAddress
    { id :: !(ApiT Address, Proxy n)
    , state :: !(ApiT AddressState)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiCredential =
      CredentialPubKey ByteString
    | CredentialScript (Script KeyHash)
    deriving (Eq, Generic, Show)

data ApiAddressData = ApiAddressData
    { address :: !ApiAddressDataPayload
    , validationLevel :: !(Maybe (ApiT ValidationLevel))
    } deriving (Eq, Generic, Show)

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
    } deriving (Eq, Generic, Show)

data ApiEpochInfo = ApiEpochInfo
    { epochNumber :: !(ApiT EpochNo)
    , epochStartTime :: !UTCTime
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

toApiEpochInfo :: EpochNo -> Qry ApiEpochInfo
toApiEpochInfo ep = ApiEpochInfo (ApiT ep) . fst <$> timeOfEpoch ep

data ApiSelectCoinsData (n :: NetworkDiscriminant)
    = ApiSelectForPayment (ApiSelectCoinsPayments n)
    | ApiSelectForDelegation ApiSelectCoinsAction
    deriving (Eq, Generic, Show, Typeable)

data ApiSelectCoinsPayments (n :: NetworkDiscriminant) = ApiSelectCoinsPayments
    { payments :: NonEmpty (AddressAmount (ApiT Address, Proxy n))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    } deriving (Eq, Generic, Show, Typeable)

newtype ApiSelectCoinsAction = ApiSelectCoinsAction
    { delegationAction :: ApiDelegationAction
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet ApiSelectCoinsAction)

data ApiCertificate
    = RegisterRewardAccount
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        }
    | JoinPool
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        , pool :: ApiT PoolId
        }
    | QuitPool
        { rewardAccountPath :: NonEmpty (ApiT DerivationIndex)
        }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

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
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiCoinSelectionChange (n :: NetworkDiscriminant) = ApiCoinSelectionChange
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiCoinSelectionOutput (n :: NetworkDiscriminant) = ApiCoinSelectionOutput
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    } deriving (Eq, Ord, Generic, Show, Typeable)
      deriving anyclass (NFData, Hashable)

data ApiCoinSelectionCollateral (n :: NetworkDiscriminant) =
    ApiCoinSelectionCollateral
        { id :: !(ApiT (Hash "Tx"))
        , index :: !Word32
        , address :: !(ApiT Address, Proxy n)
        , derivationPath :: NonEmpty (ApiT DerivationIndex)
        , amount :: !(Quantity "lovelace" Natural)
        }
    deriving (Eq, Generic, Show, Typeable)
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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletBalance = ApiWalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    , reward :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletAssetsBalance = ApiWalletAssetsBalance
    { available :: !(ApiT W.TokenMap)
    , total :: !(ApiT W.TokenMap)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiWalletPassphraseInfo = ApiWalletPassphraseInfo
    { lastUpdatedAt :: UTCTime
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletPassphraseInfo)

data ApiWalletDelegation = ApiWalletDelegation
    { active :: !ApiWalletDelegationNext
    , next :: ![ApiWalletDelegationNext]
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletDelegationNext = ApiWalletDelegationNext
    { status :: !ApiWalletDelegationStatus
    , target :: !(Maybe (ApiT PoolId))
    , changesAt :: !(Maybe ApiEpochInfo)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletDelegationStatus
    = NotDelegating
    | Delegating
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ApiWalletPassphrase = ApiWalletPassphrase
    { passphrase :: ApiT (Passphrase "lenient")
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletPassphrase)

newtype ApiWalletUtxoSnapshot = ApiWalletUtxoSnapshot
    { entries :: [ApiWalletUtxoSnapshotEntry]
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiWalletUtxoSnapshot)

data ApiWalletUtxoSnapshotEntry = ApiWalletUtxoSnapshotEntry
    { ada :: !(Quantity "lovelace" Natural)
    , adaMinimum :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiStakePool = ApiStakePool
    { id :: !(ApiT PoolId)
    , metrics :: !ApiStakePoolMetrics
    , metadata :: !(Maybe (ApiT StakePoolMetadata))
    , cost :: !(Quantity "lovelace" Natural)
    , margin :: !(Quantity "percent" Percentage)
    , pledge :: !(Quantity "lovelace" Natural)
    , retirement :: !(Maybe ApiEpochInfo)
    , flags :: ![ApiStakePoolFlag]
    } deriving (Eq, Generic, Show)

data ApiStakePoolFlag
    = Delisted
    deriving stock (Eq, Generic, Show)
    deriving anyclass NFData

data ApiStakePoolMetrics = ApiStakePoolMetrics
    { nonMyopicMemberRewards :: !(Quantity "lovelace" Natural)
    , relativeStake :: !(Quantity "percent" Percentage)
    , saturation :: !Double
    , producedBlocks :: !(Quantity "block" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiUtxoStatistics = ApiUtxoStatistics
    { total :: !(Quantity "lovelace" Natural)
    , scale :: !(ApiT BoundType)
    , distribution :: !(Map Word64 Word64)
    } deriving (Eq, Generic, Show)
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
    , passphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

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
    , passphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

data ByronWalletFromXPrvPostData = ByronWalletFromXPrvPostData
    { name :: !(ApiT WalletName)
    , encryptedRootPrivateKey :: !(ApiT XPrv)
    -- ^ A root private key hex-encoded, encrypted using a given passphrase.
    -- The underlying key should contain: private key, chain code, and public key
    , passphraseHash :: !(ApiT (Hash "encryption"))
    -- ^ A hash of master passphrase. The hash should be an output of a
    -- Scrypt function with the following parameters:
    -- - logN = 14
    -- - r = 8
    -- - p = 1
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiAccountPublicKey = ApiAccountPublicKey
    { key :: (ApiT XPub)
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiAccountPublicKey)

newtype WalletOrAccountPostData = WalletOrAccountPostData
    { postData :: Either WalletPostData AccountPostData
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet WalletOrAccountPostData)

data AccountPostData = AccountPostData
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !ApiAccountPublicKey
    , addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    } deriving (Eq, Generic, Show)

newtype WalletPutData = WalletPutData
    { name :: (Maybe (ApiT WalletName))
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet WalletPutData)

newtype SettingsPutData = SettingsPutData
    { settings :: (ApiT W.Settings)
    }
    deriving (Eq, Generic)
    deriving Show via (Quiet SettingsPutData)

data WalletPutPassphraseData = WalletPutPassphraseData
    { oldPassphrase :: !(ApiT (Passphrase "raw"))
    , newPassphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

data ByronWalletPutPassphraseData = ByronWalletPutPassphraseData
    { oldPassphrase :: !(Maybe (ApiT (Passphrase "lenient")))
    , newPassphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

data ApiConstructTransaction (n :: NetworkDiscriminant) = ApiConstructTransaction
    { transaction :: !(ApiT SealedTx)
    , coinSelection :: !(ApiCoinSelection n)
    , mintedBurned :: !(Maybe (NonEmpty (ApiT ApiMintedBurnedInfo)))
    , fee :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show, Typeable)
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
data ApiConstructTransactionData (n :: NetworkDiscriminant) = ApiConstructTransactionData
    { payments :: !(Maybe (ApiPaymentDestination n))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    , mintedBurned :: !(Maybe (NonEmpty (ApiMintBurnData n)))
    , delegations :: !(Maybe (NonEmpty ApiMultiDelegationAction))
    , validityInterval :: !(Maybe ApiValidityInterval)
    } deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

data ApiPaymentDestination (n :: NetworkDiscriminant)
    = ApiPaymentAddresses !(NonEmpty (AddressAmount (ApiAddressIdT n)))
    -- ^ Pay amounts to one or more addresses.
    | ApiPaymentAll !(NonEmpty  (ApiT Address, Proxy n))
    -- ^ Migrate all money to one or more addresses.
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

-- | Times where transactions are valid.
data ApiValidityInterval = ApiValidityInterval
    { invalidBefore :: !ApiValidityBound
    -- ^ Tx is not valid before this time. Defaults to genesis.
    , invalidHereafter :: !ApiValidityBound
    -- ^ Tx is not valid at this time and after. Defaults to now + 2 hours.
    } deriving (Eq, Generic, Show)
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
    } deriving (Eq, Generic, Show)

-- | Legacy transaction API.
data PostTransactionOldData (n :: NetworkDiscriminant) = PostTransactionOldData
    { payments :: !(NonEmpty (AddressAmount (ApiT Address, Proxy n)))
    , passphrase :: !(ApiT (Passphrase "lenient"))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    } deriving (Eq, Generic, Show, Typeable)

-- | Legacy transaction API.
data PostTransactionFeeOldData (n :: NetworkDiscriminant) = PostTransactionFeeOldData
    { payments :: (NonEmpty (AddressAmount (ApiT Address, Proxy n)))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    } deriving (Eq, Generic, Show, Typeable)

type ApiBase64 = ApiBytesT 'Base64 ByteString

newtype ApiSerialisedTransaction = ApiSerialisedTransaction
    { transaction :: ApiT SealedTx
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (NFData)

data ApiExternalInput (n :: NetworkDiscriminant) = ApiExternalInput
    { id :: !(ApiT (Hash "Tx"))
    , index :: !Word32
    , address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    , datum :: !(Maybe (ApiT (Hash "Datum")))
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiBalanceTransactionPostData (n :: NetworkDiscriminant) = ApiBalanceTransactionPostData
    { transaction :: !(ApiT SealedTx)
    , inputs :: ![ApiExternalInput n]
    , redeemers :: ![ApiRedeemer n]
    } deriving (Eq, Generic, Show)

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
    } deriving (Eq, Generic, Show)

data ApiNetworkParameters = ApiNetworkParameters
    { genesisBlockHash :: !(ApiT (Hash "Genesis"))
    , blockchainStartTime :: !(ApiT StartTime)
    , slotLength :: !(Quantity "second" NominalDiffTime)
    , epochLength :: !(Quantity "slot" Word32)
    , securityParameter :: !(Quantity "block" Word32)
    , activeSlotCoefficient :: !(Quantity "percent" Double)
    , decentralizationLevel :: !(Quantity "percent" Percentage)
    , desiredPoolNumber :: !Word16
    , minimumUtxoValue :: !(Quantity "lovelace" Natural)
    , maximumTokenBundleSize :: !(Quantity "byte" Natural)
    , eras :: !ApiEraInfo
    , maximumCollateralInputCount :: !Word16
    , minimumCollateralPercentage :: !Natural
    , executionUnitPrices :: !(Maybe ExecutionUnitPrices)
    } deriving (Eq, Generic, Show)

data ApiEraInfo = ApiEraInfo
    { byron :: !(Maybe ApiEpochInfo)
    , shelley :: !(Maybe ApiEpochInfo)
    , allegra :: !(Maybe ApiEpochInfo)
    , mary :: !(Maybe ApiEpochInfo)
    , alonzo :: !(Maybe ApiEpochInfo)
    } deriving (Eq, Generic, Show)

toApiNetworkParameters
    :: Monad m
    => NetworkParameters
    -> TxConstraints
    -> (EpochNo -> m ApiEpochInfo)
    -> m ApiNetworkParameters
toApiNetworkParameters (NetworkParameters gp sp pp) txConstraints toEpochInfo = do
    byron <- traverse toEpochInfo (pp ^. #eras . #byron)
    shelley <- traverse toEpochInfo (pp ^. #eras . #shelley)
    allegra <- traverse toEpochInfo (pp ^. #eras . #allegra)
    mary <- traverse toEpochInfo (pp ^. #eras . #mary)
    alonzo <- traverse toEpochInfo (pp ^. #eras . #alonzo)

    let apiEras = ApiEraInfo { byron, shelley, allegra, mary, alonzo }

    return $ ApiNetworkParameters
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
            $ unDecentralizationLevel
            $ view #decentralizationLevel pp
        , desiredPoolNumber = view #desiredNumberOfStakePools pp
        , minimumUtxoValue = toApiCoin $ case (view #minimumUTxOvalue pp) of
            MinimumUTxOValue c ->
                c
            MinimumUTxOValueCostPerWord _perWord ->
                txOutputMinimumAdaQuantity txConstraints TokenMap.empty
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
  where
    toApiCoin = Quantity . fromIntegral . unCoin


newtype ApiTxId = ApiTxId
    { id :: ApiT (Hash "Tx")
    }
    deriving (Eq, Generic)
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
    , outputs :: ![AddressAmount (ApiT Address, Proxy n)]
    , collateral :: ![ApiTxCollateral n]
    , withdrawals :: ![ApiWithdrawal n]
    , mint :: !(ApiT W.TokenMap)
    , status :: !(ApiT TxStatus)
    , metadata :: !ApiTxMetadata
    , scriptValidity :: !(Maybe (ApiT TxScriptValidity))
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiWalletInput (n :: NetworkDiscriminant) = ApiWalletInput
    { id :: !(ApiT (Hash "Tx"))
    , index :: !Word32
    , address :: !(ApiT Address, Proxy n)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiTxInputGeneral (n :: NetworkDiscriminant) =
      ExternalInput (ApiT TxIn)
    | WalletInput (ApiWalletInput n)
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ResourceContext = External | Our
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiWithdrawalGeneral (n :: NetworkDiscriminant) = ApiWithdrawalGeneral
    { stakeAddress :: !(ApiT W.RewardAccount, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , context :: !ResourceContext
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletOutput (n :: NetworkDiscriminant) = ApiWalletOutput
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiTxOutputGeneral (n :: NetworkDiscriminant) =
      ExternalOutput (AddressAmount (ApiT Address, Proxy n))
    | WalletOutput (ApiWalletOutput n)
      deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiExternalCertificate (n :: NetworkDiscriminant)
    = RegisterRewardAccountExternal
        { rewardAccount :: !(ApiT W.RewardAccount, Proxy n)
        }
    | JoinPoolExternal
        { rewardAccount :: !(ApiT W.RewardAccount, Proxy n)
        , pool :: ApiT PoolId
        }
    | QuitPoolExternal
        { rewardAccount :: !(ApiT W.RewardAccount, Proxy n)
        }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiRegisterPool = ApiRegisterPool
    { poolId :: !(ApiT PoolId)
    , poolOwners :: ![ApiT W.PoolOwner]
    , poolMargin :: !(Quantity "percent" Percentage)
    , poolCost :: !(Quantity "lovelace" Natural)
    , poolPledge :: !(Quantity "lovelace" Natural)
    , poolMetadata :: Maybe (ApiT W.StakePoolMetadataUrl, ApiT W.StakePoolMetadataHash)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiDeregisterPool = ApiDeregisterPool
    { poolId :: !(ApiT PoolId)
    , retirementEpoch :: !(ApiT EpochNo)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiAnyCertificate n =
      WalletDelegationCertificate ApiCertificate
    | DelegationCertificate (ApiExternalCertificate n)
    | StakePoolRegister ApiRegisterPool
    | StakePoolDeregister ApiDeregisterPool
    | OtherCertificate (ApiT NonWalletCertificate)
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiDecodedTransaction (n :: NetworkDiscriminant) = ApiDecodedTransaction
    { id :: !(ApiT (Hash "Tx"))
    , fee :: !(Quantity "lovelace" Natural)
    , inputs :: ![ApiTxInputGeneral n]
    , outputs :: ![ApiTxOutputGeneral n]
    , collateral :: ![ApiTxInputGeneral n]
    , withdrawals :: ![ApiWithdrawalGeneral n]
    , assetsMinted :: !(ApiT W.TokenMap)
    , assetsBurned :: !(ApiT W.TokenMap)
    , certificates :: ![ApiAnyCertificate n]
    , depositsTaken :: ![Quantity "lovelace" Natural]
    , depositsReturned :: ![Quantity "lovelace" Natural]
    , metadata :: !ApiTxMetadata
    , scriptValidity :: !(Maybe (ApiT TxScriptValidity))
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

-- | The response cardano-wallet returns upon successful submission of a
-- mint/burn transaction.
data ApiMintedBurnedTransaction (n :: NetworkDiscriminant) = ApiMintedBurnedTransaction
    { transaction :: !(ApiTransaction n)
    -- ^ Information about the mint/burn transaction itself.
    , mintedBurned :: !(NonEmpty (ApiT ApiMintedBurnedInfo))
    -- ^ Helpful information about each unique asset minted or burned (where the
    -- identity is the policyId + asset name of the asset).
    }
    deriving (Eq, Generic, Show, Typeable)
    deriving anyclass NFData

data ApiMintedBurnedInfo = ApiMintedBurnedInfo
    { verificationKeyIndex :: !(ApiT DerivationIndex)
    -- ^ The monetary policy index the asset was minted/burnt under.
    , policyId            :: !(ApiT W.TokenPolicyId)
    -- ^ The policy ID the asset was minted/burnt under.
    , assetName           :: !(ApiT W.TokenName)
    -- ^ The name of the asset minted/burnt.
    , subject             :: !(ApiT W.TokenFingerprint)
    -- ^ The subject of the asset minted/burnt. This is useful to users wishing
    -- to attach metadata to their asset.
    , policyScript        :: !(ApiT (Script KeyHash))
    -- ^ The script which this asset was minted and/or burned under
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiTxMetadata = ApiTxMetadata
    { getApiTxMetadata :: Maybe (ApiT TxMetadata)
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiTxMetadata)

data ApiWithdrawal n = ApiWithdrawal
    { stakeAddress :: !(ApiT W.RewardAccount, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiCoinSelectionWithdrawal n = ApiCoinSelectionWithdrawal
    { stakeAddress :: !(ApiT W.RewardAccount, Proxy n)
    , derivationPath :: !(NonEmpty (ApiT DerivationIndex))
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWithdrawalPostData
    = SelfWithdrawal
    | ExternalWithdrawal (ApiMnemonicT '[15,18,21,24])
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiTxInput (n :: NetworkDiscriminant) = ApiTxInput
    { source :: !(Maybe (AddressAmount (ApiT Address, Proxy n)))
    , input :: !(ApiT TxIn)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data ApiTxCollateral (n :: NetworkDiscriminant) = ApiTxCollateral
    { source :: !(Maybe (AddressAmountNoAssets (ApiT Address, Proxy n)))
    , input :: !(ApiT TxIn)
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

data AddressAmount addr = AddressAmount
    { address :: !addr
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data AddressAmountNoAssets addr = AddressAmountNoAssets
    { address :: !addr
    , amount :: !(Quantity "lovelace" Natural)
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

coinToQuantity :: Integral n => Coin -> Quantity "lovelace" n
coinToQuantity = Quantity . fromIntegral . unCoin

coinFromQuantity :: Integral n => Quantity "lovelace" n -> Coin
coinFromQuantity = Coin . fromIntegral . getQuantity

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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiSlotId = ApiSlotId
    { epochNumber :: !(ApiT EpochNo)
    , slotNumber :: !(ApiT SlotInEpoch)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiBlockReference = ApiBlockReference
    { absoluteSlotNumber :: !(ApiT SlotNo)
    , slotId :: !ApiSlotId
    , time :: !UTCTime
    , block :: !ApiBlockInfo
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiBlockInfo = ApiBlockInfo
    { height :: Quantity "block" Natural
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiBlockInfo)

data ApiEra
    = ApiByron
    | ApiShelley
    | ApiAllegra
    | ApiMary
    | ApiAlonzo
    deriving (Show, Eq, Generic, Enum, Ord, Bounded)
    deriving anyclass NFData

instance FromJSON ApiEra where
    parseJSON = genericParseJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }
instance ToJSON ApiEra where
    toJSON = genericToJSON $ Aeson.defaultOptions
        { constructorTagModifier = drop 4 . camelTo2 '_' }

data ApiNetworkInformation = ApiNetworkInformation
    { syncProgress :: !(ApiT SyncProgress)
    , nextEpoch :: !(Maybe ApiEpochInfo)
    , nodeTip :: !ApiBlockReference
    , networkTip :: !(Maybe ApiSlotReference)
    , nodeEra :: !ApiEra
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data NtpSyncingStatus =
      NtpSyncingStatusUnavailable
    | NtpSyncingStatusPending
    | NtpSyncingStatusAvailable
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiNtpStatus = ApiNtpStatus
    { status :: !NtpSyncingStatus
    , offset :: !(Maybe (Quantity "microsecond" Integer))
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiNetworkClock = ApiNetworkClock
    { ntpStatus :: ApiNtpStatus
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiNetworkClock)

data ApiPostRandomAddressData = ApiPostRandomAddressData
    { passphrase :: !(ApiT (Passphrase "lenient"))
    , addressIndex :: !(Maybe (ApiT (Index 'AD.Hardened 'AddressK)))
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiWalletMigrationPlanPostData (n :: NetworkDiscriminant) =
    ApiWalletMigrationPlanPostData
    { addresses :: NonEmpty (ApiT Address, Proxy n)
    }
    deriving (Eq, Generic, Typeable)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiWalletMigrationPlanPostData n))

data ApiWalletMigrationPostData (n :: NetworkDiscriminant) (s :: Symbol) =
    ApiWalletMigrationPostData
    { passphrase :: !(ApiT (Passphrase s))
    , addresses :: !(NonEmpty (ApiT Address, Proxy n))
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

newtype ApiPutAddressesData (n :: NetworkDiscriminant) = ApiPutAddressesData
    { addresses :: [(ApiT Address, Proxy n)]
    }
    deriving (Eq, Generic, Typeable)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiPutAddressesData n))

data ApiWalletMigrationBalance = ApiWalletMigrationBalance
    { ada :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletMigrationPlan (n :: NetworkDiscriminant) = ApiWalletMigrationPlan
    { selections :: !(NonEmpty (ApiCoinSelection n))
    , totalFee :: Quantity "lovelace" Natural
    , balanceLeftover :: ApiWalletMigrationBalance
    , balanceSelected :: ApiWalletMigrationBalance
    } deriving (Eq, Generic, Show, Typeable)
      deriving anyclass NFData

newtype ApiWithdrawRewards = ApiWithdrawRewards Bool
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiWalletSignData = ApiWalletSignData
    { metadata :: ApiT TxMetadata
    , passphrase :: ApiT (Passphrase "lenient")
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data VerificationKeyHashing = WithHashing | WithoutHashing
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiVerificationKeyShelley = ApiVerificationKeyShelley
    { getApiVerificationKey :: (ByteString, Role)
    , hashed :: VerificationKeyHashing
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiVerificationKeyShared = ApiVerificationKeyShared
    { getApiVerificationKey :: (ByteString, Role)
    , hashed :: VerificationKeyHashing
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data KeyFormat = Extended | NonExtended
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance ToText KeyFormat where
    toText Extended = "extended"
    toText NonExtended = "non_extended"

instance ToHttpApiData KeyFormat where
    toUrlPiece = toText

instance FromText KeyFormat where
    fromText txt = case txt of
        "extended" -> Right Extended
        "non_extended" -> Right NonExtended
        _ -> Left $ TextDecodingError $ unwords
            [ "I couldn't parse the given key format."
            , "I am expecting one of the words 'extended' or"
            , "'non_extended'."]

instance FromHttpApiData KeyFormat where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

data ApiPostAccountKeyData = ApiPostAccountKeyData
    { passphrase :: ApiT (Passphrase "raw")
    , format :: KeyFormat
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiPostAccountKeyDataWithPurpose = ApiPostAccountKeyDataWithPurpose
    { passphrase :: ApiT (Passphrase "raw")
    , format :: KeyFormat
    , purpose :: Maybe (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiAccountKey = ApiAccountKey
    { getApiAccountKey :: ByteString
    , format :: KeyFormat
    , purpose :: Index 'Hardened 'PurposeK
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiAccountKeyShared = ApiAccountKeyShared
    { getApiAccountKey :: ByteString
    , format :: KeyFormat
    , purpose :: Index 'Hardened 'PurposeK
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data XPubOrSelf = SomeAccountKey XPub | Self
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiScriptTemplateEntry = ApiScriptTemplateEntry
    { cosigners :: Map Cosigner XPubOrSelf
    , template :: Script Cosigner
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiSharedWalletPostDataFromMnemonics = ApiSharedWalletPostDataFromMnemonics
    { name :: !(ApiT WalletName)
    , mnemonicSentence :: !(ApiMnemonicT (AllowedMnemonics 'Shelley))
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT (AllowedMnemonics 'SndFactor)))
    , passphrase :: !(ApiT (Passphrase "raw"))
    , accountIndex :: !(ApiT DerivationIndex)
    , paymentScriptTemplate :: !ApiScriptTemplateEntry
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplateEntry)
    , scriptValidation :: !(Maybe (ApiT ValidationLevel))
    } deriving (Eq, Generic, Show)

data ApiSharedWalletPostDataFromAccountPubX = ApiSharedWalletPostDataFromAccountPubX
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !ApiAccountPublicKey
    , accountIndex :: !(ApiT DerivationIndex)
    , paymentScriptTemplate :: !ApiScriptTemplateEntry
    , delegationScriptTemplate :: !(Maybe ApiScriptTemplateEntry)
    , scriptValidation :: !(Maybe (ApiT ValidationLevel))
    } deriving (Eq, Generic, Show)

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
    , paymentScriptTemplate :: !ScriptTemplate
    , delegationScriptTemplate :: !(Maybe ScriptTemplate)
    , delegation :: !ApiWalletDelegation
    , balance :: !ApiWalletBalance
    , assets :: !ApiWalletAssetsBalance
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiPendingSharedWallet = ApiPendingSharedWallet
    { id :: !(ApiT WalletId)
    , name :: !(ApiT WalletName)
    , accountIndex :: !(ApiT DerivationIndex)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , paymentScriptTemplate :: !ScriptTemplate
    , delegationScriptTemplate :: !(Maybe ScriptTemplate)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiSharedWallet = ApiSharedWallet
    { wallet :: Either ApiPendingSharedWallet ApiActiveSharedWallet
    }
    deriving (Eq, Generic)
    deriving anyclass NFData
    deriving Show via (Quiet ApiSharedWallet)

data ApiSharedWalletPatchData = ApiSharedWalletPatchData
    { cosigner :: !(ApiT Cosigner)
    , accountPublicKey :: !ApiAccountPublicKey
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

-- | Error codes returned by the API, in the form of snake_cased strings
data ApiErrorCode
    = NoSuchWallet
    | NoSuchTransaction
    | TransactionAlreadyInLedger
    | WalletAlreadyExists
    | NoRootKey
    | WrongEncryptionPassphrase
    | MalformedTxPayload
    | KeyNotFoundForAddress
    | NotEnoughMoney
    | InsufficientCollateral
    | TransactionIsTooBig
    | InputsDepleted
    | CannotCoverFee
    | InvalidCoinSelection
    | NetworkUnreachable
    | NetworkMisconfigured
    | NetworkQueryFailed
    | CreatedInvalidTransaction
    | CreatedMultidelegationTransaction
    | CreatedMultiaccountTransaction
    | RejectedByCoreNode
    | BadRequest
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | UnsupportedMediaType
    | UnexpectedError
    | StartTimeLaterThanEndTime
    | UnableToDetermineCurrentEpoch
    | NotSynced
    | NothingToMigrate
    | NoSuchPool
    | PoolAlreadyJoined
    | NotDelegatingTo
    | NotImplemented
    | WalletNotResponding
    | AddressAlreadyExists
    | InvalidWalletType
    | QueryParamMissing
    | NonNullRewards
    | UtxoTooSmall
    | MinWithdrawalWrong
    | AlreadyWithdrawing
    | WithdrawalNotWorth
    | PastHorizon
    | UnableToAssignInputOutput
    | SoftDerivationRequired
    | HardenedDerivationRequired
    | AssetNotPresent
    | OutputTokenBundleSizeExceedsLimit
    | OutputTokenQuantityExceedsLimit
    | SharedWalletNotPending
    | SharedWalletNoDelegationTemplate
    | SharedWalletKeyAlreadyExists
    | SharedWalletNoSuchCosigner
    | SharedWalletCannotUpdateKey
    | SharedWalletScriptTemplateInvalid
    | TokensMintedButNotSpentOrBurned
    | TransactionAlreadyBalanced
    | RedeemerScriptFailure
    | RedeemerTargetNotFound
    | UnresolvedInputs
    | RedeemerInvalidData
    | ExistingKeyWitnesses
    | ForeignTransaction
    | MissingWitnessesInTransaction
    deriving (Eq, Generic, Show, Data, Typeable)
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
    parseJSON = fromTextJSON "ISO-8601 Time"
instance ToJSON (ApiT Iso8601Time) where
    toJSON = toTextJSON

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

data ApiPoolId
    = ApiPoolIdPlaceholder
    | ApiPoolId PoolId
    deriving (Eq, Generic, Show)

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

instance FromText (ApiT (Hash "encryption"))  where
    fromText txt = case convertFromBase Base16 $ T.encodeUtf8 txt of
        Right bytes -> Right $ ApiT $ Hash bytes
        Left _ -> textDecodingError
      where
        textDecodingError = Left $ TextDecodingError $ unwords
            [ "Invalid encrypted passphrase:"
            , "expecting a hex-encoded value."
            ]

instance DecodeAddress n => FromHttpApiData (ApiT Address, Proxy n) where
    parseUrlPiece txt = do
        let proxy = Proxy @n
        addr <- bimap (T.pack . getTextDecodingError) ApiT (decodeAddress @n txt)
        return (addr, proxy)

instance EncodeAddress n => ToHttpApiData (ApiT Address, Proxy n) where
    toUrlPiece = encodeAddress @n . getApiT . fst

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
    } deriving (Eq, Generic, Show)
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

-- | Polymorphic wrapper type to put around primitive types and, 3rd party lib
-- types to avoid defining orphan instances and/or, undesirable instances on
-- primitive types. It helps to keep a nice separation of concerns between the
-- API layer and other modules.
newtype ApiT a =
    ApiT { getApiT :: a }
    deriving (Generic, Eq, Functor)
    deriving newtype (Semigroup, Monoid, Hashable)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiT a))
deriving instance Ord a => Ord (ApiT a)

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
     { _index :: !Natural
    , _key :: !(ApiT W.RewardAccount, Proxy n)
    , _stake :: !(Quantity "lovelace" Natural)
      -- ^ The total ada this stake key controls / is associated with. This
      -- also includes the reward balance.
    , _rewardBalance :: !(Quantity "lovelace" Natural)
      -- ^ The current reward balance (not lifetime).
    , _delegation :: !ApiWalletDelegation
      -- ^ The delegation of this stake key
    } deriving (Generic, Eq, Show)

-- | A stake key found in the wallet UTxO, but which isn't ours.
--
-- We /could/ provide the current delegation status for foreign stake
-- keys.
data ApiForeignStakeKey (n :: NetworkDiscriminant) = ApiForeignStakeKey
    { _key :: !(ApiT W.RewardAccount, Proxy n)
    , _stake :: !(Quantity "lovelace" Natural)
      -- ^ The total ada this stake key controls / is associated with. This
      -- also includes the reward balance.
    , _rewardBalance :: !(Quantity "lovelace" Natural)
      -- ^ The current reward balance (not lifetime).
    } deriving (Generic, Eq, Show)

-- | For describing how much stake is associated with no stake key.
newtype ApiNullStakeKey = ApiNullStakeKey
    { _stake :: Quantity "lovelace" Natural
      -- ^ The total stake of the wallet UTxO that is not associated with a
      -- stake key, because it's part of an enterprise address.
    }
    deriving (Generic, Eq)
    deriving Show via (Quiet ApiNullStakeKey)

-- | Collection of stake keys associated with a wallet.
data ApiStakeKeys (n :: NetworkDiscriminant) = ApiStakeKeys
    { _ours :: ![ApiOurStakeKey n]
    , _foreign :: ![ApiForeignStakeKey n]
    , _none :: !ApiNullStakeKey
    } deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
                               JSON Instances
-------------------------------------------------------------------------------}

instance DecodeAddress n => FromJSON (ApiAddress n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiAddress n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiMetadataError where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON ApiMetadataError where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON ApiAsset where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiAsset where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiOurStakeKey n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiOurStakeKey n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiForeignStakeKey n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiForeignStakeKey n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiNullStakeKey where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNullStakeKey where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiStakeKeys n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiStakeKeys n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT W.TokenPolicyId) where
    parseJSON = fromTextJSON "PolicyId"
instance ToJSON (ApiT W.TokenPolicyId) where
    toJSON = toTextJSON

instance FromJSON (ApiT W.TokenName) where
    parseJSON = withText "AssetName"
        (fmap (ApiT . W.UnsafeTokenName) . eitherToParser . fromHexText)
instance ToJSON (ApiT W.TokenName) where
    toJSON = toJSON . hexText . W.unTokenName . getApiT

instance FromJSON (ApiT W.TokenFingerprint) where
    parseJSON = fromTextJSON "TokenFingerprint"
instance ToJSON (ApiT W.TokenFingerprint) where
    toJSON = toTextJSON

instance FromJSON ApiAssetMetadata where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiAssetMetadata where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance ToJSON (ApiT DerivationIndex) where
    toJSON = toTextJSON
instance FromJSON (ApiT DerivationIndex) where
    parseJSON = fromTextJSON "DerivationIndex"

instance ToJSON ApiVerificationKeyShelley where
    toJSON (ApiVerificationKeyShelley (pub, role_) hashed) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case role_ of
            UtxoExternal -> case hashed of
                WithHashing -> [humanReadablePart|addr_vkh|]
                WithoutHashing -> [humanReadablePart|addr_vk|]
            UtxoInternal -> case hashed of
                WithHashing -> [humanReadablePart|addr_vkh|]
                WithoutHashing -> [humanReadablePart|addr_vk|]
            MutableAccount -> case hashed of
                WithHashing -> [humanReadablePart|stake_vkh|]
                WithoutHashing -> [humanReadablePart|stake_vk|]

instance FromJSON ApiVerificationKeyShelley where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed verification key")
        (role, hashing) <- parseRoleHashing hrp
        payload <- case hashing of
            WithoutHashing -> parsePubVer bytes
            WithHashing -> parsePubVerHash bytes
        pure $ ApiVerificationKeyShelley (payload,role) hashing
      where
        parseRoleHashing = \case
            hrp | hrp == [humanReadablePart|addr_vk|] -> pure (UtxoExternal, WithoutHashing)
            hrp | hrp == [humanReadablePart|stake_vk|] -> pure (MutableAccount, WithoutHashing)
            hrp | hrp == [humanReadablePart|addr_vkh|] -> pure (UtxoExternal, WithHashing)
            hrp | hrp == [humanReadablePart|stake_vkh|] -> pure (MutableAccount, WithHashing)
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected one of:\
                \ \"addr_vkh\", \"stake_vkh\",\"addr_vk\" or \"stake_vk\"."

parseBech32
    :: Text
    -> Text
    -> Aeson.Parser (Bech32.HumanReadablePart, ByteString)
parseBech32 err =
    either (const $ fail errBech32) parseDataPart . Bech32.decodeLenient
  where
      errBech32 =
          T.unpack err <>". Expected a bech32-encoded key."

parseDataPart
    :: (Bech32.HumanReadablePart, Bech32.DataPart)
    -> Aeson.Parser (Bech32.HumanReadablePart, ByteString)
parseDataPart =
    maybe (fail errDataPart) pure . traverse dataPartToBytes
  where
      errDataPart =
          "Couldn't decode data-part to valid UTF-8 bytes."

parsePubVer :: MonadFail f => ByteString -> f ByteString
parsePubVer bytes
    | BS.length bytes == 32 =
          pure bytes
    | otherwise =
          fail "Not a valid Ed25519 public key. Must be 32 bytes, without chain code"

parsePubVerHash :: MonadFail f => ByteString -> f ByteString
parsePubVerHash bytes
    | BS.length bytes == 28 =
          pure bytes
    | otherwise =
          fail "Not a valid hash of Ed25519 public key. Must be 28 bytes."

instance ToJSON ApiVerificationKeyShared where
    toJSON (ApiVerificationKeyShared (pub, role_) hashed) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case role_ of
            UtxoExternal -> case hashed of
                WithHashing -> [humanReadablePart|addr_shared_vkh|]
                WithoutHashing -> [humanReadablePart|addr_shared_vk|]
            UtxoInternal -> case hashed of
                WithHashing -> [humanReadablePart|addr_shared_vkh|]
                WithoutHashing -> [humanReadablePart|addr_shared_vk|]
            MutableAccount -> case hashed of
                WithHashing -> [humanReadablePart|stake_shared_vkh|]
                WithoutHashing -> [humanReadablePart|stake_shared_vk|]

instance FromJSON ApiVerificationKeyShared where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed verification key")
        (role, hashing) <- parseRoleHashing hrp
        payload <- case hashing of
            WithoutHashing -> parsePubVer bytes
            WithHashing -> parsePubVerHash bytes
        pure $ ApiVerificationKeyShared (payload,role) hashing
      where
        parseRoleHashing = \case
            hrp | hrp == [humanReadablePart|addr_shared_vk|] -> pure (UtxoExternal, WithoutHashing)
            hrp | hrp == [humanReadablePart|stake_shared_vk|] -> pure (MutableAccount, WithoutHashing)
            hrp | hrp == [humanReadablePart|addr_shared_vkh|] -> pure (UtxoExternal, WithHashing)
            hrp | hrp == [humanReadablePart|stake_shared_vkh|] -> pure (MutableAccount, WithHashing)
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected one of:\
                \ \"addr_shared_vkh\", \"stake_shared_vkh\",\"addr_shared_vk\" or \"stake_shared_vk\"."

instance ToJSON ApiAccountKey where
    toJSON (ApiAccountKey pub extd purpose') =
        toJSON $ Bech32.encodeLenient (hrp purpose') $ dataPartFromBytes pub
      where
        hrp p
            | p == purposeCIP1854 = case extd of
                  Extended -> [humanReadablePart|acct_shared_xvk|]
                  NonExtended -> [humanReadablePart|acct_shared_vk|]
            | otherwise = case extd of
                  Extended -> [humanReadablePart|acct_xvk|]
                  NonExtended -> [humanReadablePart|acct_vk|]

instance FromJSON ApiAccountKey where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed extended/normal account public key")
        (extended', purpose') <- parseHrp hrp
        pub <- parsePub bytes extended'
        pure $ ApiAccountKey pub extended' purpose'
      where
        parseHrp = \case
            hrp | hrp == [humanReadablePart|acct_xvk|] -> pure (Extended, purposeCIP1852)
            hrp | hrp == [humanReadablePart|acct_vk|] -> pure (NonExtended, purposeCIP1852)
            hrp | hrp == [humanReadablePart|acct_shared_xvk|] -> pure (Extended, purposeCIP1854)
            hrp | hrp == [humanReadablePart|acct_shared_vk|] -> pure (NonExtended, purposeCIP1854)
            _ -> fail errHrp
          where
              errHrp =
                  "Unrecognized human-readable part. Expected one of:\
                  \ \"acct_xvk\", \"acct_vk\", \"acct_shared_xvk\" or \"acct_shared_vk\"."

parsePubErr :: IsString p => KeyFormat -> p
parsePubErr = \case
    Extended ->
        "Not a valid Ed25519 extended public key. Must be 64 bytes, with chain code"
    NonExtended ->
        "Not a valid Ed25519 normal public key. Must be 32 bytes, without chain code"

parsePub :: MonadFail f => ByteString -> KeyFormat -> f ByteString
parsePub bytes extd
    | BS.length bytes == bytesExpectedLength =
          pure bytes
    | otherwise =
          fail $ parsePubErr extd
  where
    bytesExpectedLength = case extd of
        Extended -> 64
        NonExtended -> 32

instance ToJSON ApiAccountKeyShared where
    toJSON (ApiAccountKeyShared pub extd _) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case extd of
            Extended -> [humanReadablePart|acct_shared_xvk|]
            NonExtended -> [humanReadablePart|acct_shared_vk|]

instance FromJSON ApiAccountKeyShared where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= (parseBech32 "Malformed extended/normal account public key")
        extended' <- parseHrp hrp
        pub <- parsePub bytes extended'
        pure $ ApiAccountKeyShared pub extended' purposeCIP1854
      where
        parseHrp = \case
            hrp | hrp == [humanReadablePart|acct_shared_xvk|] -> pure Extended
            hrp | hrp == [humanReadablePart|acct_shared_vk|] -> pure NonExtended
            _ -> fail errHrp
          where
              errHrp =
                  "Unrecognized human-readable part. Expected one of:\
                  \ \"acct_shared_xvk\" or \"acct_shared_vk\"."


instance FromJSON KeyFormat where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON KeyFormat where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON ApiPostAccountKeyData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiPostAccountKeyData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiPostAccountKeyDataWithPurpose where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiPostAccountKeyDataWithPurpose where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiEpochInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiEpochInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiSelectCoinsAction where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiSelectCoinsAction where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiSelectCoinsPayments n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiSelectCoinsPayments n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiSelectCoinsData n) where
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

instance EncodeAddress n => ToJSON (ApiSelectCoinsData n) where
    toJSON (ApiSelectForPayment v) = toJSON v
    toJSON (ApiSelectForDelegation v) = toJSON v

instance (DecodeStakeAddress n, DecodeAddress n) =>
    FromJSON (ApiCoinSelection n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance (EncodeStakeAddress n, EncodeAddress n) =>
    ToJSON (ApiCoinSelection n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

apiCertificateOptions :: Aeson.Options
apiCertificateOptions = Aeson.defaultOptions
      { constructorTagModifier = camelTo2 '_'
      , tagSingleConstructors = True
      , fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
      , omitNothingFields = True
      , sumEncoding = TaggedObject
          {
            tagFieldName = "certificate_type"
          , contentsFieldName = "details" -- this isn't actually used
          }
      }

instance FromJSON ApiCertificate where
    parseJSON = genericParseJSON apiCertificateOptions

instance ToJSON ApiCertificate where
    toJSON = genericToJSON apiCertificateOptions

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

instance DecodeAddress n => FromJSON (ApiCoinSelectionChange n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionChange n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelectionCollateral n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionCollateral n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelectionOutput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionOutput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiCoinSelectionWithdrawal n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiCoinSelectionWithdrawal n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance {-# OVERLAPS #-} DecodeAddress n => FromJSON (ApiT Address, Proxy n)
  where
    parseJSON x = do
        let proxy = Proxy @n
        addr <- parseJSON x >>= eitherToParser
            . bimap ShowFmt ApiT
            . decodeAddress @n
        return (addr, proxy)
instance {-# OVERLAPS #-} EncodeAddress n => ToJSON (ApiT Address, Proxy n)
  where
    toJSON (addr, _) = toJSON . encodeAddress @n . getApiT $ addr

instance FromJSON (ApiT AddressState) where
    parseJSON = fmap ApiT . genericParseJSON defaultSumTypeOptions
instance ToJSON (ApiT AddressState) where
    toJSON = genericToJSON defaultSumTypeOptions . getApiT

instance FromJSON ApiWallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWallet where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletPassphrase where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletPassphrase where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletUtxoSnapshot where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletUtxoSnapshot where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletUtxoSnapshotEntry where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletUtxoSnapshotEntry where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletPostData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiAccountPublicKey where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON ApiAccountPublicKey where
    toJSON =
        toJSON . hexText . xpubToBytes . getApiT . key

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

instance FromJSON AccountPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON AccountPostData where
    toJSON = genericToJSON defaultRecordTypeOptions

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
    Aeson.Object m -> Aeson.Object (HM.insert k v m)
    json -> json

instance MkSomeMnemonic mw => FromJSON (ByronWalletPostData mw) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON (ByronWalletPostData mw) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT (Hash "encryption")) where
    parseJSON = parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT (Hash "encryption")) where
    toJSON = toTextJSON

instance FromJSON (ApiT XPrv) where
    parseJSON = parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON (ApiT XPrv) where
    toJSON = toJSON . toText

instance FromJSON (ApiT (Hash "VerificationKey")) where
    parseJSON = fromTextJSON "VerificationKey Hash"
instance ToJSON (ApiT (Hash "VerificationKey")) where
    toJSON = toTextJSON

instance FromJSON (ApiT (Hash "TokenPolicy")) where
    parseJSON = fromTextJSON "TokenPolicy Hash"
instance ToJSON (ApiT (Hash "TokenPolicy")) where
    toJSON = toTextJSON

instance FromJSON ByronWalletFromXPrvPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ByronWalletFromXPrvPostData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPutData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON SettingsPutData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  SettingsPutData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPutPassphraseData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutPassphraseData where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance FromJSON ByronWalletPutPassphraseData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ByronWalletPutPassphraseData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiMaintenanceActionPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiMaintenanceActionPostData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiMaintenanceAction where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiMaintenanceAction where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON MaintenanceAction where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON MaintenanceAction where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON ApiTxId where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiTxId where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiFee where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiFee where
    toJSON = genericToJSON defaultRecordTypeOptions

instance (PassphraseMaxLength purpose, PassphraseMinLength purpose)
    => FromJSON (ApiT (Passphrase purpose)) where
    parseJSON = fromTextJSON "Passphrase"
instance ToJSON (ApiT (Passphrase purpose)) where
    toJSON = toTextJSON

instance FromJSON ApiCredential where
    parseJSON v =
        (CredentialScript <$> parseJSON v) <|>
        (CredentialPubKey <$> parsePubKey v)

parsePubKey :: Aeson.Value -> Aeson.Parser ByteString
parsePubKey = withText "CredentialPubKey" $ \txt ->
    case detectEncoding (T.unpack txt) of
        Just EBech32{} -> do
            (hrp, dp) <- case Bech32.decodeLenient txt of
                Left _ -> fail "CredentialPubKey's Bech32 has invalid text."
                Right res -> pure res
            let checkPayload bytes
                    | BS.length bytes /= 32 =
                          fail "CredentialPubKey must be 32 bytes."
                    | otherwise = pure bytes
            let proceedWhenHrpCorrect = case  Bech32.dataPartToBytes dp of
                    Nothing ->
                          fail "CredentialPubKey has invalid Bech32 datapart."
                    Just bytes -> checkPayload bytes
            case Bech32.humanReadablePartToText hrp of
                "stake_vk" -> proceedWhenHrpCorrect
                "addr_vk" -> proceedWhenHrpCorrect
                _ -> fail "CredentialPubKey must have either 'addr_vk' or 'stake_vk' prefix."
        _ -> fail "CredentialPubKey must be must be encoded as Bech32."

instance ToJSON ApiCredential where
    toJSON (CredentialPubKey key') = do
        let hrp = [Bech32.humanReadablePart|addr_vk|]
        String $ T.decodeUtf8 $ encode (EBech32 hrp) key'
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
             \then bech32 encoded public keys are expected to be used with possible prefixes:\
             \ 'stake_vk' or 'addr_vk', always with proper payload size."
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
    v <- o .: k
    case fromText v of
        Right bytes -> pure bytes
        Left (TextDecodingError err) -> fail err

instance ToJSON AnyAddress where
    toJSON (AnyAddress p addrType net) =
        object [ "address" .= T.decodeUtf8 (encode (EBech32 hrp) p) ]
      where
        Right hrp = Bech32.humanReadablePartFromText (prefix <> suffix)
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
    parseJSON = fromTextJSON "WalletId"
instance ToJSON (ApiT WalletId) where
    toJSON = toTextJSON

instance FromJSON (ApiT AddressPoolGap) where
    parseJSON = parseJSON >=>
        eitherToParser . bimap ShowFmt ApiT . fromText . T.pack . show @Integer
instance ToJSON (ApiT AddressPoolGap) where
    toJSON = toJSON . getAddressPoolGap . getApiT

instance FromJSON ApiWalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

data ApiByronWalletBalance = ApiByronWalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

instance FromJSON ApiByronWalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiByronWalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletAssetsBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletAssetsBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT W.TokenMap) where
    parseJSON = fmap (ApiT . W.getFlat) . parseJSON
instance ToJSON (ApiT W.TokenMap) where
    toJSON = toJSON . W.Flat . getApiT

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser
           . bimap ShowFmt ApiT
           . decodePoolIdBech32
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . encodePoolIdBech32 . getApiT

instance FromJSON ApiWalletDelegationStatus where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON ApiWalletDelegationStatus where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON ApiWalletDelegation where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletDelegation where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletDelegationNext where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletDelegationNext where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiStakePool where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiStakePool where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiStakePoolMetrics where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiStakePoolMetrics where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiStakePoolFlag where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON ApiStakePoolFlag where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON (ApiT WalletName) where
    parseJSON = fromTextJSON "WalletName"
instance ToJSON (ApiT WalletName) where
    toJSON = toTextJSON

instance FromJSON (ApiT W.Settings) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT W.Settings) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON ApiWalletPassphraseInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletPassphraseInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT SyncProgress) where
    parseJSON = fmap ApiT . genericParseJSON syncProgressOptions
instance ToJSON (ApiT SyncProgress) where
    toJSON = genericToJSON syncProgressOptions . getApiT

instance FromJSON ApiUtxoStatistics where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiUtxoStatistics where
    toJSON = genericToJSON defaultRecordTypeOptions

instance ToJSON (ApiT BoundType) where
    toJSON = genericToJSON defaultSumTypeOptions . getApiT
instance FromJSON (ApiT BoundType) where
    parseJSON = fmap ApiT . genericParseJSON defaultSumTypeOptions

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
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiSerialisedTransaction where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiSignTransactionPostData where
    parseJSON = genericParseJSON strictRecordTypeOptions
instance ToJSON ApiSignTransactionPostData where
    toJSON = genericToJSON strictRecordTypeOptions

instance DecodeAddress t => FromJSON (PostTransactionOldData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionOldData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress t => FromJSON (ApiPaymentDestination t) where
    parseJSON obj = parseAll <|> parseAddrs
      where
        parseAll = ApiPaymentAll <$> parseJSON obj
        parseAddrs = ApiPaymentAddresses <$> parseJSON obj

instance EncodeAddress t => ToJSON (ApiPaymentDestination t) where
    toJSON (ApiPaymentAddresses addrs) = toJSON addrs
    toJSON (ApiPaymentAll addrs) = toJSON addrs

instance DecodeAddress t => FromJSON (ApiConstructTransactionData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (ApiConstructTransactionData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiExternalInput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiExternalInput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiRedeemer n) where
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
instance EncodeStakeAddress n => ToJSON (ApiRedeemer n) where
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

instance (DecodeStakeAddress n, DecodeAddress n)
    => FromJSON (ApiBalanceTransactionPostData n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance (EncodeStakeAddress n, EncodeAddress n)
    => ToJSON (ApiBalanceTransactionPostData n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance ToJSON ApiValidityInterval where
    toJSON = genericToJSON defaultRecordTypeOptions
instance FromJSON ApiValidityInterval where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance (DecodeAddress t, DecodeStakeAddress t) => FromJSON (ApiConstructTransaction t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance (EncodeAddress t, EncodeStakeAddress t) => ToJSON (ApiConstructTransaction t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiMintedBurnedTransaction n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiMintedBurnedTransaction n) where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance DecodeAddress t => FromJSON (PostTransactionFeeOldData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionFeeOldData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

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
        let Aeson.Object rest = toJSON sli
        in Aeson.Object ("absolute_slot_number" .= sln <> "time" .= t <> rest)

instance FromJSON ApiSlotId where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiSlotId where
    toJSON = genericToJSON defaultRecordTypeOptions

-- Note: These custom JSON instances are for compatibility with the existing API
-- schema. At some point, we can switch to the generic instances.
-- A BlockReference is just a SlotReference with the block height included.
instance FromJSON ApiBlockReference where
    parseJSON v = do
        ApiSlotReference sln sli t <- parseJSON v
        ApiBlockReference sln sli t <$> parseJSON v
instance ToJSON ApiBlockReference where
    toJSON (ApiBlockReference sln sli t (ApiBlockInfo bh)) =
        let Aeson.Object rest = toJSON (ApiSlotReference sln sli t)
        in Aeson.Object ("height" .= bh <> rest)

instance FromJSON ApiBlockInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiBlockInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT EpochNo) where
    parseJSON = fmap (ApiT . unsafeEpochNo) . parseJSON
instance ToJSON (ApiT EpochNo) where
    toJSON (ApiT (EpochNo en)) = toJSON $ fromIntegral @Word31 @Word32 en

instance FromJSON (ApiT SlotInEpoch) where
    parseJSON = fmap (ApiT . SlotInEpoch) . parseJSON
instance ToJSON (ApiT SlotInEpoch) where
    toJSON (ApiT (SlotInEpoch sn)) = toJSON sn

instance FromJSON (ApiT SlotNo) where
    parseJSON = fmap (ApiT . SlotNo) . parseJSON
instance ToJSON (ApiT SlotNo) where
    toJSON (ApiT (SlotNo sn)) = toJSON sn

instance FromJSON a => FromJSON (AddressAmount a) where
    parseJSON = withObject "AddressAmount " $ \v ->
        prependFailure "parsing AddressAmount failed, " $
        AddressAmount
            <$> v .: "address"
            <*> (v .: "amount" >>= validateCoin)
            <*> v .:? "assets" .!= mempty
      where
        validateCoin q
            | coinIsValidForTxOut (coinFromQuantity q) = pure q
            | otherwise = fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (unCoin txOutMaxCoin) <> " lovelace."

instance ToJSON (ApiT W.TokenBundle) where
    -- TODO: consider other structures
    toJSON (ApiT (W.TokenBundle c ts)) = object
        [ "amount" .= coinToQuantity @Word c
        , "assets" .= toJSON (ApiT ts)
        ]

instance FromJSON (ApiT W.TokenBundle) where
    -- TODO: reject unknown fields
    parseJSON = withObject "Value " $ \v ->
        prependFailure "parsing Value failed, " $
        fmap ApiT $ W.TokenBundle
            <$> (v .: "amount" >>= validateCoin)
            <*> fmap getApiT (v .: "assets" .!= mempty)
      where
        validateCoin :: Quantity "lovelace" Word64 -> Aeson.Parser Coin
        validateCoin (coinFromQuantity -> c)
            | coinIsValidForTxOut c = pure c
            | otherwise = fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (unCoin txOutMaxCoin) <> " lovelace."

instance ToJSON a => ToJSON (AddressAmount a) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON a => FromJSON (AddressAmountNoAssets a) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON a => ToJSON (AddressAmountNoAssets a) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiTransaction n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiTransaction n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiWalletInput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletInput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiWalletOutput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletOutput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiExternalCertificate n) where
    parseJSON = genericParseJSON apiCertificateOptions
instance EncodeStakeAddress n => ToJSON (ApiExternalCertificate n) where
    toJSON = genericToJSON apiCertificateOptions

instance FromJSON (ApiT W.PoolOwner) where
    parseJSON = fromTextJSON "ApiT PoolOwner"
instance ToJSON (ApiT W.PoolOwner) where
    toJSON = toTextJSON

instance FromJSON (ApiT W.StakePoolMetadataUrl) where
    parseJSON = fromTextJSON "ApiT StakePoolMetadataUrl"
instance ToJSON (ApiT W.StakePoolMetadataUrl) where
    toJSON = toTextJSON

instance FromJSON (ApiT W.StakePoolMetadataHash) where
    parseJSON = fromTextJSON "ApiT StakePoolMetadataHash"
instance ToJSON (ApiT W.StakePoolMetadataHash) where
    toJSON = toTextJSON

instance FromJSON (ApiT W.NonWalletCertificate) where
  parseJSON val
    | val == object ["certificate_type" .= String "mir"]
    = pure $ ApiT MIRCertificate
    | val == object ["certificate_type" .= String "genesis"]
    = pure $ ApiT GenesisCertificate
    | otherwise
    = fail
        "expected object with key 'certificate_type' and value either 'mir' or 'genesis'"
instance ToJSON (ApiT W.NonWalletCertificate) where
    toJSON (ApiT cert) = object ["certificate_type" .= String (toText cert)]

parseExtendedAesonObject
    :: ( Generic a
       , Aeson.GFromJSON Aeson.Zero (Rep a) )
    => String
    -> Text
    -> Value
    -> Parser a
parseExtendedAesonObject txt fieldtoremove = withObject txt $ \o -> do
    let removeCertType (numTxt,_) = numTxt /= fieldtoremove
    let o' = HM.fromList $ filter removeCertType $ HM.toList o
    genericParseJSON defaultRecordTypeOptions (Object o')

extendAesonObject
    :: ( Generic a
       , Aeson.GToJSON' Value Aeson.Zero (Rep a))
    => [Aeson.Pair]
    -> a
    -> Value
extendAesonObject tobeadded apipool =
    let Object obj = genericToJSON defaultRecordTypeOptions apipool
        Object obj' = object tobeadded
    in Object $ obj <> obj'

instance FromJSON ApiRegisterPool where
    parseJSON = parseExtendedAesonObject "ApiRegisterPool" "certificate_type"
instance ToJSON ApiRegisterPool where
    toJSON = extendAesonObject ["certificate_type" .= String "register_pool"]

instance FromJSON ApiDeregisterPool where
    parseJSON = parseExtendedAesonObject "ApiDeregisterPool" "certificate_type"
instance ToJSON ApiDeregisterPool where
    toJSON = extendAesonObject ["certificate_type" .= String "deregister_pool"]

instance DecodeStakeAddress n => FromJSON (ApiAnyCertificate n) where
    parseJSON = withObject "ApiAnyCertificate" $ \o -> do
        (certType :: String) <- o .: "certificate_type"
        case certType of
            "register_pool" -> StakePoolRegister <$> parseJSON (Object o)
            "deregister_pool" -> StakePoolDeregister <$> parseJSON (Object o)
            "join_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "quit_pool" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "register_reward_account" -> WalletDelegationCertificate <$> parseJSON (Object o)
            "join_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "quit_pool_external" -> DelegationCertificate <$> parseJSON (Object o)
            "register_reward_account_external" -> DelegationCertificate <$> parseJSON (Object o)
            "mir" -> OtherCertificate <$> parseJSON (Object o)
            "genesis" -> OtherCertificate <$> parseJSON (Object o)
            _ -> fail $ "unknown certificate_type: " <> show certType
instance EncodeStakeAddress n => ToJSON (ApiAnyCertificate n) where
    toJSON (WalletDelegationCertificate cert) = toJSON cert
    toJSON (DelegationCertificate cert) = toJSON cert
    toJSON (StakePoolRegister reg) = toJSON reg
    toJSON (StakePoolDeregister dereg) = toJSON dereg
    toJSON (OtherCertificate cert) = toJSON cert

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiDecodedTransaction n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiDecodedTransaction n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiTxOutputGeneral n)
  where
    parseJSON obj = do
        derPathM <-
            (withObject "ApiTxOutputGeneral" $
             \o -> o .:? "derivation_path" :: Aeson.Parser (Maybe (NonEmpty (ApiT DerivationIndex)))) obj
        case derPathM of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser (AddressAmount (ApiT Address, Proxy n))
                pure $ ExternalOutput xs
            Just _ -> do
                xs <- parseJSON obj :: Aeson.Parser (ApiWalletOutput n)
                pure $ WalletOutput xs
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiTxOutputGeneral n)
  where
    toJSON (ExternalOutput content) = toJSON content
    toJSON (WalletOutput content) = toJSON content

instance
    ( DecodeAddress n
    , DecodeStakeAddress n
    ) => FromJSON (ApiTxInputGeneral n)
  where
    parseJSON obj = do
        derPathM <-
            (withObject "ApiTxInputGeneral" $
             \o -> o .:? "derivation_path" :: Aeson.Parser (Maybe (NonEmpty (ApiT DerivationIndex)))) obj
        case derPathM of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser (ApiT TxIn)
                pure $ ExternalInput xs
            Just _ -> do
                xs <- parseJSON obj :: Aeson.Parser (ApiWalletInput n)
                pure $ WalletInput xs
instance
    ( EncodeAddress n
    , EncodeStakeAddress n
    ) => ToJSON (ApiTxInputGeneral n)
  where
    toJSON (ExternalInput content) = toJSON content
    toJSON (WalletInput content) = toJSON content

instance FromJSON (ApiT TxMetadata) where
    parseJSON = fmap ApiT
        . either (fail . displayError) pure
        . metadataFromJson TxMetadataJsonDetailedSchema

instance ToJSON (ApiT TxMetadata) where
    toJSON = metadataToJson TxMetadataJsonDetailedSchema . getApiT

instance FromJSON ApiTxMetadata where
    parseJSON Aeson.Null = pure $ ApiTxMetadata Nothing
    parseJSON v = ApiTxMetadata . Just <$> parseJSON v
instance ToJSON ApiTxMetadata where
    toJSON (ApiTxMetadata x) = case x of
        Nothing -> Aeson.Null
        Just (ApiT md) | txMetadataIsNull md -> Aeson.Null
        Just md -> toJSON md

instance DecodeAddress n => FromJSON (ApiWalletMigrationPlanPostData n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletMigrationPlanPostData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance (DecodeAddress n, PassphraseMaxLength s, PassphraseMinLength s) =>
    FromJSON (ApiWalletMigrationPostData n s)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n =>
    ToJSON (ApiWalletMigrationPostData n s)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

instance (DecodeAddress n) => FromJSON (ApiPutAddressesData n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiPutAddressesData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiTxInput n) where
    parseJSON v = ApiTxInput <$> optional (parseJSON v) <*> parseJSON v

instance EncodeAddress n => ToJSON (ApiTxInput n) where
    toJSON (ApiTxInput s i) =
        Object (maybe mempty (fromValue . toJSON) s <> fromValue (toJSON i))
      where
        fromValue (Object o) = o
        fromValue _ = mempty

instance DecodeAddress n => FromJSON (ApiTxCollateral n) where
    parseJSON v = ApiTxCollateral <$> optional (parseJSON v) <*> parseJSON v

instance EncodeAddress n => ToJSON (ApiTxCollateral n) where
    toJSON (ApiTxCollateral s i) =
        Object (maybe mempty (fromValue . toJSON) s <> fromValue (toJSON i))
      where
        fromValue (Object o) = o
        fromValue _ = mempty

instance FromJSON (ApiT TxIn) where
    parseJSON = withObject "TxIn" $ \v -> ApiT <$>
        (TxIn <$> fmap getApiT (v .: "id") <*> v .: "index")

instance ToJSON (ApiT TxIn) where
    toJSON (ApiT (TxIn txid ix)) = object
        [ "id" .= toJSON (ApiT txid)
        , "index" .= toJSON ix ]

instance FromJSON (ApiT (Hash "Tx")) where
    parseJSON = fromTextJSON "Tx Hash"
instance ToJSON (ApiT (Hash "Tx")) where
    toJSON = toTextJSON

instance FromJSON (ApiT (Hash "Datum")) where
    parseJSON = fromTextJSON "Datum Hash"
instance ToJSON (ApiT (Hash "Datum")) where
    toJSON = toTextJSON

instance FromJSON (ApiT Direction) where
    parseJSON = fmap ApiT . genericParseJSON defaultSumTypeOptions
instance ToJSON (ApiT Direction) where
    toJSON = genericToJSON defaultSumTypeOptions . getApiT

instance FromJSON (ApiT TxStatus) where
    parseJSON = fmap ApiT . genericParseJSON defaultSumTypeOptions
instance ToJSON (ApiT TxStatus) where
    toJSON = genericToJSON defaultSumTypeOptions . getApiT

instance FromJSON ApiNetworkInformation where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNetworkInformation where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON NtpSyncingStatus where
    parseJSON =
        parseJSON >=> eitherToParser . first ShowFmt . fromText
instance ToJSON NtpSyncingStatus where
    toJSON = toJSON . toText

instance FromJSON ApiNtpStatus where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNtpStatus where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiNetworkClock where
    parseJSON = parseJSON >=> pure . ApiNetworkClock
instance ToJSON ApiNetworkClock where
    toJSON (ApiNetworkClock st) = toJSON st

instance FromJSON (ApiT StakePoolMetadata) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT StakePoolMetadata) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

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
    parseJSON = fromTextJSON "Genesis Hash"
instance ToJSON (ApiT (Hash "Genesis")) where
    toJSON = toTextJSON

instance FromJSON ApiEraInfo where
    parseJSON = genericParseJSON explicitNothingRecordTypeOptions
instance ToJSON ApiEraInfo where
    toJSON = genericToJSON explicitNothingRecordTypeOptions

instance FromJSON ApiNetworkParameters where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNetworkParameters where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletSignData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletSignData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiWithdrawal n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeStakeAddress n => ToJSON (ApiWithdrawal n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeStakeAddress n => FromJSON (ApiWithdrawalGeneral n) where
    parseJSON obj = do
        myResource <-
            (withObject "ApiWithdrawalGeneral" $
             \o -> o .:? "context" :: Aeson.Parser (Maybe Text)) obj
        case myResource of
            Nothing -> do
                (ApiWithdrawal addr amt)  <- parseJSON obj :: Aeson.Parser (ApiWithdrawal n)
                pure $ ApiWithdrawalGeneral addr amt External
            _ -> do
                (ApiWithdrawal addr amt)  <- parseJSON obj :: Aeson.Parser (ApiWithdrawal n)
                pure $ ApiWithdrawalGeneral addr amt Our
instance EncodeStakeAddress n => ToJSON (ApiWithdrawalGeneral n) where
    toJSON (ApiWithdrawalGeneral addr amt ctx) = do
        let obj = [ "stake_address" .= toJSON addr
                  , "amount" .= toJSON amt]
        case ctx of
            External -> object obj
            Our -> object $ obj ++ ["context" .= String "ours"]

instance {-# OVERLAPS #-} (DecodeStakeAddress n)
    => FromJSON (ApiT W.RewardAccount, Proxy n)
  where
    parseJSON x = do
        let proxy = Proxy @n
        acct <- parseJSON x >>= eitherToParser
            . bimap ShowFmt ApiT
            . decodeStakeAddress @n
        return (acct, proxy)

instance {-# OVERLAPS #-} EncodeStakeAddress n
    => ToJSON (ApiT W.RewardAccount, Proxy n)
  where
    toJSON (acct, _) = toJSON . encodeStakeAddress @n . getApiT $ acct

instance ToJSON XPubOrSelf where
    toJSON (SomeAccountKey xpub) =
        String $ T.decodeUtf8 $ encode EBase16 $ xpubToBytes xpub
    toJSON Self = "self"

instance FromJSON XPubOrSelf where
    parseJSON t = parseXPub t <|> parseSelf t
      where
        parseXPub = withText "XPub" $ \txt ->
            case fromBase16 (T.encodeUtf8 txt) of
                Left err -> fail err
                Right hex' -> case xpubFromBytes hex' of
                    Nothing -> fail "Extended public key cannot be retrieved from a given hex bytestring"
                    Just validXPub -> pure $ SomeAccountKey validXPub
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
            case HM.toList o of
                [] -> fail "Cosigners object array should not be empty"
                cs -> for (reverse cs) $ \(numTxt, str) -> do
                    cosigner' <- parseJSON @Cosigner (String numTxt)
                    xpubOrSelf <- parseJSON str
                    pure (cosigner', xpubOrSelf)

instance ToJSON ApiScriptTemplateEntry where
    toJSON (ApiScriptTemplateEntry cosigners' template') =
        object [ "cosigners" .= object (fmap toPair (Map.toList cosigners'))
               , "template" .= toJSON template']
      where
        cosignerToText (Cosigner ix) = "cosigner#"<> T.pack (show ix)
        toPair (cosigner', xpubOrSelf) =
            ( cosignerToText cosigner'
            , toJSON xpubOrSelf  )

instance FromJSON ApiSharedWalletPostDataFromAccountPubX where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiSharedWalletPostDataFromAccountPubX where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiSharedWalletPostDataFromMnemonics where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiSharedWalletPostDataFromMnemonics where
    toJSON = genericToJSON defaultRecordTypeOptions

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
        case HM.toList o of
                [] -> fail "ApiSharedWalletPatchData should not be empty"
                [(numTxt, str)] -> do
                    cosigner' <- parseJSON @(ApiT Cosigner) (String numTxt)
                    xpub <- parseJSON @ApiAccountPublicKey str
                    pure $ ApiSharedWalletPatchData cosigner' xpub
                _ -> fail "ApiSharedWalletPatchData should have one pair"

instance ToJSON ApiSharedWalletPatchData where
    toJSON (ApiSharedWalletPatchData cosigner accXPub) =
        object [ toText cosigner .= toJSON accXPub ]

instance FromJSON ApiActiveSharedWallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiActiveSharedWallet where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiPendingSharedWallet where
    parseJSON val = case val of
        Aeson.Object obj -> do
            let obj' = HM.delete "state" obj
            genericParseJSON defaultRecordTypeOptions (Aeson.Object obj')
        _ -> fail "ApiPendingSharedWallet should be object"
instance ToJSON ApiPendingSharedWallet where
    toJSON wal = Aeson.Object $ HM.insert "state" (object ["status" .= String "incomplete"]) obj
      where
        (Aeson.Object obj) = genericToJSON defaultRecordTypeOptions wal

instance FromJSON ApiSharedWallet where
    parseJSON obj = do
        balance <-
            (withObject "ActiveSharedWallet" $
             \o -> o .:? "balance" :: Aeson.Parser (Maybe ApiWalletBalance)) obj
        case balance of
            Nothing -> do
                xs <- parseJSON obj :: Aeson.Parser ApiPendingSharedWallet
                pure $ ApiSharedWallet $ Left xs
            _ -> do
                xs <- parseJSON obj :: Aeson.Parser ApiActiveSharedWallet
                pure $ ApiSharedWallet $ Right xs

instance ToJSON ApiSharedWallet where
    toJSON (ApiSharedWallet (Left c))= toJSON c
    toJSON (ApiSharedWallet (Right c))= toJSON c

instance ToJSON ApiErrorCode where
    toJSON = genericToJSON defaultSumTypeOptions

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

instance FromJSON ApiByronWallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiByronWallet where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiWalletMigrationBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletMigrationBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

instance (DecodeStakeAddress n, DecodeAddress n) =>
    FromJSON (ApiWalletMigrationPlan n)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance (EncodeStakeAddress n, EncodeAddress n) =>
    ToJSON (ApiWalletMigrationPlan n)
  where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiPostRandomAddressData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiPostRandomAddressData where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance FromText a => FromHttpApiData (ApiT a) where
    parseUrlPiece = bimap pretty ApiT . fromText
instance ToText a => ToHttpApiData (ApiT a) where
    toUrlPiece = toText . getApiT

instance MimeRender OctetStream ApiSerialisedTransaction where
   mimeRender ct = mimeRender ct . view #transaction

instance FromHttpApiData ApiTxId where
    parseUrlPiece txt = case fromText txt of
        Left (TextDecodingError err) -> Left $ T.pack err
        Right tid -> Right $ ApiTxId $ ApiT tid

instance ToHttpApiData ApiTxId where
    toUrlPiece (ApiTxId (ApiT tid)) = toText tid

instance FromHttpApiData ApiPoolId where
    parseUrlPiece t
        | t == "*" =
            Right ApiPoolIdPlaceholder
        | otherwise =
            ApiPoolId <$> case fromText t of
                Left _ ->
                    left (T.pack . show . ShowFmt) $ decodePoolIdBech32 t
                Right r ->
                    Right r

instance ToHttpApiData ApiPoolId where
    toUrlPiece = \case
        ApiPoolIdPlaceholder -> "*"
        ApiPoolId pid -> encodePoolIdBech32 pid

instance FromHttpApiData ApiAddressInspectData where
    parseUrlPiece = pure . ApiAddressInspectData

instance ToHttpApiData ApiAddressInspectData where
    toUrlPiece = unApiAddressInspectData

{-------------------------------------------------------------------------------
                                Aeson Options
-------------------------------------------------------------------------------}

data TaggedObjectOptions = TaggedObjectOptions
    { _tagFieldName :: String
    , _contentsFieldName :: String
    }

defaultSumTypeOptions :: Aeson.Options
defaultSumTypeOptions = Aeson.defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , tagSingleConstructors = True
    }

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }

strictRecordTypeOptions :: Aeson.Options
strictRecordTypeOptions = defaultRecordTypeOptions
    { rejectUnknownFields = True
    }

taggedSumTypeOptions :: Aeson.Options -> TaggedObjectOptions -> Aeson.Options
taggedSumTypeOptions base opts = base
    { sumEncoding = TaggedObject (_tagFieldName opts) (_contentsFieldName opts)
    }

explicitNothingRecordTypeOptions :: Aeson.Options
explicitNothingRecordTypeOptions = defaultRecordTypeOptions
    { omitNothingFields = False
    }

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure

hexText :: ByteString -> Text
hexText = T.decodeLatin1 . hex

fromHexText :: Text -> Either String ByteString
fromHexText = fromHex . T.encodeUtf8

toTextJSON :: ToText a => ApiT a -> Value
toTextJSON = toJSON . toText . getApiT

fromTextJSON :: FromText a => String -> Value -> Aeson.Parser (ApiT a)
fromTextJSON n = withText n (eitherToParser . bimap ShowFmt ApiT . fromText)

{-------------------------------------------------------------------------------
                          User-Facing Address Encoding
-------------------------------------------------------------------------------}

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress (n :: NetworkDiscriminant) where
    encodeAddress :: Address -> Text

instance EncodeAddress 'Mainnet => EncodeAddress ('Staging pm) where
    encodeAddress = encodeAddress @'Mainnet

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress (n :: NetworkDiscriminant) where
    decodeAddress :: Text -> Either TextDecodingError Address

instance DecodeAddress 'Mainnet => DecodeAddress ('Staging pm) where
    decodeAddress = decodeAddress @'Mainnet

class EncodeStakeAddress (n :: NetworkDiscriminant) where
    encodeStakeAddress :: W.RewardAccount -> Text

instance EncodeStakeAddress 'Mainnet => EncodeStakeAddress ('Staging pm) where
    encodeStakeAddress = encodeStakeAddress @'Mainnet

class DecodeStakeAddress (n :: NetworkDiscriminant) where
    decodeStakeAddress :: Text -> Either TextDecodingError W.RewardAccount

instance DecodeStakeAddress 'Mainnet => DecodeStakeAddress ('Staging pm) where
    decodeStakeAddress = decodeStakeAddress @'Mainnet

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
type family ApiMintedBurnedTransactionT (n :: k) :: Type
type family PostMintBurnAssetDataT (n :: k) :: Type
type family ApiWalletMigrationPlanPostDataT (n :: k) :: Type
type family ApiWalletMigrationPostDataT (n :: k1) (s :: k2) :: Type
type family ApiPutAddressesDataT (n :: k) :: Type
type family ApiBalanceTransactionPostDataT (n :: k) :: Type
type family ApiDecodedTransactionT (n :: k) :: Type

type instance ApiAddressT (n :: NetworkDiscriminant) =
    ApiAddress n

type instance ApiStakeKeysT (n :: NetworkDiscriminant) =
    ApiStakeKeys n

type instance ApiPutAddressesDataT (n :: NetworkDiscriminant) =
    ApiPutAddressesData n

type instance ApiAddressIdT (n :: NetworkDiscriminant) =
    (ApiT Address, Proxy n)

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

type instance PostMintBurnAssetDataT (n :: NetworkDiscriminant) =
    PostMintBurnAssetData n

type instance ApiWalletMigrationPlanPostDataT (n :: NetworkDiscriminant) =
    ApiWalletMigrationPlanPostData n

type instance ApiWalletMigrationPostDataT (n :: NetworkDiscriminant) (s :: Symbol) =
    ApiWalletMigrationPostData n s

type instance ApiMintedBurnedTransactionT (n :: NetworkDiscriminant) =
    ApiMintedBurnedTransaction n

type instance ApiBalanceTransactionPostDataT (n :: NetworkDiscriminant) =
    ApiBalanceTransactionPostData n

type instance ApiDecodedTransactionT (n :: NetworkDiscriminant) =
    ApiDecodedTransaction n

{-------------------------------------------------------------------------------
                         SMASH interfacing types
-------------------------------------------------------------------------------}

-- | Parses the SMASH HealthCheck type from the SMASH API.
data HealthStatusSMASH = HealthStatusSMASH
    { status :: Text
    , version :: Text
    } deriving (Generic, Show, Eq, Ord)

instance FromJSON HealthStatusSMASH where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON HealthStatusSMASH where
    toJSON = genericToJSON defaultRecordTypeOptions

-- | Dscribes the health status of the SMASH server.
data HealthCheckSMASH =
      Available          -- server available
    | Unavailable        -- server reachable, but unavailable
    | Unreachable        -- could not get a response from the SMASH server
    | NoSmashConfigured  -- no SMASH server has been configured
    deriving (Generic, Show, Eq, Ord)

newtype ApiHealthCheck = ApiHealthCheck
    { health :: HealthCheckSMASH }
    deriving (Generic, Eq, Ord)
    deriving Show via (Quiet ApiHealthCheck)

instance FromJSON HealthCheckSMASH where
    parseJSON = genericParseJSON defaultSumTypeOptions
        { sumEncoding = UntaggedValue }
instance ToJSON HealthCheckSMASH where
    toJSON = genericToJSON defaultSumTypeOptions

instance FromJSON ApiHealthCheck where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiHealthCheck where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT SmashServer) where
    parseJSON = fromTextJSON "SmashServer"
instance ToJSON (ApiT SmashServer) where
    toJSON = toTextJSON

{-------------------------------------------------------------------------------
                         Token minting types
-------------------------------------------------------------------------------}

-- | Data required when submitting a mint/burn transaction. Cardano implements
-- minting and burning using transactions, so some of these fields are shared
-- with @PostTransactionData@.
data PostMintBurnAssetData (n :: NetworkDiscriminant) = PostMintBurnAssetData
    { mintBurn   :: !(NonEmpty (ApiMintBurnData n))
    -- ^ Minting and burning requests.
    , passphrase :: !(ApiT (Passphrase "lenient"))
    -- ^ Passphrase of the wallet.
    , metadata   :: !(Maybe (ApiT TxMetadata))
    -- ^ Metadata to attach to the transaction that mints/burns.
    } deriving (Eq, Generic, Show)

instance DecodeAddress n => FromJSON (PostMintBurnAssetData n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance EncodeAddress n => ToJSON (PostMintBurnAssetData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

-- | Core minting and burning request information.
--
-- Assets are minted and burned under a "policy". The policy defines under what
-- circumstances a token may be minted and burned. The policy is the hash of a serialized
-- script that contain verification keys and timelocks combined in a conditions, possibly nested,
-- to accommodate non-trivial time conditions.
-- In non-multisig case the script regulating minting/burning will contain
-- a verification key of the wallet with optional time predicates. The verification key
-- can be specified by a user as an option. It is key index derived on top of current account public key
-- using "0" purpose. Otherwise the default index (ie. the first one, with ix=0) is taken.
-- In multisig case the script regulating minting/burning will contain verification keys of
-- signers with optional time predicates. The used key derivation index is the same for all
-- engaged derivation keys. If not specified then ix=0 is assumed to be used.
data ApiMintBurnData (n :: NetworkDiscriminant) = ApiMintBurnData
    { verificationKeyIndex :: !(Maybe (ApiT DerivationIndex))
    -- ^ The key derivation index to use for verification key derivation in a script.
    , policyScriptTemplate :: !(ApiT (Script Cosigner))
    -- ^ A script regulating minting/burning policy. For non-multisig only 'cosigner#0' is expected
    -- in place of verification key. Only one cosigner should be present.
    , assetName            :: !(ApiT W.TokenName)
    -- ^ The name of the asset to mint/burn.
    , operation            :: !(ApiMintBurnOperation n)
    -- ^ The minting or burning operation to perform.
    } deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance DecodeAddress n => FromJSON (ApiMintBurnData n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance EncodeAddress n => ToJSON (ApiMintBurnData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

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
    { receivingAddress :: (ApiT Address, Proxy n)
    -- ^ Address that receives the minted assets.
    , amount           :: Quantity "assets" Natural
    -- ^ Amount of assets to mint.
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance DecodeAddress n => FromJSON (ApiMintData n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance EncodeAddress n => ToJSON (ApiMintData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

-- | The format of a burn request: burn "amount". The user can only specify the
-- type of tokens to burn (policyId, assetName), and the amount, the exact
-- tokens selected are up to the implementation.
newtype ApiBurnData = ApiBurnData (Quantity "assets" Natural)
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance FromJSON ApiBurnData where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance ToJSON ApiBurnData where
    toJSON (burn) = genericToJSON defaultRecordTypeOptions burn

instance EncodeAddress n => ToJSON (ApiMintBurnOperation n) where
    toJSON = object . pure . \case
        ApiMint mint -> "mint" .= mint
        ApiBurn burn -> "burn" .= burn

instance DecodeAddress n => FromJSON (ApiMintBurnOperation n) where
    parseJSON = Aeson.withObject "ApiMintBurnOperation" $ \o ->
        case HM.keys o of
            ["mint"] -> ApiMint <$> o .: "mint"
            ["burn"] -> ApiBurn <$> o .: "burn"
            [] -> fail "Must include a \"mint\" or \"burn\" property."
            _ -> fail "May be either a \"mint\" or a \"burn\"."

instance FromJSON ApiMintedBurnedInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions

instance ToJSON ApiMintedBurnedInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT ApiMintedBurnedInfo) where
    parseJSON = fmap ApiT . parseJSON
instance ToJSON (ApiT ApiMintedBurnedInfo) where
    toJSON = toJSON . getApiT

instance FromJSON (ApiT (Script KeyHash)) where
    parseJSON = fmap ApiT . parseJSON
instance ToJSON (ApiT (Script KeyHash)) where
    toJSON = toJSON . getApiT

instance FromJSON (ApiT (Script Cosigner)) where
    parseJSON = fmap ApiT . parseJSON
instance ToJSON (ApiT (Script Cosigner)) where
    toJSON = toJSON . getApiT

instance FromJSON (ApiT TxScriptValidity) where
    parseJSON = fmap ApiT . genericParseJSON Aeson.defaultOptions
        { constructorTagModifier = camelTo2 '_' . drop 8 }

instance ToJSON (ApiT TxScriptValidity) where
    toJSON = genericToJSON Aeson.defaultOptions
        { constructorTagModifier = camelTo2 '_' . drop 8 } . getApiT
