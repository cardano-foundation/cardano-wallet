{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
    , ApiAddress (..)
    , ApiCredential (..)
    , ApiAddressData (..)
    , AnyAddress (..)
    , AnyAddressType (..)
    , ApiCertificate (..)
    , ApiDelegationAction (..)
    , ApiEpochInfo (..)
    , toApiEpochInfo
    , ApiSelectCoinsData (..)
    , ApiSelectCoinsPayments (..)
    , ApiSelectCoinsAction (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionChange (..)
    , ApiCoinSelectionInput (..)
    , ApiCoinSelectionOutput (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiStakePoolFlag (..)
    , ApiWallet (..)
    , ApiWalletBalance (..)
    , ApiWalletAssetsBalance (..)
    , ApiWalletPassphrase (..)
    , ApiWalletPassphraseInfo (..)
    , ApiUtxoStatistics (..)
    , toApiUtxoStatistics
    , WalletPostData (..)
    , WalletPutData (..)
    , SettingsPutData (..)
    , WalletPutPassphraseData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , PostExternalTransactionData (..)
    , ApiTransaction (..)
    , ApiWithdrawalPostData (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , MaintenanceAction (..)
    , ApiFee (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiTxMetadata (..)
    , AddressAmount (..)
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
    , Iso8601Time (..)
    , MinWithdrawal (..)
    , ApiNetworkParameters (..)
    , toApiNetworkParameters
    , ApiEraInfo (..)
    , ApiWalletDelegation (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletDelegationNext (..)
    , ApiPoolId (..)
    , ApiWalletMigrationPostData (..)
    , ApiWalletMigrationInfo (..)
    , ApiWithdrawal (..)
    , ApiWalletSignData (..)
    , ApiVerificationKey (..)
    , ApiAccountKey (..)
    , ApiPostAccountKeyData (..)

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

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)

    -- * Type families
    , ApiAddressT
    , ApiPutAddressesDataT
    , ApiAddressIdT
    , ApiCoinSelectionT
    , ApiSelectCoinsDataT
    , ApiTransactionT
    , PostTransactionDataT
    , PostTransactionFeeDataT
    , ApiWalletMigrationPostDataT

    -- * API Type Conversions
    , coinToQuantity
    , coinFromQuantity

    -- * Others
    , defaultRecordTypeOptions
    , HealthStatusSMASH (..)
    , HealthCheckSMASH (..)
    , ApiHealthCheck (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub, xpubFromBytes, xpubToBytes )
import Cardano.Address.Script
    ( Script )
import Cardano.Api.Typed
    ( TxMetadataJsonSchema (..)
    , displayError
    , metadataFromJson
    , metadataToJson
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
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , Role (..)
    , fromHex
    , hex
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, SeqState, getAddressPoolGap )
import Cardano.Wallet.Primitive.Slotting
    ( Qry, timeOfEpoch )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , DecentralizationLevel (..)
    , EpochLength (..)
    , EpochNo (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , PoolId (..)
    , PoolMetadataGCStatus (..)
    , ShowFmt (..)
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
    ( Coin (..), isValidCoin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), TxIn (..), TxMetadata, TxStatus (..), txMetadataIsNull )
import Cardano.Wallet.Primitive.Types.UTxO
    ( BoundType, HistogramBar (..), UTxOStatistics (..) )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Codec.Binary.Bech32.TH
    ( humanReadablePart )
import "cardano-addresses" Codec.Binary.Encoding
    ( AbstractEncoding (..), detectEncoding, encode )
import Control.Applicative
    ( optional, (<|>) )
import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( guard, (>=>) )
import Data.Aeson.Types
    ( FromJSON (..)
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
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
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
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word16, Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Nat, Symbol )
import Numeric.Natural
    ( Natural )
import Servant.API
    ( MimeRender (..), MimeUnrender (..), OctetStream )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDerivation as AD
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                               Styles of Wallets
-------------------------------------------------------------------------------}

data WalletStyle
    = Shelley
    | Byron

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
    } deriving (Eq, Generic, Show)

newtype ApiMaintenanceAction = ApiMaintenanceAction
    { gcStakePools :: ApiT PoolMetadataGCStatus
    } deriving (Eq, Generic, Show)

data ApiAsset = ApiAsset
    { policyId :: ApiT W.TokenPolicyId
    , assetName :: ApiT W.TokenName
    , metadata :: Maybe (ApiT W.AssetMetadata)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

toApiAsset :: Maybe W.AssetMetadata -> W.AssetId -> ApiAsset
toApiAsset metadata_ (W.AssetId policyId_ assetName_) = ApiAsset
    { policyId = ApiT policyId_
    , assetName = ApiT assetName_
    , metadata = ApiT <$> metadata_
    }

data ApiAddress (n :: NetworkDiscriminant) = ApiAddress
    { id :: !(ApiT Address, Proxy n)
    , state :: !(ApiT AddressState)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiCredential =
      CredentialPubKey ByteString
    | CredentialScript Script
    deriving (Eq, Generic, Show)

data ApiAddressData =
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
    deriving (Eq, Generic, Show)

newtype ApiSelectCoinsPayments (n :: NetworkDiscriminant) = ApiSelectCoinsPayments
    { payments :: NonEmpty (AddressAmount (ApiT Address, Proxy n))
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiSelectCoinsAction = ApiSelectCoinsAction
    { delegationAction :: ApiDelegationAction
    } deriving (Eq, Generic, Show)

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
    { inputs :: !(NonEmpty (ApiCoinSelectionInput n))
    , outputs :: ![ApiCoinSelectionOutput n]
    , change :: ![ApiCoinSelectionChange n]
    , certificates :: Maybe (NonEmpty ApiCertificate)
    , deposits :: ![Quantity "lovelace" Natural]
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiCoinSelectionChange (n :: NetworkDiscriminant) = ApiCoinSelectionChange
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiCoinSelectionInput (n :: NetworkDiscriminant) = ApiCoinSelectionInput
    { id :: !(ApiT (Hash "Tx"))
    , index :: !Word32
    , address :: !(ApiT Address, Proxy n)
    , derivationPath :: NonEmpty (ApiT DerivationIndex)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiCoinSelectionOutput (n :: NetworkDiscriminant) = ApiCoinSelectionOutput
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Ord, Generic, Show)
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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

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
    } deriving (Eq, Generic, Show)
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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype WalletOrAccountPostData = WalletOrAccountPostData
    { postData :: Either WalletPostData AccountPostData
    } deriving (Eq, Generic, Show)

data AccountPostData = AccountPostData
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !ApiAccountPublicKey
    , addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    } deriving (Eq, Generic, Show)

newtype WalletPutData = WalletPutData
    { name :: (Maybe (ApiT WalletName))
    } deriving (Eq, Generic, Show)

newtype SettingsPutData = SettingsPutData
    { settings :: (ApiT W.Settings)
    } deriving (Eq, Generic, Show)

data WalletPutPassphraseData = WalletPutPassphraseData
    { oldPassphrase :: !(ApiT (Passphrase "raw"))
    , newPassphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

data ByronWalletPutPassphraseData = ByronWalletPutPassphraseData
    { oldPassphrase :: !(Maybe (ApiT (Passphrase "lenient")))
    , newPassphrase :: !(ApiT (Passphrase "raw"))
    } deriving (Eq, Generic, Show)

data PostTransactionData (n :: NetworkDiscriminant) = PostTransactionData
    { payments :: !(NonEmpty (AddressAmount (ApiT Address, Proxy n)))
    , passphrase :: !(ApiT (Passphrase "lenient"))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    } deriving (Eq, Generic, Show)

data PostTransactionFeeData (n :: NetworkDiscriminant) = PostTransactionFeeData
    { payments :: (NonEmpty (AddressAmount (ApiT Address, Proxy n)))
    , withdrawal :: !(Maybe ApiWithdrawalPostData)
    , metadata :: !(Maybe (ApiT TxMetadata))
    , timeToLive :: !(Maybe (Quantity "second" NominalDiffTime))
    } deriving (Eq, Generic, Show)

newtype PostExternalTransactionData = PostExternalTransactionData
    { payload :: ByteString
    } deriving (Eq, Generic, Show)

data ApiFee = ApiFee
    { estimatedMin :: !(Quantity "lovelace" Natural)
    , estimatedMax :: !(Quantity "lovelace" Natural)
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
    , eras :: !ApiEraInfo
    } deriving (Eq, Generic, Show)

data ApiEraInfo = ApiEraInfo
    { byron :: !(Maybe ApiEpochInfo)
    , shelley :: !(Maybe ApiEpochInfo)
    , allegra :: !(Maybe ApiEpochInfo)
    , mary :: !(Maybe ApiEpochInfo)
    } deriving (Eq, Generic, Show)

toApiNetworkParameters
    :: Monad m
    => NetworkParameters
    -> (EpochNo -> m ApiEpochInfo)
    -> m ApiNetworkParameters
toApiNetworkParameters (NetworkParameters gp sp pp) toEpochInfo = do
    byron <- traverse toEpochInfo (pp ^. #eras . #byron)
    shelley <- traverse toEpochInfo (pp ^. #eras . #shelley)
    allegra <- traverse toEpochInfo (pp ^. #eras . #allegra)
    mary <- traverse toEpochInfo (pp ^. #eras . #mary)

    let apiEras = ApiEraInfo { byron, shelley, allegra, mary }

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
        , minimumUtxoValue = Quantity
            $ fromIntegral
            $ unCoin
            $ view #minimumUTxOvalue pp
        , eras = apiEras
        }

newtype ApiTxId = ApiTxId
    { id :: ApiT (Hash "Tx")
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiTransaction (n :: NetworkDiscriminant) = ApiTransaction
    { id :: !(ApiT (Hash "Tx"))
    , amount :: !(Quantity "lovelace" Natural)
    , fee :: !(Quantity "lovelace" Natural)
    , deposit :: !(Quantity "lovelace" Natural)
    , insertedAt :: !(Maybe ApiBlockReference)
    , pendingSince :: !(Maybe ApiBlockReference)
    , expiresAt :: !(Maybe ApiSlotReference)
    , depth :: !(Maybe (Quantity "block" Natural))
    , direction :: !(ApiT Direction)
    , inputs :: ![ApiTxInput n]
    , outputs :: ![AddressAmount (ApiT Address, Proxy n)]
    , withdrawals :: ![ApiWithdrawal n]
    , mint :: !(ApiT W.TokenMap)
    , status :: !(ApiT TxStatus)
    , metadata :: !ApiTxMetadata
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiTxMetadata = ApiTxMetadata
    { getApiTxMetadata :: Maybe (ApiT TxMetadata)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWithdrawal n = ApiWithdrawal
    { stakeAddress :: !(ApiT W.RewardAccount, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWithdrawalPostData
    = SelfWithdrawal
    | ExternalWithdrawal (ApiMnemonicT '[15,18,21,24])
    deriving (Eq, Generic, Show)

data ApiTxInput (n :: NetworkDiscriminant) = ApiTxInput
    { source :: !(Maybe (AddressAmount (ApiT Address, Proxy n)))
    , input :: !(ApiT TxIn)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data AddressAmount addr = AddressAmount
    { address :: !addr
    , amount :: !(Quantity "lovelace" Natural)
    , assets :: !(ApiT W.TokenMap)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

coinToQuantity :: Integral n => Coin -> Quantity "lovelace" n
coinToQuantity = Quantity . fromIntegral . unCoin

coinFromQuantity :: Integral n => Quantity "lovelace" n -> Coin
coinFromQuantity = Coin . fromIntegral . getQuantity

newtype ApiAddressInspect = ApiAddressInspect
    { unApiAddressInspect :: Aeson.Value }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ApiAddressInspectData = ApiAddressInspectData
    { unApiAddressInspectData :: Text }
    deriving (Eq, Generic, Show)
    deriving newtype (IsString)
    deriving anyclass NFData

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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiEra
    = ApiByron
    | ApiShelley
    | ApiAllegra
    | ApiMary
    deriving (Show, Eq, Generic, Enum, Ord)
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
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiPostRandomAddressData = ApiPostRandomAddressData
    { passphrase :: !(ApiT (Passphrase "lenient"))
    , addressIndex :: !(Maybe (ApiT (Index 'AD.Hardened 'AddressK)))
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletMigrationPostData (n :: NetworkDiscriminant) (s :: Symbol) =
    ApiWalletMigrationPostData
    { passphrase :: !(ApiT (Passphrase s))
    , addresses :: ![(ApiT Address, Proxy n)]
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiPutAddressesData (n :: NetworkDiscriminant) = ApiPutAddressesData
    { addresses :: [(ApiT Address, Proxy n)]
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiWalletMigrationInfo = ApiWalletMigrationInfo
    { migrationCost :: Quantity "lovelace" Natural
    , leftovers :: Quantity "lovelace" Natural
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiWithdrawRewards = ApiWithdrawRewards Bool
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data ApiWalletSignData = ApiWalletSignData
    { metadata :: ApiT TxMetadata
    , passphrase :: ApiT (Passphrase "lenient")
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

newtype ApiVerificationKey = ApiVerificationKey
    { getApiVerificationKey :: (ByteString, Role)
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiPostAccountKeyData = ApiPostAccountKeyData
    { passphrase :: ApiT (Passphrase "raw")
    , extended :: Bool
    } deriving (Eq, Generic, Show)
      deriving anyclass NFData

data ApiAccountKey = ApiAccountKey
    { getApiAccountKey :: ByteString
    , extended :: Bool
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
    | TransactionIsTooBig
    | InputsDepleted
    | CannotCoverFee
    | InvalidCoinSelection
    | NetworkUnreachable
    | NetworkMisconfigured
    | NetworkQueryFailed
    | CreatedInvalidTransaction
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
    deriving (Eq, Generic, Show, Data, Typeable)
    deriving anyclass NFData

-- | Defines a point in time that can be formatted as and parsed from an
--   ISO 8601-compliant string.
--
newtype Iso8601Time = Iso8601Time
    { getIso8601Time :: UTCTime
    } deriving (Eq, Ord, Show)

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
    } deriving (Show)

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
    deriving (Generic, Show, Eq, Functor)
    deriving newtype (Semigroup, Monoid)
    deriving anyclass NFData
deriving instance Ord a => Ord (ApiT a)

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
    deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                               JSON Instances
-------------------------------------------------------------------------------}

instance DecodeAddress n => FromJSON (ApiAddress n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiAddress n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiAsset where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiAsset where
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

instance FromJSON (ApiT W.AssetMetadata) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT W.AssetMetadata) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance ToJSON (ApiT DerivationIndex) where
    toJSON = toTextJSON
instance FromJSON (ApiT DerivationIndex) where
    parseJSON = fromTextJSON "DerivationIndex"

instance ToJSON ApiVerificationKey where
    toJSON (ApiVerificationKey (pub, role_)) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = case role_ of
            UtxoInternal -> [humanReadablePart|addr_vk|]
            UtxoExternal -> [humanReadablePart|addr_vk|]
            MutableAccount -> [humanReadablePart|stake_vk|]
            MultisigScript -> [humanReadablePart|script_vk|]

instance FromJSON ApiVerificationKey where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= parseBech32
        fmap ApiVerificationKey . (,)
            <$> parsePub bytes
            <*> parseRole hrp
      where
        parseBech32 =
            either (const $ fail errBech32) parseDataPart . Bech32.decodeLenient
          where
            errBech32 =
                "Malformed verification key. Expected a bech32-encoded key."

        parseDataPart =
            maybe (fail errDataPart) pure . traverse dataPartToBytes
          where
            errDataPart =
                "Couldn't decode data-part to valid UTF-8 bytes."

        parseRole = \case
            hrp | hrp == [humanReadablePart|addr_vk|] -> pure UtxoExternal
            hrp | hrp == [humanReadablePart|stake_vk|] -> pure MutableAccount
            hrp | hrp == [humanReadablePart|script_vk|] -> pure MultisigScript
            _ -> fail errRole
          where
            errRole =
                "Unrecognized human-readable part. Expected one of:\
                \ \"addr_vk\", \"stake_vk\" or \"script_vk\"."

        parsePub bytes
            | BS.length bytes == 32 =
                pure bytes
            | otherwise =
                fail "Not a valid Ed25519 public key. Must be 32 bytes, without chain code"

instance ToJSON ApiAccountKey where
    toJSON (ApiAccountKey pub extd) =
        toJSON $ Bech32.encodeLenient hrp $ dataPartFromBytes pub
      where
        hrp = if extd then [humanReadablePart|acct_xvk|]
            else [humanReadablePart|acct_vk|]

instance FromJSON ApiAccountKey where
    parseJSON value = do
        (hrp, bytes) <- parseJSON value >>= parseBech32
        extended' <- parseHrp hrp
        flip ApiAccountKey extended' <$> parsePub bytes extended'
      where
        parseBech32 =
            either (const $ fail errBech32) parseDataPart . Bech32.decodeLenient
          where
            errBech32 =
                "Malformed extended/normal account public key. Expected a bech32-encoded key."

        parseHrp = \case
            hrp | hrp == [humanReadablePart|acct_xvk|] -> pure True
            hrp | hrp == [humanReadablePart|acct_vk|] -> pure False
            _ -> fail errHrp
          where
              errHrp =
                  "Unrecognized human-readable part. Expected one of:\
                  \ \"acct_xvk\" or \"acct_vk\"."

        parseDataPart =
            maybe (fail errDataPart) pure . traverse dataPartToBytes
          where
            errDataPart =
                "Couldn't decode data-part to valid UTF-8 bytes."

        bytesExpectedLength extd = if extd then 64 else 32

        parsePubErr extd =
            if extd then
                  "Not a valid Ed25519 extended public key. Must be 64 bytes, with chain code"
            else
                  "Not a valid Ed25519 normal public key. Must be 32 bytes, without chain code"

        parsePub bytes extd
            | BS.length bytes == (bytesExpectedLength extd) =
                pure bytes
            | otherwise =
                fail $ parsePubErr extd

instance FromJSON ApiPostAccountKeyData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiPostAccountKeyData where
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
        case (p, a) of
            (Just _, Just _) -> fail "Specified both payments and action, pick one"
            (Nothing, Just v) ->
                pure $ ApiSelectForDelegation $ ApiSelectCoinsAction v
            (Just v, Nothing) ->
                pure $ ApiSelectForPayment $ ApiSelectCoinsPayments v
            _ -> fail "No valid parse for ApiSelectCoinsPayments or ApiSelectCoinsAction"
instance EncodeAddress n => ToJSON (ApiSelectCoinsData n) where
    toJSON (ApiSelectForPayment v) = toJSON v
    toJSON (ApiSelectForDelegation v) = toJSON v

instance DecodeAddress n => FromJSON (ApiCoinSelection n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelection n) where
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

instance DecodeAddress n => FromJSON (ApiCoinSelectionChange n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionChange n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelectionInput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionInput n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelectionOutput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionOutput n) where
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
        fail "ApiAddressData must have at least one credential."
      where
         parseBaseAddr = withObject "AddrBase" $ \o ->
             AddrBase <$> o .: "payment" <*> o .: "stake"
         parseEnterprise = withObject "AddrEnterprise" $ \o ->
             AddrEnterprise <$> o .: "payment"
         parseRewardAccount = withObject "AddrRewardAccount" $ \o ->
             AddrRewardAccount <$> o .: "stake"

instance ToJSON ApiAddressData where
    toJSON (AddrEnterprise payment') =
        object [ "payment" .= payment']
    toJSON (AddrRewardAccount stake') =
        object [ "stake" .= stake']
    toJSON (AddrBase payment' stake') =
        object [ "payment" .= payment', "stake" .= stake']

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

instance DecodeAddress t => FromJSON (PostTransactionData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionData t) where
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

instance DecodeAddress t => FromJSON (PostTransactionFeeData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionFeeData t) where
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
            | isValidCoin (coinFromQuantity q) = pure q
            | otherwise = fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (unCoin maxBound) <> " lovelace."

instance ToJSON a => ToJSON (AddressAmount a) where
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

instance (DecodeAddress n , PassphraseMaxLength s , PassphraseMinLength s) => FromJSON (ApiWalletMigrationPostData n s)
  where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiWalletMigrationPostData n s) where
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

instance FromJSON ApiWalletMigrationInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiWalletMigrationInfo where
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

instance FromText (AddressAmount Text) where
    fromText text = do
        let err = Left . TextDecodingError $ "Parse error. Expecting format \
            \\"<amount>@<address>\" but got " <> show text
        case split (=='@') text of
            [] -> err
            [_] -> err
            [l, r] -> AddressAmount r <$> fromText l <*> pure mempty
            _ -> err

instance FromText PostExternalTransactionData where
    fromText text = case convertFromBase Base16 (T.encodeUtf8 text) of
        Left _ ->
            fail "Parse error. Expecting hex-encoded format."
        Right load ->
            pure $ PostExternalTransactionData load

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

{-------------------------------------------------------------------------------
                             HTTPApiData instances
-------------------------------------------------------------------------------}

instance FromText a => FromHttpApiData (ApiT a) where
    parseUrlPiece = bimap pretty ApiT . fromText
instance ToText a => ToHttpApiData (ApiT a) where
    toUrlPiece = toText . getApiT

instance MimeUnrender OctetStream PostExternalTransactionData where
    mimeUnrender _ =
        pure . PostExternalTransactionData . BL.toStrict

instance MimeRender OctetStream PostExternalTransactionData where
   mimeRender _ (PostExternalTransactionData val) = BL.fromStrict val

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
type family ApiAddressT (n :: k) :: *
type family ApiAddressIdT (n :: k) :: *
type family ApiCoinSelectionT (n :: k) :: *
type family ApiSelectCoinsDataT (n :: k) :: *
type family ApiTransactionT (n :: k) :: *
type family PostTransactionDataT (n :: k) :: *
type family PostTransactionFeeDataT (n :: k) :: *
type family ApiWalletMigrationPostDataT (n :: k1) (s :: k2) :: *
type family ApiPutAddressesDataT (n :: k) :: *

type instance ApiAddressT (n :: NetworkDiscriminant) =
    ApiAddress n

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

type instance PostTransactionDataT (n :: NetworkDiscriminant) =
    PostTransactionData n

type instance PostTransactionFeeDataT (n :: NetworkDiscriminant) =
    PostTransactionFeeData n

type instance ApiWalletMigrationPostDataT (n :: NetworkDiscriminant) (s :: Symbol) =
    ApiWalletMigrationPostData n s


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
    deriving (Generic, Show, Eq, Ord)

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
