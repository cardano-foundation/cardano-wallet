{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
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

    -- * API Types
    , ApiAddress (..)
    , ApiEpochInfo (..)
    , ApiSelectCoinsData (..)
    , ApiCoinSelection (..)
    , ApiCoinSelectionInput (..)
    , ApiStakePool (..)
    , ApiStakePoolMetrics (..)
    , ApiWallet (..)
    , ApiWalletPassphrase (..)
    , ApiUtxoStatistics (..)
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , PostExternalTransactionData (..)
    , ApiTimeReference (..)
    , ApiTransaction (..)
    , ApiFee (..)
    , ApiTxId (..)
    , ApiTxInput (..)
    , AddressAmount (..)
    , ApiErrorCode (..)
    , ApiNetworkInformation (..)
    , ApiNtpStatus (..)
    , NtpSyncingStatus (..)
    , ApiNetworkClock (..)
    , ApiBlockReference (..)
    , ApiNetworkTip (..)
    , Iso8601Time (..)
    , ApiEpochNumber (..)
    , ApiNetworkParameters (..)
    , toApiNetworkParameters
    , ApiWalletDelegation (..)
    , ApiWalletDelegationStatus (..)
    , ApiWalletDelegationNext (..)
    , ApiPoolId (..)

    -- * API Types (Byron)
    , ApiByronWallet (..)
    , ApiByronWalletBalance (..)
    , ApiByronWalletMigrationInfo (..)
    , ByronWalletPostData (..)
    , SomeByronWalletPostData (..)

    -- * API Types (Hardware)
    , AccountPostData (..)
    , AccountPublicKey (..)
    , WalletOrAccountPostData (..)

    -- * User-Facing Address Encoding/Decoding
    , EncodeAddress (..)
    , DecodeAddress (..)

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)
    ) where

import Prelude

import Cardano.Pool.Metadata
    ( StakePoolMetadata )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , FromMnemonic (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PersistPublicKey (..)
    , SomeMnemonic (..)
    , XPub
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( decodeLegacyAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), decodeShelleyAddress, xpubFromText )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
    ( mnemonicToText )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Address (..)
    , AddressState (..)
    , BlockchainParameters (..)
    , BoundType
    , Coin (..)
    , Direction (..)
    , EpochLength (..)
    , EpochNo (..)
    , Hash (..)
    , PoolId (..)
    , ShowFmt (..)
    , SlotLength (..)
    , SlotNo (..)
    , StartTime (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxStatus (..)
    , WalletBalance (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , isValidCoin
    , unsafeEpochNo
    )
import Codec.Binary.Bech32
    ( dataPartFromBytes, dataPartToBytes )
import Control.Applicative
    ( optional )
import Control.Arrow
    ( left )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..)
    , SumEncoding (..)
    , ToJSON (..)
    , Value (Object)
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
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Either.Extra
    ( maybeToEither )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage, Quantity (..) )
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
import Data.Time.Text
    ( iso8601, iso8601ExtendedUtc, utcTimeFromText, utcTimeToText )
import Data.Word
    ( Word32, Word64 )
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

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

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

type instance AllowedMnemonics 'Random    = '[12]
type instance AllowedMnemonics 'Icarus    = '[15]
type instance AllowedMnemonics 'Trezor    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Ledger    = '[12,15,18,21,24]
type instance AllowedMnemonics 'Shelley   = '[15,18,21,24]
type instance AllowedMnemonics 'SndFactor = '[9,12]

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data ApiAddress (n :: NetworkDiscriminant) = ApiAddress
    { id :: !(ApiT Address, Proxy n)
    , state :: !(ApiT AddressState)
    } deriving (Eq, Generic, Show)

data ApiEpochInfo = ApiEpochInfo
    { epochNumber :: !(ApiT EpochNo)
    , epochStartTime :: !UTCTime
    } deriving (Eq, Generic, Show)

newtype ApiSelectCoinsData (n :: NetworkDiscriminant) = ApiSelectCoinsData
    { payments :: NonEmpty (AddressAmount n)
    } deriving (Eq, Generic, Show)

data ApiCoinSelection (n :: NetworkDiscriminant) = ApiCoinSelection
    { inputs :: !(NonEmpty (ApiCoinSelectionInput n))
    , outputs :: !(NonEmpty (AddressAmount n))
    } deriving (Eq, Generic, Show)

data ApiCoinSelectionInput (n :: NetworkDiscriminant) = ApiCoinSelectionInput
    { id :: !(ApiT (Hash "Tx"))
    , index :: !Word32
    , address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

data ApiWallet = ApiWallet
    { id :: !(ApiT WalletId)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , balance :: !(ApiT WalletBalance)
    , delegation :: !ApiWalletDelegation
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe (ApiT WalletPassphraseInfo))
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    } deriving (Eq, Generic, Show)

data ApiWalletDelegation = ApiWalletDelegation
    { active :: !ApiWalletDelegationNext
    , next :: ![ApiWalletDelegationNext]
    } deriving (Eq, Generic, Show)

data ApiWalletDelegationNext = ApiWalletDelegationNext
    { status :: !ApiWalletDelegationStatus
    , target :: !(Maybe (ApiT PoolId))
    , changesAt :: !(Maybe ApiEpochInfo)
    } deriving (Eq, Generic, Show)

data ApiWalletDelegationStatus
    = NotDelegating
    | Delegating
    deriving (Eq, Generic, Show)

newtype ApiWalletPassphrase = ApiWalletPassphrase
    { passphrase :: ApiT (Passphrase "encryption")
    } deriving (Eq, Generic, Show)

data ApiStakePool = ApiStakePool
    { id :: !(ApiT PoolId)
    , metrics :: !ApiStakePoolMetrics
    , apparentPerformance :: !Double
    , metadata :: !(Maybe StakePoolMetadata)
    , cost :: !(Quantity "lovelace" Natural)
    , margin :: !(Quantity "percent" Percentage)
    , desirability :: !Double
    , saturation :: !Double
    } deriving (Eq, Generic, Show)

data ApiStakePoolMetrics = ApiStakePoolMetrics
    { controlledStake :: !(Quantity "lovelace" Natural)
    , producedBlocks :: !(Quantity "block" Natural)
    } deriving (Eq, Generic, Show)

data ApiUtxoStatistics = ApiUtxoStatistics
    { total :: !(Quantity "lovelace" Natural)
    , scale :: !(ApiT BoundType)
    , distribution :: !(Map Word64 Word64)
    } deriving (Eq, Generic, Show)

data WalletPostData = WalletPostData
    { addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    , mnemonicSentence :: !(ApiMnemonicT (AllowedMnemonics 'Shelley))
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT (AllowedMnemonics 'SndFactor)))
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

data SomeByronWalletPostData
    = SomeRandomWallet (ByronWalletPostData (AllowedMnemonics 'Random))
    | SomeIcarusWallet (ByronWalletPostData (AllowedMnemonics 'Icarus))
    | SomeTrezorWallet (ByronWalletPostData (AllowedMnemonics 'Trezor))
    | SomeLedgerWallet (ByronWalletPostData (AllowedMnemonics 'Ledger))
    deriving (Eq, Generic, Show)

data ByronWalletPostData mw = ByronWalletPostData
    { mnemonicSentence :: !(ApiMnemonicT mw)
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

newtype AccountPublicKey = AccountPublicKey
    { key :: (ApiT (ShelleyKey 'AccountK XPub))
    } deriving (Eq, Generic, Show)

newtype WalletOrAccountPostData = WalletOrAccountPostData
    { postData :: Either WalletPostData AccountPostData
    } deriving (Eq, Generic, Show)

data AccountPostData = AccountPostData
    { name :: !(ApiT WalletName)
    , accountPublicKey :: !AccountPublicKey
    , addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    } deriving (Eq, Generic, Show)

newtype WalletPutData = WalletPutData
    { name :: (Maybe (ApiT WalletName))
    } deriving (Eq, Generic, Show)

data WalletPutPassphraseData = WalletPutPassphraseData
    { oldPassphrase :: !(ApiT (Passphrase "encryption-old"))
    , newPassphrase :: !(ApiT (Passphrase "encryption-new"))
    } deriving (Eq, Generic, Show)

data PostTransactionData n = PostTransactionData
    { payments :: !(NonEmpty (AddressAmount n))
    , passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

newtype PostTransactionFeeData n = PostTransactionFeeData
    { payments :: (NonEmpty (AddressAmount n))
    } deriving (Eq, Generic, Show)

newtype PostExternalTransactionData = PostExternalTransactionData
    { payload :: ByteString
    } deriving (Eq, Generic, Show)

newtype ApiFee = ApiFee
    { amount :: (Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

data ApiEpochNumber =
    ApiEpochNumberLatest | ApiEpochNumber EpochNo
    deriving (Eq, Generic, Show)

data ApiNetworkParameters = ApiNetworkParameters
    { genesisBlockHash :: !(ApiT (Hash "Genesis"))
    , blockchainStartTime :: !(ApiT StartTime)
    , slotLength :: !(Quantity "second" NominalDiffTime)
    , epochLength :: !(Quantity "slot" Word32)
    , epochStability :: !(Quantity "block" Word32)
    , activeSlotCoefficient :: !(Quantity "percent" Double)
    } deriving (Eq, Generic, Show)

toApiNetworkParameters :: BlockchainParameters -> ApiNetworkParameters
toApiNetworkParameters bp = ApiNetworkParameters
    (ApiT $ getGenesisBlockHash bp)
    (ApiT $ getGenesisBlockDate bp)
    (Quantity $ unSlotLength $ getSlotLength bp)
    (Quantity $ unEpochLength $ getEpochLength bp)
    (getEpochStability bp)
    (Quantity
        $ (*100)
        $ unActiveSlotCoefficient
        $ getActiveSlotCoefficient bp)

newtype ApiTxId = ApiTxId
    { id :: ApiT (Hash "Tx")
    } deriving (Eq, Generic, Show)

data ApiTransaction n = ApiTransaction
    { id :: !(ApiT (Hash "Tx"))
    , amount :: !(Quantity "lovelace" Natural)
    , insertedAt :: !(Maybe ApiTimeReference)
    , pendingSince :: !(Maybe ApiTimeReference)
    , depth :: !(Quantity "block" Natural)
    , direction :: !(ApiT Direction)
    , inputs :: ![ApiTxInput n]
    , outputs :: ![AddressAmount n]
    , status :: !(ApiT TxStatus)
    } deriving (Eq, Generic, Show)

data ApiTxInput n = ApiTxInput
    { source :: !(Maybe (AddressAmount n))
    , input :: !(ApiT TxIn)
    } deriving (Eq, Generic, Show)

data AddressAmount (n :: NetworkDiscriminant) = AddressAmount
    { address :: !(ApiT Address, Proxy n)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

data ApiTimeReference = ApiTimeReference
    { time :: !UTCTime
    , block :: !ApiBlockReference
    } deriving (Eq, Generic, Show)

data ApiBlockReference = ApiBlockReference
    { epochNumber :: !(ApiT EpochNo)
    , slotNumber :: !(ApiT SlotNo)
    , height :: !(Quantity "block" Natural)
    } deriving (Eq, Generic, Show)

data ApiNetworkTip = ApiNetworkTip
    { epochNumber :: !(ApiT EpochNo)
    , slotNumber :: !(ApiT SlotNo)
    } deriving (Eq, Generic, Show)

data ApiNetworkInformation = ApiNetworkInformation
    { syncProgress :: !(ApiT SyncProgress)
    , nextEpoch :: !ApiEpochInfo
    , nodeTip :: !ApiBlockReference
    , networkTip :: !ApiNetworkTip
    } deriving (Eq, Generic, Show)

data NtpSyncingStatus =
      NtpSyncingStatusUnavailable
    | NtpSyncingStatusPending
    | NtpSyncingStatusAvailable
    deriving (Eq, Generic, Show)

data ApiNtpStatus = ApiNtpStatus
    { status :: !NtpSyncingStatus
    , offset :: !(Maybe (Quantity "microsecond" Integer))
    } deriving (Eq, Generic, Show)

newtype ApiNetworkClock = ApiNetworkClock
    { ntpStatus :: ApiNtpStatus
    } deriving (Eq, Generic, Show)

-- | Error codes returned by the API, in the form of snake_cased strings
data ApiErrorCode
    = NoSuchWallet
    | NoSuchTransaction
    | TransactionNotPending
    | WalletAlreadyExists
    | NoRootKey
    | WrongEncryptionPassphrase
    | MalformedTxPayload
    | KeyNotFoundForAddress
    | NotEnoughMoney
    | UtxoNotEnoughFragmented
    | TransactionIsTooBig
    | InputsDepleted
    | CannotCoverFee
    | InvalidCoinSelection
    | NetworkUnreachable
    | NetworkMisconfigured
    | NetworkTipNotFound
    | CreatedInvalidTransaction
    | RejectedByCoreNode
    | BadRequest
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | StartTimeLaterThanEndTime
    | UnsupportedMediaType
    | UnexpectedError
    | NotSynced
    | NothingToMigrate
    | NoSuchPool
    | PoolAlreadyJoined
    | NotDelegatingTo
    | InvalidRestorationParameters
    | RejectedTip
    | NoSuchEpochNo
    | InvalidDelegationDiscovery
    deriving (Eq, Generic, Show)

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

instance FromHttpApiData Iso8601Time where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

instance ToHttpApiData Iso8601Time where
    toUrlPiece = toText

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

instance ToText ApiEpochNumber where
    toText ApiEpochNumberLatest = "latest"
    toText (ApiEpochNumber (EpochNo e)) = T.pack $ show e

instance FromText ApiEpochNumber where
    fromText txt = case txt of
        "latest" ->
            Right ApiEpochNumberLatest
        rest -> case T.decimal @Int rest of
            Right (num, "") | num >= minValue && num <= maxValue ->
                Right $ ApiEpochNumber $ EpochNo $ fromIntegral num
            _ ->
                Left errText
      where
        minValue = fromIntegral $ minBound @Word31
        maxValue = fromIntegral $ maxBound @Word31
        errText  = TextDecodingError $ unwords
            [ "I couldn't parse the given epoch number."
            , "I am expecting either the word 'latest' or, an integer from"
            , show (minBound @Word31)
            , "to"
            , show (maxBound @Word31) <> "."
            ]

instance ToHttpApiData ApiEpochNumber where
    toUrlPiece = toText

instance FromHttpApiData ApiEpochNumber where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

data ApiPoolId
    = ApiPoolIdPlaceholder
    | ApiPoolId PoolId
    deriving (Eq, Generic, Show)

instance FromText AccountPublicKey where
    fromText txt = case xpubFromText (T.encodeUtf8 txt) of
        Left _ ->
            Left $ TextDecodingError $ unwords $
            [ "AccountPublicKey: unable to deserialize ShelleyKey from json. "
            , "Expecting hex-encoded string of 128 characters."]
        Right pubkey ->
            Right $ AccountPublicKey $ ApiT $ ShelleyKey pubkey

{-------------------------------------------------------------------------------
                              API Types: Byron
-------------------------------------------------------------------------------}

data ApiByronWallet = ApiByronWallet
    { id :: !(ApiT WalletId)
    , balance :: !(ApiByronWalletBalance)
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe (ApiT WalletPassphraseInfo))
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    } deriving (Eq, Generic, Show)

newtype ApiByronWalletMigrationInfo = ApiByronWalletMigrationInfo
    { migrationCost :: Quantity "lovelace" Natural
    } deriving (Eq, Generic, Show)

{-------------------------------------------------------------------------------
                              Polymorphic Types
-------------------------------------------------------------------------------}

-- | Polymorphic wrapper type to put around primitive types and, 3rd party lib
-- types to avoid defining orphan instances and/or, undesirable instances on
-- primitive types. It helps to keep a nice separation of concerns between the
-- API layer and other modules.
newtype ApiT a =
    ApiT { getApiT :: a }
    deriving (Generic, Show, Eq)

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

instance FromJSON ApiEpochInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiEpochInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiSelectCoinsData n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiSelectCoinsData n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelection n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelection n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiCoinSelectionInput n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiCoinSelectionInput n) where
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

instance FromJSON AccountPublicKey where
    parseJSON =
        parseJSON >=> eitherToParser . bimap ShowFmt Prelude.id . fromText
instance ToJSON AccountPublicKey where
    toJSON =
        toJSON . T.decodeUtf8 . serializeXPub . getApiT . key

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
        SomeRandomWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Random)
        SomeIcarusWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Icarus)
        SomeTrezorWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Trezor)
        SomeLedgerWallet w -> toJSON w
            & withExtraField (fieldName, toJSON $ toText Ledger)
      where
        fieldName :: Text
        fieldName = "style"

instance FromJSON SomeByronWalletPostData where
    parseJSON = withObject "SomeByronWallet" $ \obj -> do
        obj .: "style" >>= \case
            t | t == toText Random ->
                SomeRandomWallet <$> parseJSON (Aeson.Object obj)
            t | t == toText Icarus ->
                SomeIcarusWallet <$> parseJSON (Aeson.Object obj)
            t | t == toText Trezor ->
                SomeTrezorWallet <$> parseJSON (Aeson.Object obj)
            t | t == toText Ledger ->
                SomeLedgerWallet <$> parseJSON (Aeson.Object obj)
            _ ->
                fail "unrecognized wallet's style."

withExtraField
    :: (Text, Value)
    -> Value
    -> Value
withExtraField (k,v) = \case
    Aeson.Object m -> Aeson.Object (HM.insert k v m)
    json -> json

instance FromMnemonic mw => FromJSON (ByronWalletPostData mw) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON (ByronWalletPostData mw) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPutData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPutPassphraseData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutPassphraseData where
    toJSON = genericToJSON defaultRecordTypeOptions

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
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT (Passphrase purpose)) where
    toJSON = toJSON . toText . getApiT

instance FromMnemonic sizes => FromJSON (ApiMnemonicT sizes)
  where
    parseJSON bytes = do
        xs <- parseJSON bytes
        m <- eitherToParser $ left ShowFmt $ fromMnemonic @sizes xs
        return $ ApiMnemonicT m

instance ToJSON (ApiMnemonicT sizes) where
    toJSON (ApiMnemonicT (SomeMnemonic mw)) = toJSON (mnemonicToText mw)

instance FromJSON (ApiT WalletId) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT WalletId) where
    toJSON = toJSON . toText . getApiT

instance FromJSON (ApiT AddressPoolGap) where
    parseJSON = parseJSON >=>
        eitherToParser . bimap ShowFmt ApiT . fromText . T.pack . show @Integer
instance ToJSON (ApiT AddressPoolGap) where
    toJSON = toJSON . getAddressPoolGap . getApiT

instance FromJSON (ApiT WalletBalance) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletBalance) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

data ApiByronWalletBalance = ApiByronWalletBalance
    { available :: !(Quantity "lovelace" Natural)
    , total :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

instance FromJSON ApiByronWalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiByronWalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . toText . getApiT

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

instance FromJSON (ApiT WalletName) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT WalletName) where
    toJSON = toJSON . toText . getApiT

instance FromJSON (ApiT WalletPassphraseInfo) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletPassphraseInfo) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

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

instance DecodeAddress t => FromJSON (PostTransactionFeeData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionFeeData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiTimeReference where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiTimeReference where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ApiBlockReference where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiBlockReference where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT EpochNo) where
    parseJSON = fmap (ApiT . unsafeEpochNo) . parseJSON
instance ToJSON (ApiT EpochNo) where
    toJSON (ApiT (EpochNo en)) = toJSON $ fromIntegral @Word31 @Word32 en

instance FromJSON (ApiT SlotNo) where
    parseJSON = fmap (ApiT . SlotNo) . parseJSON
instance ToJSON (ApiT SlotNo) where
    toJSON (ApiT (SlotNo sn)) = toJSON sn

instance FromJSON ApiNetworkTip where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNetworkTip where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (AddressAmount n) where
    parseJSON bytes = do
        v@(AddressAmount _ (Quantity c)) <-
            genericParseJSON defaultRecordTypeOptions bytes
        if isValidCoin (Coin $ fromIntegral c)
            then return v
            else fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (getCoin maxBound) <> " lovelace."

instance EncodeAddress n => ToJSON (AddressAmount n) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress n => FromJSON (ApiTransaction n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiTransaction n) where
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
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT (Hash "Tx")) where
    toJSON = toJSON . toText . getApiT

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
        parseJSON >=> eitherToParser . bimap ShowFmt Prelude.id . fromText
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
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT (Hash "Genesis")) where
    toJSON = toJSON . toText . getApiT

instance FromJSON ApiNetworkParameters where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiNetworkParameters where
    toJSON = genericToJSON defaultRecordTypeOptions

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

instance FromJSON ApiByronWalletMigrationInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiByronWalletMigrationInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

{-------------------------------------------------------------------------------
                             FromText/ToText instances
-------------------------------------------------------------------------------}

instance DecodeAddress n => FromText (AddressAmount n) where
    fromText text = do
        let err = Left . TextDecodingError $ "Parse error. Expecting format \
            \\"<amount>@<address>\" but got " <> show text
        case split (=='@') text of
            [] -> err
            [_] -> err
            [l, r] -> AddressAmount
                <$> fmap ((,Proxy @n) . ApiT) (decodeAddress @n r)
                <*> fromText l
            _ -> err

instance EncodeAddress n => ToText (AddressAmount n) where
    toText (AddressAmount (ApiT addr, _) coins) =
        toText coins <> "@" <> encodeAddress @n addr

instance FromText PostExternalTransactionData where
    fromText text = case convertFromBase Base16 (T.encodeUtf8 text) of
        Left _ ->
            fail "Parse error. Expecting hex-encoded format."
        Right load ->
            pure $ PostExternalTransactionData load

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
            bimap pretty ApiPoolId (fromText t)

instance ToHttpApiData ApiPoolId where
    toUrlPiece = \case
        ApiPoolIdPlaceholder -> "*"
        ApiPoolId pid -> toText pid

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

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure

{-------------------------------------------------------------------------------
                          User-Facing Address Encoding
-------------------------------------------------------------------------------}

-- | An abstract class to allow encoding of addresses depending on the target
-- backend used.
class EncodeAddress (n :: NetworkDiscriminant) where
    encodeAddress :: Address -> Text

-- | An abstract class to allow decoding of addresses depending on the target
-- backend used.
class DecodeAddress (n :: NetworkDiscriminant) where
    decodeAddress :: Text -> Either TextDecodingError Address

-- | Encode an 'Address' to a human-readable format. This produces two kinds of
-- encodings:
--
-- - [Base58](https://en.wikipedia.org/wiki/Base58)
--   for legacy / Byron addresses
-- - [Bech32](https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki)
--   for Shelley addresses
--
-- The right encoding is picked by looking at the raw 'Address' representation
-- in order to figure out to which class the address belongs.
instance EncodeAddress 'Mainnet where
    encodeAddress = gEncodeAddress

instance EncodeAddress 'Testnet where
    encodeAddress = gEncodeAddress

gEncodeAddress :: Address -> Text
gEncodeAddress (Address bytes) =
    if isJust (decodeLegacyAddress bytes) then base58 else bech32
  where
    base58 = T.decodeUtf8 $ encodeBase58 bitcoinAlphabet bytes
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = [Bech32.humanReadablePart|addr|]

-- | Decode text string into an 'Address'. Jörmungandr recognizes two kind of
-- addresses:
--
-- - Legacy / Byron addresses encoded as `Base58`
-- - Shelley addresses, encoded as `Bech32`
--
-- See also 'EncodeAddress Jormungandr'
instance DecodeAddress 'Mainnet where
    decodeAddress = gDecodeAddress (decodeShelleyAddress @'Mainnet)

instance DecodeAddress 'Testnet where
    decodeAddress = gDecodeAddress (decodeShelleyAddress @'Testnet)

gDecodeAddress
    :: (ByteString -> Either TextDecodingError Address)
    -> Text
    -> Either TextDecodingError Address
gDecodeAddress decodeShelley text =
    case (tryBech32, tryBase58) of
        (Just bytes, _) -> decodeShelley bytes
        (_, Just bytes) -> decodeLegacyAddress bytes
            & maybeToEither (TextDecodingError
            "Unable to decode Address: neither Bech32-encoded nor a \
            \valid Byron Address.")
        (Nothing, Nothing) -> Left $ TextDecodingError
            "Unable to decode Address: encoding is neither Bech32 nor \
            \Base58."
  where
    -- | Attempt decoding a legacy 'Address' using a Base58 encoding.
    tryBase58 :: Maybe ByteString
    tryBase58 =
        decodeBase58 bitcoinAlphabet (T.encodeUtf8 text)

    -- | Attempt decoding an 'Address' using a Bech32 encoding.
    tryBech32 :: Maybe ByteString
    tryBech32 = do
        (_, dp) <- either (const Nothing) Just (Bech32.decodeLenient text)
        dataPartToBytes dp
