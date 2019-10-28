{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
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
    -- * API Types
      ApiAddress (..)
    , ApiStakePool (..)
    , StakePoolMetrics (..)
    , ApiWallet (..)
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
    , ApiBlockReference (..)
    , ApiNetworkTip (..)
    , Iso8601Time (..)

    -- * API Types (Byron)
    , ApiByronWallet (..)
    , ApiByronWalletMigrationInfo (..)
    , ApiMigrateByronWalletData (..)
    , ByronWalletPostData (..)

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)
    , getApiMnemonicT
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( DecodeAddress (..), EncodeAddress (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , BoundType
    , Coin (..)
    , Direction (..)
    , EpochNo (..)
    , Hash (..)
    , PoolId (..)
    , ShowFmt (..)
    , SlotNo (..)
    , SyncProgress (..)
    , TxIn (..)
    , TxStatus (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , isValidCoin
    )
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
    , (.=)
    )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Either.Extra
    ( maybeToEither )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text, split )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time
    ( UTCTime )
import Data.Time.Text
    ( iso8601, iso8601ExtendedUtc, utcTimeFromText, utcTimeToText )
import Data.Word
    ( Word64 )
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

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data ApiAddress (n :: NetworkDiscriminant) = ApiAddress
    { id :: !(ApiT Address, Proxy n)
    , state :: !(ApiT AddressState)
    } deriving (Eq, Generic, Show)

data ApiWallet = ApiWallet
    { id :: !(ApiT WalletId)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , balance :: !(ApiT WalletBalance)
    , delegation :: !(ApiT (WalletDelegation (ApiT PoolId)))
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe (ApiT WalletPassphraseInfo))
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    } deriving (Eq, Generic, Show)

data ApiStakePool = ApiStakePool
    { id :: !(ApiT PoolId)
    , metrics :: !StakePoolMetrics
    } deriving (Eq, Generic, Show)

data StakePoolMetrics = StakePoolMetrics
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
    , mnemonicSentence :: !(ApiMnemonicT '[15,18,21,24] "seed")
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT '[9,12] "generation"))
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

data ByronWalletPostData = ByronWalletPostData
    { mnemonicSentence :: !(ApiMnemonicT '[12] "seed")
      -- It was theoretically possible to create a Byron wallet with a mnemonic
      -- sentence longer than 12 words. However, in practice, previous wallets
      -- only ever supported mnemonic sentences of exactly 12 words. Hence, we
      -- restrict ourselves to this length.
    , name :: !(ApiT WalletName)
    , passphrase :: !(ApiT (Passphrase "encryption"))
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
    , outputs :: !(NonEmpty (AddressAmount n))
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
    , nodeTip :: !ApiBlockReference
    , networkTip :: !ApiNetworkTip
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

{-------------------------------------------------------------------------------
                              API Types: Byron
-------------------------------------------------------------------------------}

data ApiByronWallet = ApiByronWallet
    { id :: !(ApiT WalletId)
    , balance :: !(ApiT WalletBalance)
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe (ApiT WalletPassphraseInfo))
    , state :: !(ApiT SyncProgress)
    , tip :: !ApiBlockReference
    } deriving (Eq, Generic, Show)

newtype ApiByronWalletMigrationInfo = ApiByronWalletMigrationInfo
    { migrationCost :: Quantity "lovelace" Natural
    } deriving (Eq, Generic, Show)

newtype ApiMigrateByronWalletData = ApiMigrateByronWalletData
    { passphrase :: ApiT (Passphrase "encryption")
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
--     { mnemonic :: ApiMnemonicT '[15,18,21,24] "root-seed"
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
newtype ApiMnemonicT (sizes :: [Nat]) (purpose :: Symbol) =
    ApiMnemonicT (Passphrase purpose, [Text])
    deriving (Generic, Show, Eq)

getApiMnemonicT :: ApiMnemonicT sizes purpose -> Passphrase purpose
getApiMnemonicT (ApiMnemonicT (pw, _)) = pw

{-------------------------------------------------------------------------------
                               JSON Instances
-------------------------------------------------------------------------------}

instance DecodeAddress n => FromJSON (ApiAddress n) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress n => ToJSON (ApiAddress n) where
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

instance FromJSON WalletPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPostData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON ByronWalletPostData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  ByronWalletPostData where
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

instance FromMnemonic sizes purpose => FromJSON (ApiMnemonicT sizes purpose)
  where
    parseJSON bytes = do
        xs <- parseJSON bytes
        m <- eitherToParser $ left ShowFmt $ fromMnemonic @sizes @purpose xs
        return $ ApiMnemonicT (m, xs)

instance ToJSON (ApiMnemonicT sizes purpose) where
    toJSON (ApiMnemonicT (!_, xs)) = toJSON xs

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

instance FromJSON (ApiT PoolId) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . toText . getApiT

instance FromJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    parseJSON = fmap ApiT . genericParseJSON walletDelegationOptions
instance ToJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    toJSON = genericToJSON walletDelegationOptions . getApiT

instance FromJSON ApiStakePool where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiStakePool where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON StakePoolMetrics where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON StakePoolMetrics where
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
    parseJSON = fmap (ApiT . EpochNo) . parseJSON
instance ToJSON (ApiT EpochNo) where
    toJSON (ApiT (EpochNo en))= toJSON en

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

instance ToJSON ApiErrorCode where
    toJSON = genericToJSON defaultSumTypeOptions

-- | Options for encoding wallet delegation settings. It can be serialized to
-- and from JSON as follows:
--
-- >>> Aeson.encode NotDelegating
-- {"status":"not_delegating"}
--
-- >>> Aeson.encode $ Delegating poolId
-- {"status":"delegating","target": "27522fe5-262e-42a5-8ccb-cef884ea2ba0"}
walletDelegationOptions :: Aeson.Options
walletDelegationOptions = taggedSumTypeOptions defaultSumTypeOptions $
    TaggedObjectOptions
        { _tagFieldName = "status"
        , _contentsFieldName = "target"
        }

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

instance FromJSON ApiMigrateByronWalletData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiMigrateByronWalletData where
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
