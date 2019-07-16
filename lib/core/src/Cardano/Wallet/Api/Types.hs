{-# LANGUAGE BangPatterns #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
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
    , ApiWallet (..)
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , ApiBlockData (..)
    , ApiTransaction (..)
    , ApiFee (..)
    , AddressAmount (..)
    , ApiErrorCode (..)
    , Iso8601Range (..)

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)
    , getApiMnemonicT
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( FromMnemonic (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Coin (..)
    , DecodeAddress (..)
    , Direction (..)
    , EncodeAddress (..)
    , Hash (..)
    , PoolId (..)
    , ShowFmt (..)
    , SlotId (..)
    , TxStatus (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , isValidCoin
    )
import Control.Arrow
    ( left )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..)
    , SumEncoding (..)
    , ToJSON (..)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    , sumEncoding
    )
import Data.Bifunctor
    ( bimap, first )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text, split )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , splitAtLastOccurrence
    )
import Data.Time
    ( UTCTime )
import Data.Time.Format
    ( defaultTimeLocale, formatTime, parseTimeM )
import Fmt
    ( pretty )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Nat, Symbol, symbolVal )
import Numeric.Natural
    ( Natural )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data ApiAddress t = ApiAddress
    { id :: !(ApiT Address, Proxy t)
    , state :: !(ApiT AddressState)
    } deriving (Eq, Generic, Show)

data ApiWallet = ApiWallet
    { id :: !(ApiT WalletId)
    , addressPoolGap :: !(ApiT AddressPoolGap)
    , balance :: !(ApiT WalletBalance)
    , delegation :: !(ApiT (WalletDelegation (ApiT PoolId)))
    , name :: !(ApiT WalletName)
    , passphrase :: !(Maybe (ApiT WalletPassphraseInfo))
    , state :: !(ApiT WalletState)
    } deriving (Eq, Generic, Show)

data WalletPostData = WalletPostData
    { addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    , mnemonicSentence :: !(ApiMnemonicT '[15,18,21,24] "seed")
    , mnemonicSecondFactor :: !(Maybe (ApiMnemonicT '[9,12] "generation"))
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

data PostTransactionData t = PostTransactionData
    { payments :: !(NonEmpty (AddressAmount t))
    , passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

newtype PostTransactionFeeData t = PostTransactionFeeData
    { payments :: (NonEmpty (AddressAmount t))
    } deriving (Eq, Generic, Show)

newtype ApiFee = ApiFee
    { amount :: (Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

data ApiTransaction t = ApiTransaction
    { id :: !(ApiT (Hash "Tx"))
    , amount :: !(Quantity "lovelace" Natural)
    , insertedAt :: !(Maybe ApiBlockData)
    , depth :: !(Quantity "block" Natural)
    , direction :: !(ApiT Direction)
    , inputs :: !(NonEmpty (AddressAmount t))
    , outputs :: !(NonEmpty (AddressAmount t))
    , status :: !(ApiT TxStatus)
    } deriving (Eq, Generic, Show)

data AddressAmount t = AddressAmount
    { address :: !(ApiT Address, Proxy t)
    , amount :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

data ApiBlockData = ApiBlockData
    { time :: UTCTime
    , block :: !(ApiT SlotId)
    } deriving (Eq, Generic, Show)

-- | Error codes returned by the API, in the form of snake_cased strings
data ApiErrorCode
    = NoSuchWallet
    | WalletAlreadyExists
    | NoRootKey
    | WrongEncryptionPassphrase
    | KeyNotFoundForAddress
    | NotEnoughMoney
    | UtxoNotEnoughFragmented
    | TransactionIsTooBig
    | InputsDepleted
    | CannotCoverFee
    | InvalidCoinSelection
    | NetworkUnreachable
    | NetworkMisconfigured
    | CreatedInvalidTransaction
    | RejectedByCoreNode
    | BadRequest
    | NotFound
    | MethodNotAllowed
    | NotAcceptable
    | UnsupportedMediaType
    | UnexpectedError
    deriving (Eq, Generic, Show)

-- | A range of dates in ISO-8601 UTC format without symbols. Meant to be
-- rendered as a HTTP 'Header', where the 'name' type-parameter renders as a
-- prefix, e.g.
--
-- name 20190227T160329Z-*
--
-- 'Nothing' ("*") can be used instead of upper and/or lower boundary.
--
-- - `20190227T160329Z-*`: means all transactions after
--    2019-02-27 T 16:03:29Z (including)
-- - `*-20190227T160329Z`: means all transactions before 2019-02-27 T 16:03:29Z
--    (including)
-- - `*-*`: means all transactions
-- - `20190227T000000Z-20200227T000000Z`: means all transaction between
--    2019-02-27 and 2020-02-27, in ascending order.
-- - `20200227T000000Z-20190227T000000Z`: means all transaction between
--  2020-02-27 and 2019-02-27, in descending order.
data Iso8601Range (name :: Symbol)
    = Iso8601Range (Maybe UTCTime) (Maybe UTCTime)
    deriving (Eq, Show)

instance KnownSymbol name => ToText (Iso8601Range name) where
    toText (Iso8601Range t1 t2) = prefix <> " " <> suffix
      where
        prefix = T.pack $ symbolVal (Proxy @name)
        suffix = timeToText t1 <> "-" <> timeToText t2
        timeToText = \case
            Nothing -> "*"
            Just t -> T.pack $ formatTime defaultTimeLocale basicUtc t

instance KnownSymbol name => FromText (Iso8601Range name) where
    fromText t = do
        (prefix, suffix) <- guardM (splitAtLastOccurrence ' ' t)
            "Unable to find required space separator character."
        (t1, t2) <- guardM (splitAtLastOccurrence '-' suffix)
            "Unable to find required hyphen separator character."
        guardB (prefix == expectedPrefix) $
            "Invalid prefix string found. Expecting: " <> show expectedPrefix
        v1 <- guardM (parseTime t1) $
            "Invalid start time string: " <> show t1
        v2 <- guardM (parseTime t2) $
            "Invalid end time string: " <> show t2
        guardB (validRange v1 v2)
            "Start time is later than end time."
        pure $ Iso8601Range v1 v2
      where
        expectedPrefix = T.pack $ symbolVal (Proxy @name)

        guardB :: Bool -> String -> Either TextDecodingError ()
        guardB b err = if b then Right () else raise err

        guardM :: Maybe a -> String -> Either TextDecodingError a
        guardM ma err = maybe (raise err) Right ma

        raise :: forall a . String -> Either TextDecodingError a
        raise err = Left $ TextDecodingError $
            "Error encountered while decoding ISO 8601 time range: " <> err

        parseTime :: Text -> Maybe (Maybe UTCTime)
        parseTime = \case
            "*" -> pure Nothing
            timeText -> pure <$>
                parseTimeM False defaultTimeLocale basicUtc $ T.unpack timeText

        validRange :: Maybe UTCTime -> Maybe UTCTime -> Bool
        validRange mt1 mt2 = fromMaybe True $ (<) <$> mt1 <*> mt2

-- | ISO 8601 basic format (UTC).
basicUtc :: String
basicUtc = "%Y%m%dT%H%M%S%QZ"

instance KnownSymbol name => FromHttpApiData (Iso8601Range (name :: Symbol))
  where
    parseUrlPiece = first (T.pack . getTextDecodingError) . fromText

instance KnownSymbol name => ToHttpApiData (Iso8601Range (name :: Symbol))
  where
    toUrlPiece = toText

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

instance DecodeAddress t => FromJSON (ApiAddress t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (ApiAddress t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance {-# OVERLAPS #-} DecodeAddress t => FromJSON (ApiT Address, Proxy t)
  where
    parseJSON x = do
        let proxy = Proxy @t
        addr <- parseJSON x >>= eitherToParser
            . bimap ShowFmt ApiT
            . decodeAddress proxy
        return (addr, proxy)
instance {-# OVERLAPS #-} EncodeAddress t => ToJSON (ApiT Address, Proxy t)
  where
    toJSON (addr, proxy) = toJSON . encodeAddress proxy . getApiT $ addr

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

instance FromJSON WalletPutData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON WalletPutPassphraseData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON  WalletPutPassphraseData where
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

instance FromJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    parseJSON = fmap ApiT . genericParseJSON walletDelegationOptions
instance ToJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    toJSON = genericToJSON walletDelegationOptions . getApiT

instance FromJSON (ApiT WalletName) where
    parseJSON = parseJSON >=> eitherToParser . bimap ShowFmt ApiT . fromText
instance ToJSON (ApiT WalletName) where
    toJSON = toJSON . toText . getApiT

instance FromJSON (ApiT WalletPassphraseInfo) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletPassphraseInfo) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON (ApiT WalletState) where
    parseJSON = fmap ApiT . genericParseJSON walletStateOptions
instance ToJSON (ApiT WalletState) where
    toJSON = genericToJSON walletStateOptions . getApiT

instance FromJSON (ApiT PoolId) where
    parseJSON = fmap (ApiT . PoolId) . parseJSON
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . getPoolId . getApiT

instance DecodeAddress t => FromJSON (PostTransactionData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress t => FromJSON (PostTransactionFeeData t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (PostTransactionFeeData t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT SlotId) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT SlotId) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON ApiBlockData where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON ApiBlockData where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress t => FromJSON (AddressAmount t) where
    parseJSON bytes = do
        v@(AddressAmount _ (Quantity c)) <-
            genericParseJSON defaultRecordTypeOptions bytes
        if isValidCoin (Coin $ fromIntegral c)
            then return v
            else fail $
                "invalid coin value: value has to be lower than or equal to "
                <> show (getCoin maxBound) <> " lovelace."

instance EncodeAddress t => ToJSON (AddressAmount t) where
    toJSON = genericToJSON defaultRecordTypeOptions

instance DecodeAddress t => FromJSON (ApiTransaction t) where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance EncodeAddress t => ToJSON (ApiTransaction t) where
    toJSON = genericToJSON defaultRecordTypeOptions

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
walletDelegationOptions = taggedSumTypeOptions $ TaggedObjectOptions
    { _tagFieldName = "status"
    , _contentsFieldName = "target"
    }

-- | Options for encoding a wallet state. It can be serialized to and from JSON
-- as follows:
--
-- >>> Aeson.encode Ready
-- {"status":"ready"}
--
-- >>> Aeson.encode $ Restoring (Quantity 14)
-- {"status":"restoring","progress":{"quantity":14,"unit":"percent"}}

walletStateOptions :: Aeson.Options
walletStateOptions = taggedSumTypeOptions $ TaggedObjectOptions
    { _tagFieldName = "status"
    , _contentsFieldName = "progress"
    }

{-------------------------------------------------------------------------------
                             FromText/ToText instances
-------------------------------------------------------------------------------}

instance DecodeAddress t => FromText (AddressAmount t) where
    fromText text = do
        let err = Left . TextDecodingError $ "Parse error. Expecting format \
            \\"<amount>@<address>\" but got " <> show text
        case split (=='@') text of
            [] -> err
            [_] -> err
            [l, r] -> AddressAmount
                <$> fmap ((,Proxy @t) . ApiT) (decodeAddress (Proxy @t) r)
                <*> fromText l
            _ -> err

instance EncodeAddress t => ToText (AddressAmount t) where
    toText (AddressAmount (ApiT addr, proxy) coins) =
        toText coins <> "@" <> encodeAddress proxy addr

{-------------------------------------------------------------------------------
                             HTTPApiData instances
-------------------------------------------------------------------------------}

instance FromText a => FromHttpApiData (ApiT a) where
    parseUrlPiece = bimap pretty ApiT . fromText
instance ToText a => ToHttpApiData (ApiT a) where
    toUrlPiece = toText . getApiT

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
    }

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
    , omitNothingFields = True
    }

taggedSumTypeOptions :: TaggedObjectOptions -> Aeson.Options
taggedSumTypeOptions opts = defaultSumTypeOptions
    { sumEncoding = TaggedObject (_tagFieldName opts) (_contentsFieldName opts)
    }

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
