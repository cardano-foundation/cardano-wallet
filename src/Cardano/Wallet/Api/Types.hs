{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
      Address (..)
    , Wallet (..)
    , WalletBalance (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)

    -- * Re-Export From Primitives
    , AddressState (..)
    , PoolId (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , AddressPoolGap
    , Passphrase(..)

    -- * Limits
    , passphraseMinLength
    , passphraseMaxLength

    -- * Polymorphic Types
    , ApiT (..)
    , ApiMnemonicT (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Mnemonic
import Cardano.Wallet.Primitive.Model
    ( AddressState (..)
    , PoolId (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , mkWalletName
    )
import Control.Applicative
    ( (<|>) )
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
    , tagSingleConstructors
    )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Nat, Symbol )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.Primitive.Types as P
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data Address = Address
    { _id :: !(ApiT P.Address)
    , _state :: !(ApiT AddressState)
    } deriving (Eq, Generic, Show)

data Wallet = Wallet
    { _id :: !(ApiT WalletId)
    , _addressPoolGap :: !(ApiT AddressPoolGap)
    , _balance :: !(ApiT WalletBalance)
    , _delegation :: !(ApiT (WalletDelegation (ApiT PoolId)))
    , _name :: !(ApiT WalletName)
    , _passphrase :: !(ApiT WalletPassphraseInfo)
    , _state :: !(ApiT WalletState)
    } deriving (Eq, Generic, Show)

data WalletPostData = WalletPostData
    { _addressPoolGap :: !(Maybe (ApiT AddressPoolGap))
    , _mnemonicSentence :: !(ApiMnemonicT '[15,18,21,24] "seed")
    , _mnemonicSecondFactor :: !(Maybe (ApiMnemonicT '[9,12] "generation"))
    , _name :: !(ApiT WalletName)
    , _passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

newtype WalletPutData = WalletPutData
    { _name :: (Maybe (ApiT WalletName))
    } deriving (Eq, Generic, Show)

data WalletPutPassphraseData = WalletPutPassphraseData
    { _oldPassphrase :: !(ApiT (Passphrase "encryption"))
    , _newPassphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Eq, Generic, Show)

data WalletBalance = WalletBalance
    { _available :: !(Quantity "lovelace" Natural)
    , _total :: !(Quantity "lovelace" Natural)
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

{-------------------------------------------------------------------------------
                               JSON Instances
-------------------------------------------------------------------------------}

instance FromJSON Address where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Address where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT AddressState) where
    parseJSON = fmap ApiT . genericParseJSON defaultSumTypeOptions
instance ToJSON (ApiT AddressState) where
    toJSON = genericToJSON defaultSumTypeOptions . getApiT

instance FromJSON (ApiT P.Address) where
    parseJSON bytes = do
        x <- parseJSON bytes
        maybe
           (fail "Unable to decode Address: expected Base58 encoding")
           (pure . ApiT . P.Address)
           (decodeBase58 bitcoinAlphabet $ T.encodeUtf8 x)
instance ToJSON (ApiT P.Address )where
    toJSON = toJSON
        . T.decodeUtf8 . encodeBase58 bitcoinAlphabet . P.getAddress . getApiT

instance FromJSON Wallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Wallet where
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

instance FromJSON (ApiT (Passphrase "encryption")) where
    parseJSON = parseJSON >=> \case
        t | T.length t < passphraseMinLength ->
            fail $ "passphrase is too short: expected at least "
                <> show passphraseMinLength <> " chars"
        t | T.length t > passphraseMaxLength ->
            fail $ "passphrase is too long: expected at most "
                <> show passphraseMaxLength <> " chars"
        t ->
            return $ ApiT $ Passphrase $ BA.convert $ T.encodeUtf8 t

passphraseMinLength :: Int
passphraseMinLength = 10

passphraseMaxLength :: Int
passphraseMaxLength = 255

instance ToJSON (ApiT (Passphrase "encryption")) where
    toJSON (ApiT (Passphrase bytes)) = toJSON $ T.decodeUtf8 $ BA.convert bytes

instance {-# OVERLAPS #-}
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    , FromJSON (ApiMnemonicT rest purpose)
    ) =>
    FromJSON (ApiMnemonicT (mw ': rest) purpose)
  where
    parseJSON bytes = parseMW <|> parseRest where
        parseMW = do
            ApiMnemonicT x <- parseJSON @(ApiMnemonicT '[mw] purpose) bytes
            return $ ApiMnemonicT x
        parseRest = do
            ApiMnemonicT x <- parseJSON @(ApiMnemonicT rest purpose) bytes
            return $ ApiMnemonicT x

instance
    ( n ~ EntropySize mw
    , csz ~ CheckSumBits n
    , ConsistentEntropy n mw csz
    ) =>
    FromJSON (ApiMnemonicT (mw ': '[]) purpose)
  where
    parseJSON bytes = do
        xs <- parseJSON bytes
        m <- eitherToParser $ mkMnemonic @mw xs
        let pwd = Passphrase $ entropyToBytes $ mnemonicToEntropy m
        return $ ApiMnemonicT (pwd, xs)

instance ToJSON (ApiMnemonicT sizes purpose) where
    toJSON (ApiMnemonicT (_, xs)) = toJSON xs

instance FromJSON (ApiT WalletId) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletId) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON (ApiT AddressPoolGap) where
    parseJSON x = do
        gap <- parseJSON x
        ApiT <$> eitherToParser (mkAddressPoolGap gap)
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
    parseJSON x = fmap ApiT . eitherToParser . mkWalletName =<< parseJSON x
instance ToJSON (ApiT WalletName) where
    toJSON = toJSON . getWalletName . getApiT

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

taggedSumTypeOptions :: TaggedObjectOptions -> Aeson.Options
taggedSumTypeOptions opts = defaultSumTypeOptions
    { sumEncoding = TaggedObject (_tagFieldName opts) (_contentsFieldName opts)
    }

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
