{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Faucet.Types
    ( AddressIndex (..)
    , AddressStyle (..)
    , FaucetAddress (..)
    , IndexedAddress (..)
    , unIndexedAddress
    , IndexedMnemonic (..)
    , unIndexedMnemonic
    , Mnemonic (..)
    , MnemonicIndex (..)
    ) where

--------------------------------------------------------------------------------

import Prelude

import qualified Codec.Binary.Encoding as Binary
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Cardano.Address
    ( Address (unAddress)
    , unsafeMkAddress
    )
import Cardano.Mnemonic
    ( SomeMnemonic
    )
import Cardano.Mnemonic.Extended
    ( mkSomeMnemonic
    , someMnemonicToSentence
    )
import Codec.Binary.Encoding
    ( AbstractEncoding (EBase16)
    , fromBase16
    )
import Control.Monad
    ( guard
    )
import Data.Aeson
    ( FromJSON
    , KeyValue ((.=))
    , ToJSON (toJSON)
    , parseJSON
    , (.:)
    )
import Data.Typeable
    ( Typeable
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )
import Servant
    ( FromHttpApiData (..)
    , ToHttpApiData
    , toUrlPiece
    )

--------------------------------------------------------------------------------

newtype FaucetAddress = FaucetAddress { unFaucetAddress :: Address }
    deriving newtype (Eq)
    deriving stock (Show)

instance ToJSON FaucetAddress where
    toJSON = toJSON
        . T.decodeUtf8
        . Binary.encode EBase16
        . unAddress
        . unFaucetAddress

instance FromJSON FaucetAddress where
    parseJSON j = do
        t <- parseJSON j
        case fromBase16 (T.encodeUtf8 t) of
          Left err -> fail $ "Invalid base16 encoded address: " <> err
          Right ad -> pure $ FaucetAddress $ unsafeMkAddress ad

data IndexedAddress = IndexedAddress AddressIndex FaucetAddress
    deriving stock (Show, Generic)

unIndexedAddress :: IndexedAddress -> FaucetAddress
unIndexedAddress (IndexedAddress _index address) = address

instance ToJSON IndexedAddress where
    toJSON (IndexedAddress index address) =
        Aeson.object [ "index" .= index, "address" .= address ]

instance FromJSON IndexedAddress where
    parseJSON = Aeson.withObject "IndexedAddress" $ \o -> do
        index <- o .: "index"
        address <- o .: "address"
        pure $ IndexedAddress index address

newtype MnemonicIndex = MnemonicIndex { mnemonicIndexToNatural :: Natural }
    deriving newtype
        ( Eq
        , Ord
        , Num
        , Enum
        , ToJSON
        , ToHttpApiData
        , FromHttpApiData
        )
    deriving stock (Show, Generic)

instance FromJSON MnemonicIndex where
    parseJSON j = do
        n <- MnemonicIndex <$> parseJSON j
        n <$ guard (n >= minBound  && n <= maxBound)

instance Bounded MnemonicIndex where
    minBound = MnemonicIndex 0
    maxBound = MnemonicIndex 10000

data IndexedMnemonic = IndexedMnemonic MnemonicIndex Mnemonic
    deriving stock (Show, Generic)

unIndexedMnemonic :: IndexedMnemonic -> Mnemonic
unIndexedMnemonic (IndexedMnemonic _index mnemonic) = mnemonic

instance ToJSON IndexedMnemonic where
    toJSON (IndexedMnemonic index mnemonic) =
        Aeson.object [ "index" .= index, "mnemonic" .= mnemonic ]

instance FromJSON IndexedMnemonic where
    parseJSON = Aeson.withObject "IndexedMnemonic" $ \o -> do
        index <- o .: "index"
        mnemonic <- o .: "mnemonic"
        pure $ IndexedMnemonic index mnemonic

data AddressStyle = AddressStyleShelley | AddressStyleByron | AddressStyleIcarus
    deriving stock (Show, Generic)

instance ToHttpApiData AddressStyle where
    toUrlPiece AddressStyleShelley = "shelley"
    toUrlPiece AddressStyleByron = "byron"
    toUrlPiece AddressStyleIcarus = "icarus"

instance FromHttpApiData AddressStyle where
    parseUrlPiece "shelley" = Right AddressStyleShelley
    parseUrlPiece "byron" = Right AddressStyleByron
    parseUrlPiece "icarus" = Right AddressStyleIcarus
    parseUrlPiece _ = Left "Invalid address style"

instance ToJSON AddressStyle where
    toJSON = Aeson.String . toUrlPiece

instance FromJSON AddressStyle where
    parseJSON = Aeson.withText "AddressStyle" $
        either (fail . T.unpack) pure . parseUrlPiece

newtype AddressIndex = AddressIndex { addressIndexToNatural :: Natural }
    deriving newtype
        ( Eq
        , Ord
        , Num
        , Enum
        , ToJSON
        , FromJSON
        , ToHttpApiData
        , FromHttpApiData
        )
    deriving stock (Show, Generic)

newtype Mnemonic = Mnemonic { toSomeMnemonic :: SomeMnemonic }
    deriving stock (Show, Generic, Typeable)

instance ToJSON Mnemonic where
    toJSON = toJSON . someMnemonicToSentence . toSomeMnemonic

instance FromJSON Mnemonic where
    parseJSON j = do
        sentence <- parseJSON j
        Mnemonic <$>
            case mkSomeMnemonic @[9, 12, 15, 18, 21, 24] (T.words sentence) of
                Left err -> fail $ show err
                Right mn -> pure mn
