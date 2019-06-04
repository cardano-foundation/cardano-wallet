{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains static configuration parameters. Rather than providing
-- and carrying around a configuration file through the application, we resolve
-- configuration data at runtime using the available environment.
--
-- This gives us a flexible and portable approach to software configuration, and
-- remove some pain from the development perspective. Prior to starting, the
-- wallet is expected to have a few configuration parameter available. One may
-- rely on a `.env` file to bundle configuration settings together for a given
-- target environment.

module Cardano.Wallet.HttpBridge.Environment
    (
    -- * Networking
      Network(..)
    , KnownNetwork (..)
    , ProtocolMagic(..)
    ) where

import Prelude

import Data.Int
    ( Int32 )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import GHC.Generics
    ( Generic )

import qualified Data.Text as T

-- | Available network options.
data Network = Mainnet | Testnet | Staging
    deriving (Generic, Show, Eq, Enum)

-- | Magic constant associated to a given network
newtype ProtocolMagic = ProtocolMagic Int32
    deriving (Generic, Show)

class KnownNetwork (n :: Network) where
    networkVal :: Network
    protocolMagic :: ProtocolMagic

instance KnownNetwork 'Mainnet where
    networkVal = Mainnet
    protocolMagic = ProtocolMagic 764824073

instance KnownNetwork 'Staging where
    networkVal = Staging
    protocolMagic = ProtocolMagic 633343913

instance KnownNetwork 'Testnet where
    networkVal = Testnet
    protocolMagic = ProtocolMagic 1097911063

instance FromText Network where
    fromText = \case
        "mainnet" -> Right Mainnet
        "testnet" -> Right Testnet
        "staging" -> Right Staging
        s -> Left $ TextDecodingError $ T.unpack s
            <> " is neither \"mainnet\", \"testnet\" nor \"staging\"."

instance ToText Network where
    toText = \case
        Mainnet -> "mainnet"
        Testnet -> "testnet"
        Staging -> "staging"
