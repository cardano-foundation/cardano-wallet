{-# LANGUAGE DeriveGeneric #-}
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

module Cardano.Environment.Jormungandr
    (
    -- * Networking
      Network(..)
    , network
    , ProtocolMagic(..)
    , protocolMagic
    ) where

import Prelude

import Cardano.Environment
    ( unsafeLookupEnv )
import Data.Int
    ( Int32 )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import GHC.Generics
    ( Generic )

import qualified Data.Text as T

-- | Available network options.
data Network = Mainnet | Testnet
    deriving (Generic, Show, Eq, Enum)

instance FromText Network where
    fromText = \case
        "mainnet" -> Right Mainnet
        "testnet" -> Right Testnet
        s -> Left $ TextDecodingError $ T.unpack s
            <> " is neither \"mainnet\" nor \"testnet\""

instance ToText Network where
    toText = \case
        Mainnet -> "mainnet"
        Testnet -> "testnet"

-- | Get the current target 'Network' from the Environment.
--
-- Throws a runtime exception is the ENV var isn't set or, is invalid.
network :: Network
network =
    unsafeLookupEnv "NETWORK"
{-# NOINLINE network #-}

newtype ProtocolMagic = ProtocolMagic Int32
    deriving (Generic, Show)

-- | Get the 'ProtocolMagic' corresponding to a given 'Network'.
protocolMagic :: Network -> ProtocolMagic
protocolMagic = \case
    Mainnet -> ProtocolMagic 764824073
    Testnet -> ProtocolMagic 1097911063
