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

module Cardano.Wallet.Jormungandr.Environment
    (
    -- * Networking
      Network(..)
    , network

    -- * Address Discrimination
    , hrp
    , single
    , grouped
    ) where

import Prelude

import Cardano.Wallet.Environment
    ( unsafeLookupEnv )
import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartFromText )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Word
    ( Word8 )
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

-- | Address discrimination Human-Readable Part (HRP).
--
-- - "ca" for 'Mainnet'
-- - "ta" for 'Testnet'
hrp :: HumanReadablePart
hrp = case network of
    Mainnet -> unsafeHumanReadablePart "ca"
    Testnet -> unsafeHumanReadablePart "ta"
  where
    unsafeHumanReadablePart = either errUnsafe id . humanReadablePartFromText
    errUnsafe _ =
        error "Programmers hard-coded an invalid bech32 human-readable part?"

-- | Address discriminant byte for single addresses, this is the first byte of
-- every addresses using the Shelley format carrying only a spending key.
single :: Word8
single = case network of
    Mainnet -> 0x03
    Testnet -> 0x83

-- | Address discriminant byte for grouped addresses, this is the first byte of
-- every addresses using the Shelley format carrying both a spending and a
-- delegation key.
grouped :: Word8
grouped = case network of
    Mainnet -> 0x04
    Testnet -> 0x84
