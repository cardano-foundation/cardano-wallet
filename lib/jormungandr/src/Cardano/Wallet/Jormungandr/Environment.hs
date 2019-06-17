{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

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
      Network (..)
    , KnownNetwork (..)
    ) where

import Prelude

import Codec.Binary.Bech32
    ( HumanReadablePart, humanReadablePartFromText )
import Data.Text
    ( Text )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

-- | Available network options.
data Network = Mainnet | Testnet
    deriving (Generic, Show, Eq, Bounded, Enum)

-- | Embed some constants into a network type.
class KnownNetwork (n :: Network) where
    networkVal :: Network
    hrp :: HumanReadablePart
        -- ^ Address discrimination Human-Readable Part (HRP).
        --
        -- - "ca" for 'Mainnet'
        -- - "ta" for 'Testnet'
    single :: Word8
        -- ^ Address discriminant byte for single addresses, this is the first byte of
        -- every addresses using the Shelley format carrying only a spending key.
    grouped :: Word8
        -- ^ Address discriminant byte for grouped addresses, this is the first byte of
        -- every addresses using the Shelley format carrying both a spending and a
        -- delegation key.

instance FromText Network where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText Network where
    toText = toTextFromBoundedEnum SnakeLowerCase

instance KnownNetwork 'Mainnet where
    networkVal = Mainnet
    hrp = unsafeHumanReadablePart "ca"
    single = 0x03
    grouped = 0x04

instance KnownNetwork 'Testnet where
    networkVal = Testnet
    hrp = unsafeHumanReadablePart "ta"
    single = 0x83
    grouped = 0x84

unsafeHumanReadablePart :: Text -> HumanReadablePart
unsafeHumanReadablePart = either errUnsafe id . humanReadablePartFromText
  where
    errUnsafe _ =
        error "Programmers hard-coded an invalid bech32 human-readable part?"
