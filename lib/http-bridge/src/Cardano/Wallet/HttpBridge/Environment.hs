{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
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
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( ProtocolMagic (..) )
import Data.Text.Class
    ( CaseStyle (..)
    , FromText (..)
    , ToText (..)
    , fromTextToBoundedEnum
    , toTextFromBoundedEnum
    )
import Data.Typeable
    ( Typeable )
import GHC.Generics
    ( Generic )

-- | Available network options.
data Network = Mainnet | Testnet
    deriving (Generic, Show, Eq, Bounded, Enum)

class Typeable n => KnownNetwork (n :: Network) where
    networkVal :: Network
    protocolMagic :: ProtocolMagic

instance KnownNetwork 'Mainnet where
    networkVal = Mainnet
    protocolMagic = ProtocolMagic 764824073

instance KnownNetwork 'Testnet where
    networkVal = Testnet
    protocolMagic = ProtocolMagic 1097911063

instance FromText Network where
    fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText Network where
    toText = toTextFromBoundedEnum SnakeLowerCase
