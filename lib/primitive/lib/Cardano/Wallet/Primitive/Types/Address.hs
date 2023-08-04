{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main 'Address' data type used by the wallet.
module Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  , AddressState (..)
  )
where

import Control.DeepSeq
  ( NFData (..)
  )
import Data.Bifunctor
  ( bimap
  )
import Data.ByteArray.Encoding
  ( Base (Base16)
  , convertFromBase
  , convertToBase
  )
import Data.ByteString
  ( ByteString
  )
import Data.Hashable
  ( Hashable
  )
import Data.Text.Class
  ( CaseStyle (..)
  , FromText (..)
  , TextDecodingError (..)
  , ToText (..)
  , fromTextToBoundedEnum
  , toTextFromBoundedEnum
  )
import Data.Text.Encoding qualified as T
import Fmt
  ( Buildable (..)
  , prefixF
  , suffixF
  )
import GHC.Generics
  ( Generic
  )
import Quiet
  ( Quiet (..)
  )
import Prelude

-- | Representation of Cardano addresses.
--
-- Addresses are basically a human-friendly representation of public keys.
-- Historically in Cardano, there exist different sort of addresses, and new
-- ones are to come. So far, we can distinguish between three types of
-- address:
--
-- - Byron Random addresses, which holds a payload with derivation path details
-- - Byron Sequential addresses, also known as Icarus'style addresses
-- - Shelley base addresses, see also [implementation-decisions/address](https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md)
--
-- For more details, see also [About Address Derivation](https://github.com/cardano-foundation/cardano-wallet/wiki/About-Address-Derivation)
--
-- Shelley base addresses can be divided into two types:
--
-- - Single Addresses: which only hold a public spending key
-- - Group Addresses: which hold both a spending and delegation keys
--
-- It'll therefore seem legitimate to represent addresses as:
--
-- @
-- data Address
--   = ByronAddress !ByteString
--   | SingleAddress !XPub
--   | GroupAddress !XPub XPub
-- @
--
-- However, there's a major drawback to this approach:  we have to consider all
-- three constructors everywhere, and make sure we test every function using
-- them three despite having no need for such fine-grained representation.
--
-- Indeed, from the wallet core code, addresses are nothing more than an opaque
-- bunch of bytes that can be compared with each other. When signing
-- transactions, we have to look up addresses anyway and therefore can
-- re-derive their corresponding public keys. The only moment the distinction
-- between address types matters is when it comes to representing addresses at
-- the edge of the application (the API layer). And here, this is precisely
-- where we need to also what target backend we're connected to. Different
-- backends use different encodings which may not be compatible.
--
-- Therefore, for simplicity, it's easier to consider addresses as "bytes", and
-- only peak into these bytes whenever we need to do something with them. This
-- makes it fairly clear that addresses are just an opaque string for the wallet
-- layer and that the underlying encoding is rather agnostic to the underlying
-- backend.
newtype Address = Address
  { unAddress :: ByteString
  }
  deriving (Generic, Eq, Ord)
  deriving anyclass (NFData, Hashable)
  deriving (Read, Show) via (Quiet Address)

instance Buildable Address where
  build addr =
    mempty
      <> prefixF 8 addrF
      <> "..."
      <> suffixF 8 addrF
    where
      addrF = build (toText addr)

instance ToText Address where
  toText =
    T.decodeUtf8
      . convertToBase Base16
      . unAddress

instance FromText Address where
  fromText =
    bimap textDecodingError Address
      . convertFromBase Base16
      . T.encodeUtf8
    where
      textDecodingError = TextDecodingError . show

-- | Denotes if an address has been previously used or not... whether that be
-- in the output of a transaction on the blockchain or one in our pending set.
data AddressState = Used | Unused
  deriving (Bounded, Enum, Eq, Generic, Show)

instance FromText AddressState where
  fromText = fromTextToBoundedEnum SnakeLowerCase

instance ToText AddressState where
  toText = toTextFromBoundedEnum SnakeLowerCase

instance Buildable AddressState where
  build = build . toText

instance NFData AddressState
