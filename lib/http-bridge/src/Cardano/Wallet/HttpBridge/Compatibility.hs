{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.HttpBridge.Compatibility
    ( -- * Target
      HttpBridge
    , Network (..)
    , block0
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Binary
    ( decodeAddressPayload, encodeProtocolMagic, encodeTx )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (Mainnet, Testnet), ProtocolMagic, protocolMagic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key (..), KeyToAddress (..), XPub, getKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockHeader (..)
    , DecodeAddress (..)
    , EncodeAddress (..)
    , Hash (..)
    , SlotId (..)
    , TxId (..)
    )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Bifunctor
    ( bimap )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Text.Class
    ( TextDecodingError (..) )

import qualified Cardano.Wallet.HttpBridge.Binary as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

-- | A type representing the http-bridge as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data HttpBridge (network :: Network)

-- | Compute a transaction id; assumed to be effectively injective.
-- It returns an hex-encoded 64-byte hash.
--
-- NOTE: This is a rather expensive operation
instance TxId (HttpBridge network) where
    txId = blake2b256 . encodeTx
      where
        -- | Encode a value to a corresponding Hash.
        --
        -- @
        --     txId :: Tx -> Hash "Tx"
        --     txId = blake2b256 . encodeTx
        -- @
        blake2b256 :: forall tag. CBOR.Encoding -> Hash tag
        blake2b256 =
            Hash . BA.convert . hash @_ @Blake2b_256 . CBOR.toStrictByteString

-- | Encode a public key to a (Byron / Legacy) Cardano 'Address'. This is mostly
-- dubious CBOR serializations with no data attributes.
instance KeyToAddress (HttpBridge 'Testnet) where
    keyToAddress = keyToAddressWith
        $ attributesWithProtocolMagic (protocolMagic @'Testnet)

instance KeyToAddress (HttpBridge 'Mainnet) where
    keyToAddress = keyToAddressWith emptyAttributes

keyToAddressWith :: CBOR.Encoding -> Key 'AddressK XPub -> Address
keyToAddressWith attrs key = Address
    $ CBOR.toStrictByteString
    $ CBOR.encodeAddress xpub attrs
  where
    xpub = getKey key

attributesWithProtocolMagic :: ProtocolMagic -> CBOR.Encoding
attributesWithProtocolMagic pm = mempty
    <> CBOR.encodeMapLen 1
    <> CBOR.encodeWord 2
    <> CBOR.encodeBytes (CBOR.toStrictByteString $ encodeProtocolMagic pm)

emptyAttributes :: CBOR.Encoding
emptyAttributes = CBOR.encodeMapLen 0

-- | Encode an 'Address' to a human-readable format, in this case
--
-- [Base58](https://en.wikipedia.org/wiki/Base58)
instance EncodeAddress (HttpBridge (network :: Network)) where
    encodeAddress _ = T.decodeUtf8 . encodeBase58 bitcoinAlphabet . getAddress

-- | Decode a [Base58](https://en.wikipedia.org/wiki/Base58) text string to an
-- 'Address'.
instance DecodeAddress (HttpBridge (network :: Network)) where
    decodeAddress _ x = do
        bytes <- maybe
            (Left $ TextDecodingError errBase58)
            Right
            (decodeBase58 bitcoinAlphabet $ T.encodeUtf8 x)
        -- We at least try to decode the address payload, since we need at least
        -- this to produce valid `TxOut` and whatnot when creating a transaction
        bimap (TextDecodingError . errDecoding) (const ()) $
            CBOR.deserialiseFromBytes decodeAddressPayload (BL.fromStrict bytes)
        return $ Address bytes
      where
        errBase58 = "Unable to decode Address: expected Base58 encoding."
        errDecoding _ = "Unable to decode Address: not a valid Byron address."

-- | An initial first block to initialize a chain using the http-bridge. We do
-- not use the `blockHash` and, do only use the `prevBlockHash` to catch up with
-- unstable epoch and therefore, the very first `prevBlockHash` matters not.
--
-- It isn't impossible to retrieve the 'blockHash' by computing a blake2b 256 of
-- the CBOR-serialized full block header, but this requires us to write the full
-- CBOR decoders (and encoders) for the all BlockHeader which is, for the
-- http-brdige implementation, a waste of time at the moment.
block0 :: BlockHeader
block0 = BlockHeader
    { slotId = SlotId 0 0
    , prevBlockHash = Hash "genesis"
    }
