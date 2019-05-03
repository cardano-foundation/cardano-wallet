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

module Cardano.Wallet.Compatibility
    ( -- * Target
      HttpBridge
    ) where

import Prelude

import Cardano.Environment
    ( Network (Local, Mainnet, Staging, Testnet), network, protocolMagic )
import Cardano.Wallet.Binary
    ( encodeAddress, encodeProtocolMagic, encodeTx )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), getKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), TxId (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA

-- | A type representing the http-bridge as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data HttpBridge

-- | Compute a transaction id; assumed to be effectively injective.
-- It returns an hex-encoded 64-byte hash.
--
-- NOTE: This is a rather expensive operation
instance TxId HttpBridge where
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
instance KeyToAddress HttpBridge where
    keyToAddress key =
        Address $ CBOR.toStrictByteString $ encodeAddress xpub encodeAttributes
      where
        xpub = getKey key
        encodeAttributes = case network of
            Mainnet -> emptyAttributes
            Staging -> emptyAttributes
            Testnet -> attributesWithProtocolMagic (protocolMagic network)
            Local -> attributesWithProtocolMagic (protocolMagic network)

        attributesWithProtocolMagic pm = mempty
            <> CBOR.encodeMapLen 1
            <> CBOR.encodeWord 2
            <> CBOR.encodeBytes
                (CBOR.toStrictByteString $ encodeProtocolMagic pm)

        emptyAttributes = CBOR.encodeMapLen 0
