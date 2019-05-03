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

import Cardano.Wallet.Binary
    ( encodeTx )
import Cardano.Wallet.Primitive.Types
    ( TxId (..) )
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
