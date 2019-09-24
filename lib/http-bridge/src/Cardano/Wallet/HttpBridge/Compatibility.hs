{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
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
    , byronEpochLength
    , byronFeePolicy
    , byronSlotLength
    , byronTxMaxSize
    , byronBlockchainParameters
    ) where

import Prelude

import Cardano.Byron.Codec.Cbor
    ( decodeAddressPayload
    , encodeDerivationPathAttr
    , encodeProtocolMagicAttr
    , encodeTx
    )
import Cardano.Wallet
    ( BlockchainParameters (..) )
import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (Mainnet, Testnet), protocolMagic )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , DecodeAddress (..)
    , DefineTx (..)
    , EncodeAddress (..)
    , EpochLength (..)
    , Hash (..)
    , SlotLength (..)
    , StartTime (..)
    , Tx (..)
    )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Bifunctor
    ( bimap )
import Data.ByteString.Base58
    ( bitcoinAlphabet, decodeBase58, encodeBase58 )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Data.Word
    ( Word16 )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Wallet.HttpBridge.Primitive.Types as W
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T

-- | A type representing the http-bridge as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data HttpBridge (network :: Network)

instance DefineTx (HttpBridge network) where
    type Tx (HttpBridge network) = W.Tx
    inputs = W.inputs
    outputs = W.outputs
    txId = blake2b256 . encodeTx . (\(W.Tx inps outs) -> (inps, outs))
      where
        blake2b256 :: forall tag. CBOR.Encoding -> Hash tag
        blake2b256 =
            Hash . BA.convert . hash @_ @Blake2b_256 . CBOR.toStrictByteString

instance PersistTx (HttpBridge network) where
    resolvedInputs = flip zip (repeat Nothing) . W.inputs
    mkTx _ inps = W.Tx (fst <$> inps)

-- | Encode a public key to a (Byron / Legacy) Cardano 'Address'. This is mostly
-- dubious CBOR serializations with no data attributes.
instance KeyToAddress (HttpBridge 'Testnet) SeqKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k)
            [ encodeProtocolMagicAttr (protocolMagic @'Testnet) ]

instance KeyToAddress (HttpBridge 'Mainnet) SeqKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k) []

instance KeyToAddress (HttpBridge 'Testnet) RndKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k)
            [ encodeDerivationPathAttr pwd acctIx addrIx
            , encodeProtocolMagicAttr (protocolMagic @'Testnet)
            ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k

instance KeyToAddress (HttpBridge 'Mainnet) RndKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k)
            [ encodeDerivationPathAttr pwd acctIx addrIx ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k


-- | Encode an 'Address' to a human-readable format, in this case
--
-- [Base58](https://en.wikipedia.org/wiki/Base58)
instance EncodeAddress (HttpBridge (network :: Network)) where
    encodeAddress _ = T.decodeUtf8 . encodeBase58 bitcoinAlphabet . unAddress

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
block0 :: Block W.Tx
block0 = Block
    { header = BlockHeader
        { slotId = minBound
        , blockHeight = Quantity 0
        , prevBlockHash = Hash "genesis"
        }
    , transactions = []
    }

-- | Hard-coded epoch length
byronEpochLength :: EpochLength
byronEpochLength = EpochLength 21600

-- | Hard-coded fee policy for Cardano on Byron
byronFeePolicy :: FeePolicy
byronFeePolicy = LinearFee (Quantity 155381) (Quantity 43.946)

-- | Hard-coded slot duration
byronSlotLength :: SlotLength
byronSlotLength = SlotLength 20

-- | Hard-coded max transaction size
byronTxMaxSize :: Quantity "byte" Word16
byronTxMaxSize = Quantity 8192

byronBlockchainParameters
    :: forall n. KnownNetwork n
    => BlockchainParameters
byronBlockchainParameters = BlockchainParameters
    { getGenesisBlockDate = case networkVal @n of
        Mainnet -> StartTime $ posixSecondsToUTCTime 1506203091
        Testnet -> StartTime $ posixSecondsToUTCTime 1563999616
    , getFeePolicy = byronFeePolicy
    , getSlotLength = byronSlotLength
    , getTxMaxSize = byronTxMaxSize
    , getEpochLength = byronEpochLength
    , getEpochStability = Quantity 2160
    }
