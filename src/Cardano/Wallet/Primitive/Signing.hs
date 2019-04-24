{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module provides functionality for signing transactions.
--
-- It relies on the binary CBOR format of transactions, an AddressScheme
-- for deriving address private keys, and cardano-crypto for the actual signing.
module Cardano.Wallet.Primitive.Signing
    ( -- * Sign transactions
      mkStdTx
    , SignTxError (..)
    )
    where


import Prelude

import Cardano.Environment
    ( ProtocolMagic (..), network, protocolMagic )
import Cardano.Wallet.Binary
    ( TxWitness (..), encodeTx, toByteString )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK, RootK)
    , Key
    , Passphrase (..)
    , XPrv
    , XPub
    , getKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressScheme (keyFrom) )
import Cardano.Wallet.Primitive.Types
    ( Address, Hash (..), Tx (..), TxIn, TxOut (..) )
import Control.Monad
    ( forM )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteString
    ( ByteString )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteArray as BA

newtype SignTxError = KeyNotFoundForAddress Address
    deriving Show

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
--
mkStdTx
    :: AddressScheme s
    => s
    -> (Key 'RootK XPrv, Passphrase "encryption")
    -> [(TxIn, TxOut)]
    -- ^ Selected inputs
    -> [TxOut]
    -- ^ Selected outputs (including change)
    -> Either SignTxError (Tx, [TxWitness])
mkStdTx s creds@(_, pwd) ownedIns outs = do
    let ins = (fmap fst ownedIns)
        tx = Tx ins outs
        txSigData = hashTx tx
    txWitnesses <- forM ownedIns $ \(_in, TxOut addr _c) -> mkWitness txSigData
        <$> withEither (KeyNotFoundForAddress addr) (keyFrom addr creds s)
    return (tx, txWitnesses)
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right
    hashTx :: Tx -> Hash "tx"
    hashTx txSigData = Hash
        $ BA.convert
        $ (hash @_ @Blake2b_256)
        $ toByteString
        $ encodeTx txSigData
    mkWitness :: Hash "tx" -> Key 'AddressK XPrv -> TxWitness
    mkWitness tx xPrv
        = PublicKeyWitness
            (encodeXPub $ publicKey xPrv)
            (sign (SignTx tx) (xPrv, pwd))




-- | Used for signing transactions
sign
    :: SignTag
    -> (Key 'AddressK XPrv, Passphrase "encryption")
    -> Hash "signature"
sign tag (key, (Passphrase pwd)) =
    Hash . CC.unXSignature $ CC.sign pwd (getKey key) (signTag tag)


-- | To protect agains replay attacks (i.e. when an attacker intercepts a
-- signed piece of data and later sends it again), we add a tag to all data
-- that we sign. This ensures that even if some bytestring can be
-- deserialized into two different types of messages (A and B), the attacker
-- can't take message A and send it as message B.
--
-- We also automatically add the network tag ('protocolMagic') whenever it
-- makes sense, to ensure that things intended for testnet won't work for
-- mainnet.
--
-- The wallet only cares about the 'SignTx' tag. In 'cardano-sl' there was
-- a lot more cases.
newtype SignTag
    = SignTx (Hash "tx")
    deriving (Eq, Ord, Show)


-- | Encode magic bytes & the contents of a @SignTag@. Magic bytes are
-- guaranteed to be different (and begin with a different byte) for different
-- tags.
signTag :: SignTag -> ByteString
signTag = \case
    SignTx (Hash payload) -> "\x01" <> pm <> toByteString (CBOR.encodeBytes payload)
  where
    pm =
        let
            ProtocolMagic x = protocolMagic network
        in
            toByteString . CBOR.encodeInt32 $ x


-- | Get the underlying ByteString
encodeXPub :: (Key level XPub) -> ByteString
encodeXPub = CC.unXPub . getKey
