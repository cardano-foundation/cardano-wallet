{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.HttpBridge.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Binary
    ( estimateMaxNumberOfInputsParams )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..), ProtocolMagic (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK)
    , KeyToAddress (..)
    , Passphrase (..)
    , PersistKey (..)
    , WalletKey (..)
    , XPrv
    , publicKey
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    , txId
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    , estimateMaxNumberOfInputsBase
    )
import Control.Monad
    ( forM, when )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )
import Fmt
    ( Buildable (..) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.HttpBridge.Binary as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- | Construct a 'TransactionLayer' compatible with Byron and the 'HttpBridge'
newTransactionLayer
    :: forall n k t.
        (KnownNetwork n, t ~ HttpBridge n, KeyToAddress t k, PersistKey k)
    => TransactionLayer t k
newTransactionLayer = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let ins = (fmap fst inps)
        let tx = Tx ins outs
        let txSigData = txId @(HttpBridge n) tx
        txWitnesses <- forM inps $ \(_in, TxOut addr _c) -> mkWitness txSigData
            <$> maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        return (tx, txWitnesses)

    , estimateSize = \(CoinSelection inps outs chngs) -> let n = length inps in
        Quantity $
        -- With `n` the number of inputs
        -- signed tx ----------------------------------- 1 + (1|2) + n*139 + ~?
        --  | list len 2              -- 1
        --  | sizeOf(tx)              -- ~? (depends, cf 'sizeOfTx')
        --  | list len n              -- 1-2 (assuming n < 255)
        --  | n * sizeOf(witness)     -- n * 139
        1
        + sizeOfTx (fst <$> inps) outs chngs
        + sizeOf (CBOR.encodeListLen $ fromIntegral n)
        + n * sizeOfTxWitness

    , estimateMaxNumberOfInputs =
        estimateMaxNumberOfInputsBase @t @k estimateMaxNumberOfInputsParams

    , validateSelection = \(CoinSelection _ outs _) -> do
        when (any (\ (TxOut _ c) -> c == Coin 0) outs)
            $ Left ErrInvalidTxOutAmount
    }
  where
    mkWitness
        :: Hash "Tx"
        -> (k 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    mkWitness tx (xPrv, pwd) = TxWitness
        $ CBOR.toStrictByteString
        $ CBOR.encodePublicKeyWitness (getRawKey $ publicKey xPrv)
        $ sign (SignTx tx) (xPrv, pwd)

    sign
        :: SignTag
        -> (k 'AddressK XPrv, Passphrase "encryption")
        -> Hash "signature"
    sign tag (key, (Passphrase pwd)) =
        Hash . CC.unXSignature $ CC.sign pwd (getRawKey key) (signTag tag)
      where
        -- | Encode magic bytes & the contents of a @SignTag@. Magic bytes are
        -- guaranteed to be different (and begin with a different byte) for different
        -- tags.
        signTag :: SignTag -> ByteString
        signTag = \case
            SignTx (Hash payload) ->
                "\x01" <> pm <> CBOR.toStrictByteString (CBOR.encodeBytes payload)
          where
            pm =
                let ProtocolMagic x = protocolMagic @n
                in CBOR.toStrictByteString . CBOR.encodeInt32 $ x

    --input -------------------------------------- 41 + (1|2|3|5)
    --  | list len 2                  -- 1
    --  | word8                       -- 1
    --  | tag 24                      -- 2
    --  | bytes ------------------------ 2 + 35 + (1|2|3|5)
    --  |   | list len 2  -- 1
    --  |   | bytes       -- 2 + 32
    --  |   | word32      -- 1|2|3|5
    sizeOfTxIn :: TxIn -> Int
    sizeOfTxIn (TxIn _ ix) =
        41 + sizeOf (CBOR.encodeWord32 $ fromIntegral ix)

    -- SEQ + MAINNET
    -- output ------------------------------------- 41-53
    --  | list len 2                  -- 1
    --  | address ---------------------- 39|40|41|43
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 33
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 1
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- SEQ + TESTNET
    -- output ------------------------------------- 48-60
    --  | list len 2                  -- 1
    --  | address ---------------------- 46|47|48|50
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 40
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 8
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- RND + MAINNET
    -- output ------------------------------------- 74-86
    --  | list len 2                  -- 1
    --  | address ---------------------- 72|73|74|76
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 66
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 34
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    --
    -- RND + TESTNET
    -- output ------------------------------------- 81-93
    --  | list len 2                  -- 1
    --  | address ---------------------- 79|80|81|83
    --  |  | list len 2         -- 1
    --  |  | tag 24             -- 2
    --  |  | bytes --------------- 2 + 73
    --  |  |  | list len 3 -- 1
    --  |  |  | bytes      -- 2 + 28
    --  |  |  | attributes -- 41
    --  |  |  | word8      -- 1
    --  |  | word32             -- 1|2|3|5
    --  | word64                      -- 1|2|3|5|9
    sizeOfTxOut :: TxOut -> Int
    sizeOfTxOut (TxOut (Address bytes) c) =
        1 + BS.length bytes + sizeOfCoin c

    -- Compute the size of a coin
    sizeOfCoin :: Coin -> Int
    sizeOfCoin = sizeOf . CBOR.encodeWord64 . getCoin

    -- Compute the size of the change, we assume that change is necessarily
    -- using a sequential scheme. For the rest, cf 'sizeOfTxOut'.
    -- Also, the size of the address depends on the size of the crc32, which can
    -- very between 1,2,3 and 5 bytes. We'll always consider the worst case for
    -- the change which makes an address payload of `43` bytes for mainnet,
    -- and `50` bytes on testnet.
    sizeOfChange :: Coin -> Int
    sizeOfChange c = case networkVal @n of
        Mainnet -> 1 + 43 + sizeOfCoin c
        Testnet -> 1 + 50 + sizeOfCoin c

    -- tx ------------------------------------- 6 + Σs(i) + ls(o) + Σs(c)
    --  | list len 3                  -- 1
    --  | begin                       -- 1
    --  | sizeOf(inps)                -- Σ sizeOf(inp)
    --  | break                       -- 1
    --  | begin                       -- 1
    --  | sizeOf(outs)                -- Σ sizeOf(out)
    --  | sizeOf(chngs)               -- Σ sizeOf(chng)
    --  | break                       -- 1
    --  | empty attributes            -- 1
    sizeOfTx :: [TxIn] -> [TxOut] -> [Coin] -> Int
    sizeOfTx inps outs chngs = 6
        + sum (map sizeOfTxIn inps)
        + sum (map sizeOfTxOut outs)
        + sum (map sizeOfChange chngs)

    -- witness ------------------------------------ 139
    --  | list len 2                  -- 1
    --  | word8                       -- 1
    --  | tag 24                      -- 2
    --  | bytes ------------------------ 2 + 133
    --  |   | list len 2 -- 1
    --  |   | bytes      -- 2+64
    --  |   | bytes      -- 2+64
    sizeOfTxWitness :: Int
    sizeOfTxWitness = 139

    -- Size of a particular CBOR encoding
    sizeOf :: CBOR.Encoding -> Int
    sizeOf = fromIntegral . BL.length . CBOR.toLazyByteString


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
    = SignTx (Hash "Tx")

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ =
        "I can't validate coin selection because at least one output has value 0."

type instance ErrValidateSelection (HttpBridge n) = ErrInvalidTxOutAmount
