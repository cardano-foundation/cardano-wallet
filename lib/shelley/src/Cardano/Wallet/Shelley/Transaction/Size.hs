{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Size estimation of cbor-encoded transactions in Shelley

module Cardano.Wallet.Shelley.Transaction.Size
    ( MinSizeOf (..)
    , MaxSizeOf (..)
    , sizeOfSignedTx
    )
    where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth, NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), TxIn (..), TxOut (..) )

import qualified Data.ByteString as BS

-- FIXME:
-- Assume null metadata (1 byte). Will need to account for metadata if present.
--
-- SIGNED-TX
--     = CBOR-LIST-LEN (3)    -- 1 byte
--     | TX-BODY
--     | CBOR-MAP-LEN  (1)    -- 1 byte / Assume vkeyWitness only
--     | U8                   -- 1 byte
--     | CBOR-LIST-LEN (nInp)
--     | TX_METADATA / NULL   -- 1 byte
sizeOfSignedTx :: [(TxIn, TxOut)] -> [TxOut] -> Int
sizeOfSignedTx ins outs
    = 1                            -- CBOR-LIST-LEN (3)
    + sizeOfTxBody ins outs
    + 1                            -- CBOR-MAP-LEN (1), assume vKeyWitness only
    + 1                            -- U8, vKeyWitness tag
    + sizeOfMajorType (length ins) -- CBOR-LIST-LEN (#TxIn)
    + (length ins) * sizeOfWitness

sizeOfWitness :: Int
sizeOfWitness
    = 1  -- CBOR-LIST-LEN (2)
    + 2  -- CBOR-BYTES (32)
    + 32 -- PUBLIC KEY
    + 2  -- CBOR-BYTES (64)
    + 64 -- SIGNATURE

-- FIXME Assume no certificates!
sizeOfTxBody :: [(TxIn, TxOut)] -> [TxOut] -> Int
sizeOfTxBody ins outs
    = 1                             -- CBOR-MAP-LEN (<7)
    + 1                             -- U8, inputs tag
    + sizeOfMajorType (length ins)  -- CBOR-LIST-LEN (#TxIn)
    + sum (sizeOfTxIn <$> ins)
    + 1                             -- U8, outputs tag
    + sizeOfMajorType (length outs) -- CBOR-LIST-LEN (#TxOut)
    + sum (sizeOfTxOut <$> outs)
    + 1                             -- U8, fee tag
    + sizeOfMajorType fee
    + 1                             -- U8, ttl tag
    + 9                             -- TODO: Could refine with actual TTL value.
  where
    fee = sum (map (getCoin . coin) (snd <$> ins))
        - sum (map (getCoin . coin) outs)

sizeOfTxIn :: (TxIn, TxOut) -> Int
sizeOfTxIn (TxIn _ ix, _)
    = 1                  -- CBOR-LIST-LEN (2)
    + 2                  -- CBOR-BYTES (32)
    + 32                 -- TX ID
    + sizeOfMajorType ix

sizeOfTxOut :: TxOut -> Int
sizeOfTxOut (TxOut (Address addr) (Coin value))
    = 1                      -- CBOR-LIST-LEN (2)
    + 2                      -- CBOR-BYTES (>=24 & <256)
    + BS.length addr
    + sizeOfMajorType value

-- Size of a CBOR major type and the value it carries. Typically for list len.
sizeOfMajorType :: Integral n => n -> Int
sizeOfMajorType x
    | n < 24         = 1
    | n < 256        = 2
    | n < 65536      = 3
    | n < 4294967296 = 5
    | otherwise      = 9
  where
    n = fromIntegral x :: Integer

class MaxSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    maxSizeOf :: Int

class MinSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    minSizeOf :: Int

instance MinSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    minSizeOf = 33 -- Could be double checked.

instance MaxSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    maxSizeOf = 65 -- Could be double checked.
