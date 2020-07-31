{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Size estimation of cbor-encoded transactions in Byron

module Cardano.Wallet.Byron.Transaction.Size
    ( sizeOfTxIn
    , sizeOfSignedTx
    , sizeOfTxWitness
    , sizeOfTxOut
    , sizeOfCoin
    , maxSizeOf
    , MaxSizeOf
    , minSizeOf
    , MinSizeOf
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth, NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), TxIn (..), TxOut (..) )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- TX
--     = CBOR-LIST-LEN (3)    -- 1 byte
--     | CBOR-BEGIN-LIST      -- 1 byte
--     | *INPUT               -- ΣsizeOf(INPUT) bytes
--     | CBOR-BREAK-LIST      -- 1 byte
--     | CBOR-BEGIN-LIST      -- 1 byte
--     | *OUTPUT              -- ΣsizeOf(OUTPUT) bytes
--     | CBOR-BREAK-LIST      -- 1 byte
--     | ATTRIBUTES (Ø)       -- 1 byte
--                            == 6 + ΣsizeOf(i) + ΣsizeOf(o)
sizeOfTx :: [TxIn] -> [TxOut] -> Int
sizeOfTx inps outs = 6
    + sum (map sizeOfTxIn inps)
    + sum (map sizeOfTxOut outs)

-- SIGNED-TX
--     = CBOR-LIST-LEN (2)    -- 1 byte
--     | TX                   -- sizeOf(TX) bytes
--     | CBOR-LIST-LEN (n)    -- 1-2 bytes (assuming n < 255)
--     | *WITNESS             -- n * 139 bytes
--                            == 3 + sizeOf(TX) + 1-2 + n * 139
sizeOfSignedTx :: [TxIn] -> [TxOut] -> Int
sizeOfSignedTx inps outs = 1
    + sizeOfTx inps outs
    + sizeOf (CBOR.encodeListLen $ fromIntegral n)
    + n * sizeOfTxWitness
  where
    n = length inps

-- INPUT
--     = CBOR-LIST-LEN (2)    --        1 byte
--     | U8                   --        1 byte
--     | CBOR-TAG (24)        --        2 bytes
--     | BYTES (38-42)        --        2 bytes
--     | 36-40OCTET           -- 35 + 1-5 bytes #---*
--                            == 41 + (1-5)         |
--     *--------------------------------------------*
--     |
--     v
--     36-40OCTET
--         = CBOR-LIST-LEN (2)    --     1 byte
--         | CBOR-BYTES (32)      --     2 bytes
--         | 32OCTET              --    32 bytes
--         | U32                  --   1-5 bytes
sizeOfTxIn :: TxIn -> Int
sizeOfTxIn (TxIn _ ix) =
    41 + sizeOf (CBOR.encodeWord32 $ fromIntegral ix)

-- WITNESS
--     = CBOR-LIST-LEN (2)    --   1 byte
--     | U8                   --   1 byte
--     | CBOR-TAG (24)        --   2 bytes
--     | CBOR-BYTES (133      --   2 bytes
--     | 133OCTET             -- 133 bytes #--------*
--                            == 139                |
--     *--------------------------------------------*
--     |
--     v
--     133OCTET
--         = CBOR-LIST-LEN (2)    --   1 byte
--         | CBOR-BYTES (64)      --   2 bytes
--         | 64OCTET              --  64 bytes
--         | CBOR-BYTES (64)      --   2 bytes
--         | 64OCTET              --  64 bytes
sizeOfTxWitness :: Int
sizeOfTxWitness = 139

-- OUTPUT
--     = CBOR-LIST-LEN (2)    --     1 byte
--     | ADDRESS              -- 40-83 bytes
--     | U64                  --   1-9 bytes
--                            == 1 + sizeOf(ADDRESS) + 1-9
sizeOfTxOut :: TxOut -> Int
sizeOfTxOut (TxOut (Address bytes) c) =
    1 + BS.length bytes + sizeOfCoin c

-- Compute the size of a coin
sizeOfCoin :: Coin -> Int
sizeOfCoin = sizeOf . CBOR.encodeWord64 . getCoin

-- Size of a particular CBOR encoding
sizeOf :: CBOR.Encoding -> Int
sizeOf = fromIntegral . BL.length . CBOR.toLazyByteString

class MaxSizeOf (t :: *) (k :: Depth -> * -> *) where
    maxSizeOf :: NetworkDiscriminant -> Int

<<<<<<< HEAD
instance forall t k pm. (MaxSizeOf t 'Mainnet k) => MaxSizeOf t ('Staging pm) k where
    maxSizeOf = maxSizeOf @t @'Mainnet @k

class MinSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    minSizeOf :: Int
=======
class MinSizeOf (t :: *) (k :: Depth -> * -> *) where
    minSizeOf :: NetworkDiscriminant -> Int
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant

instance forall t k pm. (MinSizeOf t 'Mainnet k) => MinSizeOf t ('Staging pm) k where
    minSizeOf = minSizeOf @t @'Mainnet @k

-- ADDRESS (MainNet, Icarus)
--     = CBOR-LIST-LEN (2)    --     1 byte
--     | 39-43OCTET           -- 39-43 bytes #------*
--                                                  |
--     *--------------------------------------------*
--     |
--     v
--     39-43OCTET
--         = CBOR-LIST-LEN (2)    --     1 byte
--         | CBOR-TAG (24)        --     2 bytes
--         | CBOR-BYTES (33)      --     2 bytes
--         | 33OCTET              --    33 bytes #------*
--         | U32                  --   1-5 bytes        |
--                                                      |
--         *--------------------------------------------*
--         |
--         v
--         33OCTET
--             = CBOR-LIST-LEN (3)    --     1 byte
--             | CBOR-BYTES (28)      --     2 bytes
--             | 28OCTET              --    28 bytes
--             | ATTRIBUTES (Ø)       --     1 byte
--             | U8                   --     1 bytes
--
--
--
-- ADDRESS (TestNet, Icarus)
--     = CBOR-LIST-LEN (2)    --     1 byte
--     | 46-50OCTET           -- 46-50 bytes #------*
--                                                  |
--     *--------------------------------------------*
--     |
--     v
--     46-50OCTET
--         = CBOR-LIST-LEN (2)    --     1 byte
--         | CBOR-TAG (24)        --     2 bytes
--         | CBOR-BYTES (40)      --     2 bytes
--         | 40OCTET              --    40 bytes #------*
--         | U32                  --   1-5 bytes        |
--                                                      |
--         *--------------------------------------------*
--         |
--         v
--         40OCTET
--             = CBOR-LIST-LEN (3)    --     1 byte
--             | CBOR-BYTES (28)      --     2 bytes
--             | 28OCTET              --    28 bytes
--             | ATTRIBUTES (8)       --     8 bytes
--             | U8                   --     1 bytes
instance MaxSizeOf Address IcarusKey where
    maxSizeOf Mainnet     = 43
    maxSizeOf (Testnet _) = 50

instance MinSizeOf Address IcarusKey where
    minSizeOf Mainnet     = 39
    minSizeOf (Testnet _) = 46

-- ADDRESS (MainNet, Random)
--     = CBOR-LIST-LEN (2)    --     1 byte
--     | 72-76OCTET           -- 72-76 bytes #------*
--                                                  |
--     *--------------------------------------------*
--     |
--     v
--     72-76OCTET
--         = CBOR-LIST-LEN (2)    --     1 byte
--         | CBOR-TAG (24)        --     2 bytes
--         | CBOR-BYTES (33)      --     2 bytes
--         | 62-66OCTET           -- 62-66 bytes #------*
--         | U32                  --   1-5 bytes        |
--                                                      |
--         *--------------------------------------------*
--         |
--         v
--         62-66OCTET
--             = CBOR-LIST-LEN (3)    --     1 byte
--             | CBOR-BYTES (28)      --     2 bytes
--             | 28OCTET              --    28 bytes
--             | ATTRIBUTES (34)      -- 30-34 bytes
--             | U8                   --     1 bytes
--
--
-- ADDRESS (TestNet, Random)
--     = CBOR-LIST-LEN (2)    --     1 byte
--     | 79-83OCTET           -- 79-83 bytes #------*
--                                                  |
--     *--------------------------------------------*
--     |
--     v
--     79-83OCTET
--         = CBOR-LIST-LEN (2)    --     1 byte
--         | CBOR-TAG (24)        --     2 bytes
--         | CBOR-BYTES (33)      --     2 bytes
--         | 69-73OCTET           -- 69-73 bytes #------*
--         | U32                  --   1-5 bytes        |
--                                                      |
--         *--------------------------------------------*
--         |
--         v
--         69-73OCTET
--             = CBOR-LIST-LEN (3)    --     1 byte
--             | CBOR-BYTES (28)      --     2 bytes
--             | 28OCTET              --    28 bytes
--             | ATTRIBUTES (37-41)   -- 37-41 bytes
--             | U8                   --     1 bytes

instance MaxSizeOf Address ByronKey where
    maxSizeOf Mainnet     = 76
    maxSizeOf (Testnet _) = 83
instance MinSizeOf Address ByronKey where
    minSizeOf Mainnet     = 68
    minSizeOf (Testnet _) = 75
