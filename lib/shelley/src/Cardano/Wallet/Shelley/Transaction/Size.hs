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

class MaxSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    maxSizeOf :: Int

class MinSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    minSizeOf :: Int

sizeOfSignedTx :: [TxIn] -> [TxOut] -> Int
sizeOfSignedTx ins outs = emptySize
    + ((40 + inPadding) * length ins)
    + ((65 + outPadding) * length outs)
  where
    -- According to the ledger specs, the 40 and 65 should be /the/ values, but
    -- to make @prop_estimateSizeNeverUnderestimates@ pass, we seem to need:
    inPadding = 110
    outPadding = 32
    emptySize = 17
    -- TODO: Figure out why and investigate closer

instance MinSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    minSizeOf = 33 -- Could be double checked.

instance MaxSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    maxSizeOf = 65 -- Could be double checked.
