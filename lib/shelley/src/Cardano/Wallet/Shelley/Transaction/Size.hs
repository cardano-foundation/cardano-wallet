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

-- TODO: To be implemented


class MaxSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    maxSizeOf :: Int

class MinSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    minSizeOf :: Int


sizeOfSignedTx :: [TxIn] -> [TxOut] -> Int
sizeOfSignedTx ins outs = 180 + (40 * length ins) + ((65 + fixme) * length outs)
  where
    -- According to the ledger specs, the 40 and 65 should be /the/ values, but
    -- this integration test failure suggests otherwise:
    -- ApplyTxError [LedgerFailure (UtxowFailure (UtxoFailure (FeeTooSmallUTxO
    -- {pfUTXOminFee = Coin 553001000, pfUTXOgivenFee = Coin 520001000})))]\"}")
    --
    -- Maybe that value wasn't taking delegation keys into account?
    --
    -- Regardless:
    -- TODO: Implement properly.
    fixme = 32

instance MinSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    minSizeOf = 33 -- Could be double checked.

instance MaxSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    maxSizeOf = 65 -- Could be double checked.
