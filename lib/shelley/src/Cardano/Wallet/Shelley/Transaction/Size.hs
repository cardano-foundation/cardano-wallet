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
sizeOfSignedTx ins outs = 180 + (40 * length ins) + ( 65 * length outs)
    -- TODO: Implement properly

instance MinSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    minSizeOf = 33 -- Could be double checked.

instance MaxSizeOf Address (n :: NetworkDiscriminant) ShelleyKey where
    maxSizeOf = 65 -- Could be double checked.
