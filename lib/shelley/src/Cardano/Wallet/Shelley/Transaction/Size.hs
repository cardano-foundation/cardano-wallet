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
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.Types

-- TODO: To be implemented


class MaxSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    maxSizeOf :: Int

class MinSizeOf (t :: *) (n :: NetworkDiscriminant) (k :: Depth -> * -> *) where
    minSizeOf :: Int


sizeOfSignedTx :: [TxIn] -> [TxOut] -> Int
sizeOfSignedTx = error "sizeOfSignedTx to be implemented"


instance MaxSizeOf Address 'Mainnet IcarusKey where
    maxSizeOf = error "maxSizeOf Mainnet IcarusKey not implemented"
instance MinSizeOf Address 'Mainnet IcarusKey where
    minSizeOf = error "minSizeOf Mainnet IcarusKey not implemented"

instance MaxSizeOf Address ('Testnet pm) IcarusKey where
    maxSizeOf = error "maxSizeOf Testnet IcarusKey not implemented"
instance MinSizeOf Address ('Testnet pm) IcarusKey where
    minSizeOf = error "minSizeOf Testnet IcarusKey not implemented"

instance MaxSizeOf Address 'Mainnet ByronKey where
    maxSizeOf = error "maxSizeOf Mainnet ByronKey not implemented"
instance MinSizeOf Address 'Mainnet ByronKey where
    minSizeOf = error "minSizeOf Mainnet ByronKey not implemented"

instance MaxSizeOf Address ('Testnet pm) ByronKey where
    maxSizeOf = error "maxSizeOf Testnet ByronKey not implemented"
instance MinSizeOf Address ('Testnet pm) ByronKey where
    minSizeOf = error "maxSizeOf Testnet ByronKey not implemented"

