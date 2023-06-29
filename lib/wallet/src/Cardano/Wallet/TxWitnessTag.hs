{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Wallet.TxWitnessTag where

import Prelude

import Cardano.Wallet.Address.Derivation
    ( Depth
    )
import Data.Kind
    ( Type
    )

data TxWitnessTag
    = TxWitnessByronUTxO
    | TxWitnessShelleyUTxO
    deriving (Show, Eq)

-- | Provide a transaction witness for a given private key. The type of witness
-- is different between types of keys and, with backward-compatible support, we
-- need to support many types for one backend target.
class TxWitnessTagFor (k :: Depth -> Type -> Type) where
    txWitnessTagFor :: TxWitnessTag
