module Cardano.Wallet.TxWitnessTag where

import Prelude

data TxWitnessTag
  = TxWitnessByronUTxO
  | TxWitnessShelleyUTxO
  deriving (Show, Eq)
