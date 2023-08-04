module Cardano.Wallet.Primitive.Delegation.UTxO
  ( stakeKeyCoinDistr
  )
where

import Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin
  )
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount (..)
  )
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.Tx.TxOut
  ( TxOut (..)
  )
import Cardano.Wallet.Primitive.Types.UTxO
  ( UTxO (..)
  )
import Data.Map
  ( Map
  )
import Data.Map qualified as Map
import Prelude

-- | Calculate how much `Coin` exists on each `Maybe RewardAccount` in the
-- `UTxO` given a way to extract `Maybe RewardAccount` from an `Address`.
stakeKeyCoinDistr
  :: (Address -> Maybe RewardAccount)
  -> UTxO
  -> Map (Maybe RewardAccount) Coin
stakeKeyCoinDistr stakeRef =
  Map.fromListWith (<>) . map classifyOut . Map.elems . unUTxO
  where
    classifyOut :: TxOut -> (Maybe RewardAccount, Coin)
    classifyOut (TxOut addr b) = (stakeRef addr, TokenBundle.getCoin b)
