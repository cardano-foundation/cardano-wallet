-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains an algorithm to select coins for migration from legacy
-- wallets to newer wallets.

module Cardano.Wallet.Primitive.CoinSelection.Migration
    ( selectCoinsForMigration
    ) where

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection )
import Cardano.Wallet.Primitive.Types
    ( Coin, UTxO )

-- TODO: Add FeePolicy.
selectCoinsForMigration
    :: UTxO
    -> Coin
      -- ^ Minimum coin size to include in outputs.
    -> [CoinSelection]
selectCoinsForMigration _utxo _minCoinSize = []
