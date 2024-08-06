{- |
Copyright: © 2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

The 'Tx' type represents transactions as they are read from the mainnet ledger.
It is compatible with the era-specific index types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Tx
    ( Tx (..)
    , TxT
    ) where

import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    , TxT
    )
