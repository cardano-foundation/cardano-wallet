module Cardano.Wallet.Deposit.Pure.UTxOHistory
    ( UTxOHistory
    , empty
    , appendBlock

    , DeltaUTxOHistory (..)
    , getUTxO
    ) where

import Cardano.Wallet.Deposit.Pure.UTxO.DeltaUTxO
    ( DeltaUTxO
    )
import Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory
    ( UTxOHistory
    , appendBlock
    , empty
    , getUTxO
    )
import Cardano.Wallet.Deposit.Read
    ( Slot
    , SlotNo
    )

-- | Changes to the UTxO history.
data DeltaUTxOHistory
    = -- | New slot tip, changes within that block.
      AppendBlock SlotNo DeltaUTxO
    | -- | Rollback tip.
      Rollback Slot
    | -- | Move finality forward.
      Prune SlotNo
