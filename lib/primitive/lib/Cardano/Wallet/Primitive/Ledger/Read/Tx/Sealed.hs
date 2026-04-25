-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Sealed
    ( fromSealedTx
    ) where

import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (unsafeReadTx)
    )
import Cardano.Wallet.Read
    ( EraValue
    , Tx
    )

import qualified Cardano.Wallet.Primitive.Types.Tx.SealedTx as W

-- | Extract the 'EraValue Tx' from a 'SealedTx'.
fromSealedTx :: W.SealedTx -> EraValue Tx
fromSealedTx = unsafeReadTx
