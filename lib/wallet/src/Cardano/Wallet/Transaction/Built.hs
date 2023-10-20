module Cardano.Wallet.Transaction.Built
    ( BuiltTx (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    , Tx
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxMeta
    )

data BuiltTx = BuiltTx
    { builtTx :: Tx
    , builtTxMeta :: TxMeta
    , builtSealedTx :: SealedTx
    }
    deriving (Show, Eq)
