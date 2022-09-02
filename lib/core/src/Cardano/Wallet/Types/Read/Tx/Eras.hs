{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Types.Read.Tx.Eras
 ( onTx )
 where

import Cardano.Wallet.Types.Read.Tx
    ( Tx (..), TxT )

onTx :: (TxT era -> t) -> Tx era -> t
onTx f (Tx x) = f x
