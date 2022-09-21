{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Tx.Eras
 ( onTx )
 where

import Cardano.Wallet.Read.Tx
    ( Tx (..), TxT )

onTx :: (TxT era -> t) -> Tx era -> t
onTx f (Tx x) = f x
