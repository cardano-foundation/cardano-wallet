{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Tx.Eras
  ( onTx
  )
where

import Cardano.Wallet.Read.Tx
  ( Tx (..)
  , TxT
  )

-- | Act upon the 'TxT' type value of a 'Tx'
onTx :: (TxT era -> t) -> Tx era -> t
onTx f (Tx x) = f x
