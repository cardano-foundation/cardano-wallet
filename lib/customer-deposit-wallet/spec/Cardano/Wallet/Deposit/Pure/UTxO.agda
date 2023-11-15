{-# OPTIONS --erasure #-}

module Cardano.Wallet.Deposit.Pure.UTxO
    {-
    ; UTxO
    -}
    where

open import Haskell.Prelude hiding (null)

open import Cardano.Wallet.Deposit.Read using
    ( Address
    ; TxIn
    ; TxOut
    ; Value
    )

import Haskell.Data.Map as Map

{-----------------------------------------------------------------------------
    UTxO
------------------------------------------------------------------------------}

UTxO = Map.Map TxIn TxOut

null : UTxO → Bool
null = Map.null

postulate
  balance : UTxO → Value
  excluding : UTxO → List TxIn → UTxO
  union : UTxO → UTxO → UTxO

  filterByAddress : (Address → Bool) → UTxO → UTxO
