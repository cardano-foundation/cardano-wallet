{-# OPTIONS --erasure #-}
module Cardano.Wallet.Deposit.Read where

open import Haskell.Prelude
import Haskell.Data.Map as Map

Addr = Nat
Address = Addr

Slot = Nat

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}
data Value : Set where
    MkValue : Integer → Value

instance
  iSemigroupValue : Semigroup Value
  _<>_ {{iSemigroupValue}} (MkValue a) (MkValue b) = MkValue (a + b)

  iMonoidValue : Monoid Value
  iMonoidValue = record {DefaultMonoid (record {mempty = MkValue 0})}

exceeds : Value → Value → Bool
exceeds (MkValue a) (MkValue b) = a >= b

minus : Value → Value → Value
minus (MkValue a) (MkValue b) = MkValue (a - b)

{-----------------------------------------------------------------------------
    Transactions
------------------------------------------------------------------------------}

TxId = Nat
Ix = Int

TxIn = TxId × Ix

record TxOut : Set where
  field
    address : Address
    value   : Value

record Tx : Set where
  field
    txid    : TxId
    inputs  : List TxIn
    outputs : List TxOut
