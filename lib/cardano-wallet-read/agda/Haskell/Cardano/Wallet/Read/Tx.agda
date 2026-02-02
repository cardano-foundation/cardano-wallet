{-# OPTIONS --erasure #-}

-- Synchronized manually with the corresponding Haskell module.
module Haskell.Cardano.Wallet.Read.Tx where

open import Haskell.Prelude
open import Haskell.Law

open import Data.Map using
    ( Map
    )
open import Data.Set using
    ( ℙ
    )
open import Haskell.Data.Word using
    ( Word16
    )
open import Haskell.Cardano.Wallet.Read.Address using
    ( CompactAddr
    )
open import Haskell.Cardano.Wallet.Read.Eras using
    ( IsEra
    )
open import Haskell.Cardano.Wallet.Read.Value using
    ( Value
    )

{-----------------------------------------------------------------------------
    TxId
------------------------------------------------------------------------------}
postulate
  TxId : Set

  instance
    iEqTxId : Eq TxId
    iOrdTxId : Ord TxId

    iLawfulEqTxId : IsLawfulEq TxId

{-----------------------------------------------------------------------------
    TxIx
------------------------------------------------------------------------------}
record TxIx : Set where
  constructor TxIxC
  field
    word16FromTxIx : Word16

open TxIx public

instance
  iEqTxIx : Eq TxIx
  iEqTxIx ._==_ x y = word16FromTxIx x == word16FromTxIx y

  iOrdFromCompareTxIx : OrdFromCompare TxIx
  iOrdFromCompareTxIx .OrdFromCompare.compare =
    λ x y → compare (word16FromTxIx x) (word16FromTxIx y)

  iOrdTxIx : Ord TxIx
  iOrdTxIx = record {OrdFromCompare iOrdFromCompareTxIx}

{-----------------------------------------------------------------------------
    TxIn
------------------------------------------------------------------------------}
record TxIn : Set where
  constructor TxInC
  field
    inputId : TxId
    inputIx : TxIx

open TxIn public

instance
  iEqTxIn : Eq TxIn
  iEqTxIn ._==_ x y = inputId x == inputId y && inputIx x == inputIx y

postulate instance
  iOrdTxIn : Ord TxIn

{-----------------------------------------------------------------------------
    TxOut
------------------------------------------------------------------------------}

postulate
  TxOut : Set

  getCompactAddr : TxOut → CompactAddr
  getValue : TxOut → Value
  mkBasicTxOut : CompactAddr → Value → TxOut

  prop-getCompactAddr-mkBasicTxOut
    : ∀ (addr : CompactAddr)
        (value : Value)
    → getCompactAddr (mkBasicTxOut addr value) ≡ addr

{-----------------------------------------------------------------------------
    ScriptValidity
------------------------------------------------------------------------------}
data IsValid : Set where
  IsValidC : Bool → IsValid

open IsValid public

{-----------------------------------------------------------------------------
    Tx
------------------------------------------------------------------------------}
postulate
  Tx : Set → Set

  getTxId : ∀{era} → {{IsEra era}} → Tx era → TxId
  getInputs : ∀{era} → {{IsEra era}} → Tx era → ℙ TxIn
  getCollateralInputs : ∀{era} → {{IsEra era}} → Tx era → ℙ TxIn
  getScriptValidity : ∀{era} → {{IsEra era}} → Tx era → IsValid
  utxoFromEraTx : ∀{era} → {{IsEra era}} → Tx era → Map TxIn TxOut
