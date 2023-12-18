{-# OPTIONS --erasure #-}

module Cardano.Write.Tx.Balance
    {-
    ; ChangeAddressGen
        ; isChange
    ; PartialTx
        ; totalOut
    ; balanceTransaction
        ; prop-balanceTransaction-addresses
        ; prop-balanceTransaction-suceeds
    -}
    where

open import Haskell.Prelude
open import Haskell.Reasoning

open import Cardano.Wallet.Deposit.Pure.UTxO using
    ( UTxO
    )
open import Cardano.Wallet.Deposit.Read using
    ( Address
    ; Tx
    ; TxIn
    ; TxOut
    ; Value
    ; exceeds
    ; minus
    )
open import Haskell.Data.List.Prop using ( _∈_ )
open import Haskell.Data.Maybe using ( isJust )

import Cardano.Wallet.Deposit.Pure.UTxO as UTxO
import Haskell.Data.Map as Map

{-----------------------------------------------------------------------------
    Partial transactions
------------------------------------------------------------------------------}

record PartialTx : Set where
  field
    outputs : List TxOut

totalOut : PartialTx → Value
totalOut = mconcat ∘ map TxOut.value ∘ PartialTx.outputs

{-----------------------------------------------------------------------------
    Change addresses
------------------------------------------------------------------------------}

ChangeAddressGen : Set → Set
ChangeAddressGen c = c → (Address × c)

isChange : ChangeAddressGen c → Address → Set
isChange = λ gen addr → ∃ (λ c → fst (gen c) ≡ addr)

{-----------------------------------------------------------------------------
    Coin selection
------------------------------------------------------------------------------}

secondCons : ∀ {a b : Set} → b → (a × List b) → (a × List b)
secondCons y (x , ys) = (x , y ∷ ys)

-- | Greedily select inputs from a list in order to exceed a given value.
coinSelectionGreedy
    : Value → List (TxIn × TxOut) → (Value × List TxIn)
coinSelectionGreedy v [] = (mempty , [])
coinSelectionGreedy v ((txin , txout) ∷ xs) =
    let dv = (TxOut.value txout)
    in  if exceeds v dv
            then secondCons txin $ coinSelectionGreedy (minus v dv) xs
            else (minus dv v , [])

{-----------------------------------------------------------------------------
    Balance transaction
------------------------------------------------------------------------------}

-- | Balance a partial transaction by selecting inputs
-- and assigning change addresses.
balanceTransaction
    : UTxO
    → ChangeAddressGen c
    → c
    → PartialTx
    → Maybe Tx
balanceTransaction utxo newAddress c0 partialTx =
    let (changeValue , ins) = coinSelectionGreedy target (Map.toAscList utxo)
        changeOutput = record
            { address = fst (newAddress c0)
            ; value = changeValue
            }
    in
    if exceeds target (UTxO.balance utxo)
        then Nothing
        else Just $ record
          { txid = 0
          ; outputs = changeOutput ∷ PartialTx.outputs partialTx
          ; inputs = ins
          }
  where
    target = totalOut partialTx

unequal : ∀ {A : Set} (x : A) → Nothing ≡ Just x → ⊥
unequal x ()

unJust : ∀ {A : Set} {x y : A} → Just x ≡ Just y → x ≡ y
unJust refl = refl

{-----------------------------------------------------------------------------
    Balance transaction
    Property: Addresses
------------------------------------------------------------------------------}

lemma-balanceTransaction-addresses
  : ∀ (u : UTxO)
      (partialTx : PartialTx)
      (new : ChangeAddressGen c)
      (c0 : c)
      (tx : Tx)
  → balanceTransaction u new c0 partialTx ≡ Just tx 
  → map TxOut.address (Tx.outputs tx)
    ≡ fst (new c0) ∷ map TxOut.address (PartialTx.outputs partialTx)
lemma-balanceTransaction-addresses u partialTx new c0 tx balance
  with exceeds (totalOut partialTx) (UTxO.balance u)
...  | True = magic (unequal tx balance)
...  | False = begin
          map TxOut.address (Tx.outputs tx)
        ≡⟨ cong (λ x → map TxOut.address (Tx.outputs x)) (sym (unJust balance)) ⟩
          fst (new c0) ∷ map TxOut.address (PartialTx.outputs partialTx)
        ∎

lemma-isChange-c0
  : ∀ (new : ChangeAddressGen c)
      (c0 : c)
      (addr : Address)
  → addr ≡ fst (new c0)
  → isChange new addr
lemma-isChange-c0 = λ new c0 addr x → c0 `witness` (sym x)

lemma-||-equal
  : ∀ (b b' : Bool)
  → (b || b') ≡ True
  → (b ≡ True) ⋁ (b' ≡ True)
lemma-||-equal True b' refl = inl refl
lemma-||-equal False True refl = inr refl

onLeft
  : ∀ {p p' q : Set} → (p → p') → p ⋁ q → p' ⋁ q
onLeft f (inl p) = inl (f p)
onLeft f (inr q) = inr q

-- | How balanceTransaction assigns addresses to outputs:
-- Each output is either
-- * a change address
-- * an address from the input partial transaction
prop-balanceTransaction-addresses
  : ∀ (u : UTxO)
      (partialTx : PartialTx)
      (new : ChangeAddressGen c)
      (c0 : c)
      (tx : Tx)
  → balanceTransaction u new c0 partialTx ≡ Just tx
  → ∀ (addr : Address)
    → addr ∈ map TxOut.address (Tx.outputs tx)
    → isChange new addr
        ⋁ addr ∈ map TxOut.address (PartialTx.outputs partialTx)

prop-balanceTransaction-addresses u partialTx new c0 tx balance addr el
    = onLeft lemma2 (lemma-||-equal b1 b2 (sym lemma1))
  where
    lemma1 =
      begin
        True
      ≡⟨ sym el ⟩
        elem addr (map TxOut.address $ Tx.outputs tx)
      ≡⟨ cong (elem addr) (lemma-balanceTransaction-addresses u partialTx new c0 tx balance) ⟩
        elem addr (fst (new c0) ∷ map TxOut.address (PartialTx.outputs partialTx))
      ≡⟨⟩
        ((addr == fst (new c0))
            || (elem addr $ map TxOut.address $ PartialTx.outputs partialTx))
      ∎

    b1 b2 : Bool
    b1 = (addr == fst (new c0))
    b2 = (elem addr $ map TxOut.address $ PartialTx.outputs partialTx)

    lemma2
      : (addr == fst (new c0)) ≡ True
      → isChange new addr
    lemma2 =
      lemma-isChange-c0 new c0 addr
      ∘ equality addr (fst (new c0))

{-----------------------------------------------------------------------------
    Balance transaction
    Property: Balancing success
------------------------------------------------------------------------------}

{-
prop-balanceTransaction-suceeds
    : ∀ (u : UTxO)
        (partialTx : PartialTx)
        (newAddress : ChangeAddressGen c)
        (c0 : c)
    → exceeds (UTxO.balance u) (totalOut partialTx) ≡ True
    → isJust (balanceTransaction u newAddress c0 partialTx) ≡ True
prop-balanceTransaction-suceeds = {!   !}
-}