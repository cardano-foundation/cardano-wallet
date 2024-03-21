{-# OPTIONS --erasure #-}

-- Experimental wallet implementation
-- with proofs for the specification.
module Cardano.WalletExperiment where

open import Haskell.Prelude
open import Haskell.Data.Maybe
import Haskell.Data.Map as Map

{-----------------------------------------------------------------------------
    Assumptions
------------------------------------------------------------------------------}
Address = Nat
Slot = Nat
TxId = Nat

Value = Integer
Customer = Nat

{-----------------------------------------------------------------------------
    Type definition
------------------------------------------------------------------------------}
record WalletState : Set where
  field
    addresses : Map.Map Address Customer
open WalletState

{-----------------------------------------------------------------------------
    Mapping between Customers and Address
------------------------------------------------------------------------------}
-- Operations

-- Helper function
swap : ∀ {a b : Set} → a × b → b × a
swap (x , y) = (y , x)

listCustomers : WalletState → List (Customer × Address)
listCustomers = map swap ∘ Map.toAscList ∘ addresses

deriveAddress : Nat → Address
deriveAddress ix = suc ix

createAddress : Customer → WalletState → (Address × WalletState)
createAddress c s0 = ( addr , s1 )
  where
    addr = deriveAddress c
    s1 = record { addresses = Map.insert addr c (addresses s0) }

-- Properties

-- Specification
knownCustomerAddress : Address → WalletState → Bool
knownCustomerAddress address = elem address ∘ map snd ∘ listCustomers

-- alternate definition of knownCustomerAddress
knownCustomerAddress' : Address → WalletState → Bool
knownCustomerAddress' address =
    elem address ∘ map fst ∘ Map.toAscList ∘ addresses

-- alternate definition and original definition are equal
lemma-known-known'
  : ∀ (a : Address) (s : WalletState)
  → knownCustomerAddress a s ≡ knownCustomerAddress' a s
lemma-known-known' a s =
  begin
    (elem a ∘ map snd ∘ listCustomers) s
  ≡⟨⟩
    (elem a ∘ map snd ∘ map swap ∘ Map.toAscList ∘ addresses) s
  ≡⟨ cong (elem a) (sym (map-∘ snd swap (Map.toAscList (addresses s)))) ⟩
    (elem a ∘ map (snd ∘ swap) ∘ Map.toAscList ∘ addresses) s
  ≡⟨⟩
    (elem a ∘ map fst ∘ Map.toAscList ∘ addresses) s
  ∎

-- lemma about converting == to ≡
lemma-lookup-insert-same
    : ∀ (a : Address) (c : Customer) (m : Map.Map Address Customer)
    → Map.lookup a (Map.insert a c m) ≡ Just c
lemma-lookup-insert-same a c m =
  begin
    Map.lookup a (Map.insert a c m)
  ≡⟨ Map.prop-lookup-insert a a c m ⟩
    (if (a == a) then Just c else Map.lookup a m)
  ≡⟨ Map.lemma-if-True a a (equality' a a refl) ⟩
    Just c
  ∎

-- Specification
prop-create-known
  : ∀ (c : Customer) (s0 : WalletState)
  → let (address , s1) = createAddress c s0
    in  knownCustomerAddress address s1 ≡ True
prop-create-known c s0 =
  let (a , s1) = createAddress c s0
  in
    begin
      knownCustomerAddress a s1
    ≡⟨ lemma-known-known' a s1 ⟩
      knownCustomerAddress' a s1
    ≡⟨ Map.prop-lookup-toAscList-Just a c (addresses s1)
        (lemma-lookup-insert-same a c (addresses s0))
      ⟩
      True
    ∎

{-----------------------------------------------------------------------------
    Address derivation
------------------------------------------------------------------------------}

-- Specification
prop-create-derive
  : ∀ (c : Customer) (s0 : WalletState)
  → let (address , _) = createAddress c s0
    in  deriveAddress c ≡ address
prop-create-derive = λ c s0 → refl
