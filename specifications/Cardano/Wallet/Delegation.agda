{-# OPTIONS --erasure #-}
module
    Cardano.Wallet.Delegation
      (Slot : Set)
      (_<=_ : Slot → Slot → Set)
      (_<_ : Slot → Slot → Set)
      (DRep : Set)
      (Pool : Set)
  where

open import Haskell.Prelude using (_≡_; Maybe; Just; Nothing)
open import Data.Product public using () renaming
  ( _×_ to _⋀_
  )

{-----------------------------------------------------------------------------
    Delegation status
------------------------------------------------------------------------------}

data Status : Set where
  Inactive   : Status
  Active     : Maybe DRep → Maybe Pool → Status 

-- | Transitions between delegation status
data Transition : Set where
  Deregister      : Transition
  VoteAndDelegate : Maybe DRep → Maybe Pool → Transition

insertIfJust : ∀ {a : Set} → Maybe a → Maybe a → Maybe a
insertIfJust (Just x) _ = Just x
insertIfJust Nothing my = my

applyTransition : Transition → Status → Status
applyTransition Deregister _
  = Inactive
applyTransition (VoteAndDelegate da db) Inactive
  = Active da db
applyTransition (VoteAndDelegate da db) (Active a b)
  = Active (insertIfJust da a) (insertIfJust db b)

{-----------------------------------------------------------------------------
    Delegation history
------------------------------------------------------------------------------}

data Operation : Set where
  ApplyTransition : Transition → Slot → Operation
  Rollback : Slot → Operation

record HistoryApi : Set₁ where
  field
    History : Set
    status : Slot → History → Status

    applyOperation : Operation → History → History

open HistoryApi

-- Property that expresses how an operation at a slot
-- changes the History data structure.
setsTheFuture
  : ∀ (api : HistoryApi)
      (op : Slot → Operation)
      (transition : Status → Status)
  → Set
setsTheFuture api op transition =
  ∀ (x : Slot) (history : History api)
  → let old      = status api x history
        history' = applyOperation api (op x) history
    in
      ∀ (y : Slot)
      → let new = status api y history'
        in  (y <  x → new ≡ status api y history)
          ⋀ (x <= y → new ≡ transition old)


record HistoryLaws (api : HistoryApi) : Set₁ where
  field
    prop-transitions
      : ∀ (t : Transition)
      → setsTheFuture api
          (ApplyTransition t)
          (applyTransition t)
    
    prop-rollback
      : setsTheFuture api
          Rollback
          (λ old → old)
