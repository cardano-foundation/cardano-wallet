{-# OPTIONS --erasure #-}
module
    Cardano.Wallet.Delegation
      (Slot : Set)
      (_<=_ : Slot → Slot → Set)
      (_<_ : Slot → Slot → Set)
      (DRep : Set)
      (Pool : Set)
  where

{-----------------------------------------------------------------------------
    Prelude

    This module is deliberately library-free: it imports nothing and defines
    the handful of types and equality combinators that it needs.  That way the
    repository-owned, Nix-pinned check
    (`checks.x86_64-linux.delegation-agda`) typechecks this file with a plain
    Agda and no library resolution at all — no agda2hs, no standard library,
    and no unpinned installer.
------------------------------------------------------------------------------}

data _≡_ {a : Set} (x : a) : a → Set where
  refl : x ≡ x

infix 4 _≡_

sym : ∀ {a : Set} {x y : a} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {a : Set} {x y z : a} → x ≡ y → y ≡ z → x ≡ z
trans refl q = q

cong : ∀ {a b : Set} (f : a → b) {x y : a} → x ≡ y → f x ≡ f y
cong f refl = refl

data Empty : Set where

absurd : ∀ {a : Set} → Empty → a
absurd ()

data Bool : Set where
  True  : Bool
  False : Bool

data Maybe (a : Set) : Set where
  Nothing : Maybe a
  Just    : a → Maybe a

data List (a : Set) : Set where
  []  : List a
  _∷_ : a → List a → List a

infixr 5 _∷_

_++_ : ∀ {a : Set} → List a → List a → List a
[] ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixr 5 _++_

record _⋀_ (a b : Set) : Set where
  constructor _,_
  field
    fst : a
    snd : b

infixr 2 _⋀_

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

{-----------------------------------------------------------------------------
    Issue #5350 — effective delegation and the duplicate-vote decision

    A wallet may re-delegate its voting power to a DRep that occurs earlier in
    its delegation history, as long as that DRep is not the *effective*
    delegation.  The named laws below are the formal backend for that rule;
    each one has a QuickCheck mirror over the Haskell implementation, listed in
    specs/5350-redelegate-previous-drep/functions-model.md:

      AGDA-5350-EMPTY   → prop_effectiveDelegationStatusEmpty
      AGDA-5350-LAST    → prop_effectiveDelegationStatusLast
      AGDA-5350-HISTORY → prop_voteDecisionIgnoresHistory
      AGDA-5350-SAME    → prop_joinDRepVotingActionEffective

    The DRep-equality parameters below (eq-refl, eq-sound) are model
    assumptions rather than #5350 laws; their Haskell mirror is the
    registered prop_drepEqualityMatchesStructure.
------------------------------------------------------------------------------}

-- | D1 — the projected delegation state that the wallet reports: the
-- currently active status together with the ordered scheduled statuses.
-- Epochs are abstracted away, but the ordered-list boundary of
-- 'WalletDelegation' is preserved.
record Delegation : Set where
  constructor MkDelegation
  field
    active : Status
    next   : List Status

-- | D1 projections.  ‘activeStatus’ is what the pre-#5350 decision looked
-- at directly; the negative control in nix/delegation-agda.nix mutates
-- ‘voteDecision’ back onto it.
activeStatus : Delegation → Status
activeStatus (MkDelegation a _) = a

nextStatuses : Delegation → List Status
nextStatuses (MkDelegation _ ss) = ss

-- | The final element of a non-empty run of scheduled statuses.
lastStatus : Status → List Status → Status
lastStatus s [] = s
lastStatus s (t ∷ ts) = lastStatus t ts

-- | F3 — the effective delegation status: the final scheduled status when the
-- schedule is non-empty, otherwise the active status.
effectiveDelegationStatus : Delegation → Status
effectiveDelegationStatus (MkDelegation a []) = a
effectiveDelegationStatus (MkDelegation a (s ∷ ss)) = lastStatus s ss

-- | AGDA-5350-EMPTY — an empty schedule selects the active status.
AGDA-5350-EMPTY
  : ∀ (d : Delegation)
  → nextStatuses d ≡ []
  → effectiveDelegationStatus d ≡ activeStatus d
AGDA-5350-EMPTY (MkDelegation a []) scheduleIsEmpty = refl
AGDA-5350-EMPTY (MkDelegation a (s ∷ ss)) ()

-- | @IsLast x s ss@ holds when @x@ is the final element of @s ∷ ss@.
-- It is defined independently of 'lastStatus', so that AGDA-5350-LAST relates
-- the implementation to a specification rather than to itself.
data IsLast (x : Status) : Status → List Status → Set where
  here
    : IsLast x x []
  there
    : ∀ {s t : Status} {ts : List Status}
    → IsLast x t ts
    → IsLast x s (t ∷ ts)

-- | AGDA-5350-LAST — a non-empty schedule selects its final status, whatever
-- the active status and the earlier scheduled statuses are.
AGDA-5350-LAST
  : ∀ (a : Status) {x s : Status} {ss : List Status}
  → IsLast x s ss
  → effectiveDelegationStatus (MkDelegation a (s ∷ ss)) ≡ x
AGDA-5350-LAST a here = refl
AGDA-5350-LAST a (there p) = AGDA-5350-LAST a p

-- | Appending a final scheduled status makes it the effective status.
lastStatus-snoc
  : ∀ (s : Status) (ss : List Status) (final : Status)
  → lastStatus s (ss ++ (final ∷ [])) ≡ final
lastStatus-snoc s [] final = refl
lastStatus-snoc s (t ∷ ts) final = lastStatus-snoc t ts final

-- | Neither the active status nor any superseded scheduled status can affect
-- the effective status once a later status is scheduled.
effective-snoc
  : ∀ (a : Status) (hs : List Status) (final : Status)
  → effectiveDelegationStatus (MkDelegation a (hs ++ (final ∷ []))) ≡ final
effective-snoc a [] final = refl
effective-snoc a (h ∷ hs) final = lastStatus-snoc h hs final

-- | The DRep carried by a delegation status, if any.
statusDRep : Status → Maybe DRep
statusDRep Inactive = Nothing
statusDRep (Active mdrep _) = mdrep

-- | D2 — the duplicate-vote observation.
data VoteDecision : Set where
  SameVote      : VoteDecision
  DifferentVote : VoteDecision

-- | The duplicate-vote decision needs to compare two 'DRep's.  'DRep' is
-- abstract here, so its equality is a module parameter together with its
-- correctness laws — exactly as the surrounding module abstracts the order on
-- 'Slot'.  Nothing about issue #5350 is assumed.
module DuplicateVote
    (_==_ : DRep → DRep → Bool)
    (eq-sound : ∀ {x y : DRep} → (x == y) ≡ True → x ≡ y)
    (eq-refl : ∀ (x : DRep) → (x == x) ≡ True)
  where

  chooseVote : Bool → VoteDecision
  chooseVote True = SameVote
  chooseVote False = DifferentVote

  decideAgainst : DRep → Maybe DRep → VoteDecision
  decideAgainst t Nothing = DifferentVote
  decideAgainst t (Just d) = chooseVote (t == d)

  -- | F4 — the duplicate-vote decision.  It is derived from F3 alone, so the
  -- history is not in scope of the decision by construction.
  voteDecision : DRep → Delegation → VoteDecision
  voteDecision t d =
    decideAgainst t (statusDRep (effectiveDelegationStatus d))

  decideAgainst-refl
    : ∀ (t : DRep)
    → decideAgainst t (Just t) ≡ SameVote
  decideAgainst-refl t = cong chooseVote (eq-refl t)

  decideAgainst-differs
    : ∀ (t : DRep) (md : Maybe DRep)
    → (md ≡ Just t → Empty)
    → decideAgainst t md ≡ DifferentVote
  decideAgainst-differs t Nothing different = refl
  decideAgainst-differs t (Just d) different = go (t == d) refl
    where
      go
        : ∀ (b : Bool)
        → (t == d) ≡ b
        → chooseVote (t == d) ≡ DifferentVote
      go True observed =
        absurd (different (cong Just (sym (eq-sound observed))))
      go False observed = cong chooseVote observed

  -- | AGDA-5350-HISTORY — replacing the active status and every superseded
  -- scheduled status, while keeping the final scheduled status, cannot change
  -- the decision.  This is the law that the pre-#5350
  -- @active || any next@ decision violates.
  AGDA-5350-HISTORY
    : ∀ (t : DRep) (a b : Status) (hs ks : List Status) (final : Status)
    → voteDecision t (MkDelegation a (hs ++ (final ∷ [])))
      ≡ voteDecision t (MkDelegation b (ks ++ (final ∷ [])))
  AGDA-5350-HISTORY t a b hs ks final =
    cong
      (λ s → decideAgainst t (statusDRep s))
      (trans (effective-snoc a hs final) (sym (effective-snoc b ks final)))

  -- | AGDA-5350-SAME — the request is a duplicate vote exactly when the
  -- target equals the effective DRep: rejected in that case, accepted
  -- otherwise.
  AGDA-5350-SAME
    : ∀ (t : DRep) (d : Delegation)
    → ( (statusDRep (effectiveDelegationStatus d) ≡ Just t)
        → voteDecision t d ≡ SameVote
      )
      ⋀
      ( ((statusDRep (effectiveDelegationStatus d) ≡ Just t) → Empty)
        → voteDecision t d ≡ DifferentVote
      )
  AGDA-5350-SAME t d = rejects , accepts
    where
      rejects
        : (statusDRep (effectiveDelegationStatus d) ≡ Just t)
        → voteDecision t d ≡ SameVote
      rejects same =
        trans (cong (decideAgainst t) same) (decideAgainst-refl t)

      accepts
        : ((statusDRep (effectiveDelegationStatus d) ≡ Just t) → Empty)
        → voteDecision t d ≡ DifferentVote
      accepts different =
        decideAgainst-differs
          t
          (statusDRep (effectiveDelegationStatus d))
          different
