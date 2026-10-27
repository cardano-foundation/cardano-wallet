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

isJust : ∀ {a : Set} → Maybe a → Bool
isJust Nothing = False
isJust (Just _) = True

just-injective : ∀ {a : Set} {x y : a} → Just x ≡ Just y → x ≡ y
just-injective refl = refl

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

{-----------------------------------------------------------------------------
    Issue #5466 — decisions from the vote delegation that the ledger holds

    A DRep's retirement removes every vote delegation to it from the ledger,
    so the wallet's history can record a vote that the ledger no longer holds,
    and from protocol version 10 the ledger refuses a withdrawal from a
    key-hash reward account that holds no vote delegation.  Two decisions
    therefore follow the ledger's vote delegation when the wallet has read
    it, and the wallet's history only when it has not: the withdrawal
    decision F11 below, and the duplicate-vote decision F12 in the
    'DuplicateVote' module.  Each named law has a QuickCheck mirror over the
    Haskell implementation, listed in
    specs/5466-ledger-vote-delegation/functions-model.md:

      AGDA-5466-WDRL-LEDGER → prop_withdrawalPermittedFollowsLedger
      AGDA-5466-WDRL-EXEMPT → prop_withdrawalPermittedExempt
      AGDA-5466-WDRL-UNREAD → prop_withdrawalPermittedUnreadFollowsHistory
      AGDA-5466-VOTE-LEDGER → prop_ledgerVoteRequestFollowsLedger
      AGDA-5466-VOTE-UNREAD → prop_ledgerVoteRequestUnreadIs5350

    The three withdrawal laws partition the inputs: WDRL-EXEMPT covers the
    bootstrap phase and script-hash accounts, and the other two cover a
    key-hash account after the bootstrap phase, split on whether the ledger
    value has been read.
------------------------------------------------------------------------------}

-- | D6 — the ledger's vote delegation for a reward account, as the wallet
-- observes it.  'Unread' stands for a value that the wallet has not read, or
-- an era before Conway; 'NoDelegation' for a ledger that holds no vote
-- delegation for the account; 'DelegatedTo' for a delegation to a DRep
-- credential, to Abstain or to No Confidence.
data LedgerVote : Set where
  Unread       : LedgerVote
  NoDelegation : LedgerVote
  DelegatedTo  : DRep → LedgerVote

-- | D6 — whether the protocol version is in the Conway bootstrap phase.
data Phase : Set where
  Bootstrap     : Phase
  PostBootstrap : Phase

-- | D6 — the kind of credential of a reward account.
data CredKind : Set where
  KeyHashCred    : CredKind
  ScriptHashCred : CredKind

-- | F11 — the withdrawal decision, with the history's vote given as the DRep
-- of the latest delegation status, if any.  Its clauses follow the guards of
-- the Haskell 'withdrawalPermitted' in order: permitted in the bootstrap
-- phase; permitted for a script-hash reward account; decided by a read
-- ledger value alone, one clause per constructor; and decided by the
-- history when the ledger value is unread.
withdrawalDecision : Phase → CredKind → LedgerVote → Maybe DRep → Bool
withdrawalDecision Bootstrap credKind ledgerVote history = True
withdrawalDecision PostBootstrap ScriptHashCred ledgerVote history = True
withdrawalDecision PostBootstrap KeyHashCred NoDelegation history = False
withdrawalDecision PostBootstrap KeyHashCred (DelegatedTo d) history = True
withdrawalDecision PostBootstrap KeyHashCred Unread history = isJust history

-- The predicates below are defined independently of 'withdrawalDecision',
-- so that the laws relate the definition to a specification rather than to
-- itself.

-- | A ledger value that the wallet has read.
data IsRead : LedgerVote → Set where
  readNone      : IsRead NoDelegation
  readDelegated : ∀ (d : DRep) → IsRead (DelegatedTo d)

-- | A ledger value that holds a vote delegation.
data IsDelegated : LedgerVote → Set where
  delegated : ∀ (d : DRep) → IsDelegated (DelegatedTo d)

-- | The phases and credential kinds that the ledger exempts from holding a
-- vote delegation: every account in the bootstrap phase, and a script-hash
-- account in every phase.
data IsExempt : Phase → CredKind → Set where
  bootstrapPhase : ∀ (c : CredKind) → IsExempt Bootstrap c
  scriptHash     : ∀ (p : Phase) → IsExempt p ScriptHashCred

-- | A history that records a vote.
data HoldsVote : Maybe DRep → Set where
  holdsVote : ∀ (d : DRep) → HoldsVote (Just d)

-- | AGDA-5466-WDRL-LEDGER — after the bootstrap phase, a key-hash reward
-- account whose ledger value has been read may withdraw exactly when the
-- ledger holds a vote delegation for it, whatever the history records.  A
-- decision from the history alone violates this law once the DRep that the
-- history names has retired.
AGDA-5466-WDRL-LEDGER
  : ∀ (v : LedgerVote) (history : Maybe DRep)
  → IsRead v
  → ( IsDelegated v
      → withdrawalDecision PostBootstrap KeyHashCred v history ≡ True
    )
    ⋀
    ( (IsDelegated v → Empty)
      → withdrawalDecision PostBootstrap KeyHashCred v history ≡ False
    )
AGDA-5466-WDRL-LEDGER _ history readNone =
  (λ ()) , (λ notDelegated → refl)
AGDA-5466-WDRL-LEDGER _ history (readDelegated d) =
  (λ isDelegated → refl) , (λ notDelegated → absurd (notDelegated (delegated d)))

-- | AGDA-5466-WDRL-EXEMPT — every account in the bootstrap phase, and a
-- script-hash account in every phase, may withdraw, whatever the ledger
-- value and the history are.
AGDA-5466-WDRL-EXEMPT
  : ∀ (p : Phase) (c : CredKind) (v : LedgerVote) (history : Maybe DRep)
  → IsExempt p c
  → withdrawalDecision p c v history ≡ True
AGDA-5466-WDRL-EXEMPT _ _ v history (bootstrapPhase c) = refl
AGDA-5466-WDRL-EXEMPT _ _ v history (scriptHash Bootstrap) = refl
AGDA-5466-WDRL-EXEMPT _ _ v history (scriptHash PostBootstrap) = refl

-- | AGDA-5466-WDRL-UNREAD — after the bootstrap phase, a key-hash reward
-- account whose ledger value is unread may withdraw exactly when the history
-- records a vote.
AGDA-5466-WDRL-UNREAD
  : ∀ (history : Maybe DRep)
  → ( HoldsVote history
      → withdrawalDecision PostBootstrap KeyHashCred Unread history ≡ True
    )
    ⋀
    ( (HoldsVote history → Empty)
      → withdrawalDecision PostBootstrap KeyHashCred Unread history ≡ False
    )
AGDA-5466-WDRL-UNREAD Nothing =
  (λ ()) , (λ noVote → refl)
AGDA-5466-WDRL-UNREAD (Just d) =
  (λ voted → refl) , (λ noVote → absurd (noVote (holdsVote d)))

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

  -- | F12 — the duplicate-vote decision from the ledger's vote delegation.
  -- Its clauses follow the Haskell 'ledgerVoteRequestFor' in order: a
  -- delegation that the ledger holds is compared with the target; a ledger
  -- that holds no vote delegation makes every request a different vote; and
  -- an unread value leaves the decision to F4.
  ledgerVoteDecision : DRep → LedgerVote → Delegation → VoteDecision
  ledgerVoteDecision t (DelegatedTo d) dlg = decideAgainst t (Just d)
  ledgerVoteDecision t NoDelegation dlg = DifferentVote
  ledgerVoteDecision t Unread dlg = voteDecision t dlg

  -- | AGDA-5466-VOTE-LEDGER — a read ledger value decides alone: the request
  -- repeats the vote delegation in force exactly when the ledger holds a
  -- delegation to the target, whatever the history records.
  AGDA-5466-VOTE-LEDGER
    : ∀ (t : DRep) (v : LedgerVote) (d : Delegation)
    → IsRead v
    → (v ≡ DelegatedTo t → ledgerVoteDecision t v d ≡ SameVote)
      ⋀
      ( (v ≡ DelegatedTo t → Empty)
        → ledgerVoteDecision t v d ≡ DifferentVote
      )
  AGDA-5466-VOTE-LEDGER t _ d readNone =
    (λ ()) , (λ different → refl)
  AGDA-5466-VOTE-LEDGER t _ d (readDelegated e) = rejects , accepts
    where
      rejects
        : DelegatedTo e ≡ DelegatedTo t
        → ledgerVoteDecision t (DelegatedTo e) d ≡ SameVote
      rejects same =
        trans
          (cong (λ v → ledgerVoteDecision t v d) same)
          (decideAgainst-refl t)

      accepts
        : (DelegatedTo e ≡ DelegatedTo t → Empty)
        → ledgerVoteDecision t (DelegatedTo e) d ≡ DifferentVote
      accepts different =
        decideAgainst-differs
          t
          (Just e)
          (λ same → different (cong DelegatedTo (just-injective same)))

  -- | AGDA-5466-VOTE-UNREAD — while the ledger value is unread, the decision
  -- is F4's, taken from the effective delegation status alone.
  AGDA-5466-VOTE-UNREAD
    : ∀ (t : DRep) (d : Delegation)
    → ledgerVoteDecision t Unread d ≡ voteDecision t d
  AGDA-5466-VOTE-UNREAD t d = refl
