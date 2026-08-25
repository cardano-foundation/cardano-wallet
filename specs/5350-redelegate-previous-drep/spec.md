# Re-delegate to a previously used DRep

## Outcome

A wallet may delegate voting power to a DRep used earlier in its delegation
history when that DRep is not the effective delegation. A request matching the
effective DRep remains rejected as `same_vote`.

## Requirements

- **R1** Determine duplicate voting from the effective delegation: the final
  scheduled `next` status when present, otherwise `active`.
- **R2** Ignore earlier historical entries when deciding whether the requested
  DRep is already effective.
- **R3** Apply the same rule in the pure transaction-building path and the IO
  wallet-layer path.
- **R4** Preserve Conway-era and stake-key registration behavior.
- **R5** Permanently cover A → B → A acceptance and A → B → B rejection,
  including active-only, scheduled quit, and predefined DRep cases where they
  distinguish effective state from history.
- **R6** Extend the repository's existing delegation Agda specification with
  the projected `active`/ordered-`next` state, effective-status selection, and
  duplicate-vote decision. The Agda source is the formal backend for these
  rules; it must typecheck through a repository-owned, Nix-pinned check.
- **R7** Mirror every named #5350 Agda law with an explicitly mapped QuickCheck
  property over the Haskell implementation. The mapping must cover the empty
  `next` fallback, final-`next` selection, history irrelevance, and
  effective-DRep rejection; the existing pure/IO parity property remains
  required. The Agda model's explicit DRep-equality assumptions must likewise
  have a registered QuickCheck mirror against an independently expressed
  structural oracle.
- **R8** Permanently wire both proof surfaces into CI: the Agda check and the
  focused QuickCheck properties. A deliberate ill-typed Agda mutation and a
  deliberate restoration of the historical `active || any next` decision must
  each make their corresponding check fail.

## Invariants

- **INV-5350-EFFECTIVE** (`BLOCKING`): duplicate-vote rejection compares the
  target only with the effective delegation, because the verdict controls
  whether a fee-bearing transaction may be built.
- **INV-5350-ORDER** (`BLOCKING`): when `next` is non-empty, only its final
  ordered status is effective; earlier entries are history.
- **INV-5350-PARITY** (`BLOCKING`): pure and IO paths return the same duplicate
  verdict for the same wallet delegation state.
- **INV-5350-GUARDS** (`BLOCKING`): the change does not weaken era or
  stake-registration guards around voting actions.
- **INV-5350-PROOF** (`ADVISORY`): the regression proof runs through the
  repository unit-test target and is demonstrated RED without the fix and
  GREEN with it.
- **INV-5350-AGDA** (`BLOCKING`): the checked Agda backend defines D1 and proves
  the empty-`next`, final-`next`, history-irrelevance, and effective-DRep
  decision laws without postulates standing in for those laws.
- **INV-5350-MIRROR** (`BLOCKING`): each named Agda law has a named QuickCheck
  mirror that exercises the corresponding Haskell function over generated
  values; no mirror may compare two values derived from the same implementation
  expression or otherwise pass vacuously.
- **INV-5350-WIRING** (`BLOCKING`): CI invokes the repository-owned Agda check
  and the unit-test target invokes every mapped QuickCheck property. Both
  paths have a recorded negative control demonstrating that they can fail.
- **INV-5350-DREP-EQ** (`BLOCKING`): equality used by the duplicate-vote
  decision is reflexive and agrees with the structural identity of `DRep`.
  The formal backend states this reliance explicitly and the Haskell test tree
  permanently checks the actual `Eq DRep` instance against a structural oracle.

## Rejection behavior

- Reject a request matching the effective DRep with `ErrAlreadyVoted` /
  `same_vote`.
- Do not reject solely because the target appears in `active` or an earlier
  scheduled entry superseded by a later status.
