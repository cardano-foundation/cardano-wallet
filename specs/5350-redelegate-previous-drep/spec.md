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

## Rejection behavior

- Reject a request matching the effective DRep with `ErrAlreadyVoted` /
  `same_vote`.
- Do not reject solely because the target appears in `active` or an earlier
  scheduled entry superseded by a later status.
