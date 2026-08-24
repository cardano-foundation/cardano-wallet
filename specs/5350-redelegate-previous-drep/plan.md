# Plan

## Constraints

- Reuse draft PR #5363 and its existing branch.
- Treat commit `5366766d1a8dc8f63a88f8bf6f55180eaf9232f7` and the preserved hostile-audit
  worktree as untrusted seed evidence; fresh Grok ownership is required.
- Preserve public API types and database state.
- Do not start a cold full-gate realization on this host. Focused warm tests,
  formatting, and linting are allowed; full verification is delegated to PR CI.
- Keep the established delegation formalism: extend
  `specifications/Cardano/Wallet/Delegation.agda`; do not introduce a new Lean
  project and do not claim Agda-to-Haskell code generation where none exists.
- The Agda toolchain and check must be pinned and owned by the repository. CI
  must not fetch an unpinned installer or depend on `/code/cardano-wallet-agda`.
- The standing no-local-Nix-build ruling remains binding. Static inspection and
  negative-control construction are local; Nix-backed execution is delegated
  to PR CI unless the project owner explicitly lifts that ruling.

## Strategy

1. Rebase the legacy draft onto current `master` through a fresh planning base.
2. Add a RED regression bundle that distinguishes historical from effective
   delegation state and exercises both accepted and rejected outcomes.
3. Make the smallest pure and IO changes needed to select the effective state.
4. Run the focused gate, format/lint checks, and independent audit before push.
5. Extend the existing Agda delegation lineage with the projected effective
   state and duplicate-vote laws, then map each law to a named QuickCheck
   property over the Haskell functions.
6. Add one repository-owned Nix-backed Agda check and invoke it from CI; prove
   its reachability with an ill-typed negative control and prove the QuickCheck
   mirror with the historical decision as its negative control.

## Slice

- **S1 — effective DRep comparison:** tasks T1–T3, one bisect-safe behavior
  commit.
- **S2 — checked formal backend and mirrors:** tasks T4–T7, one follow-up commit
  on PR #5363. The change is additive to S1 and does not reopen its accepted
  production behavior unless a formal/QC mismatch exposes a defect.
