# Tasks

## Slice S1 — effective DRep comparison

- [x] **T1** Add a RED regression bundle covering historical and effective
  DRep distinctions, with accepted and rejected outcomes.
- [x] **T2** Align pure and IO duplicate-vote decisions with the effective
  delegation state while preserving existing guards.
- [x] **T3** Run focused verification, formatting/lint checks, and submit a
  clean signed candidate for independent audit.

  Static verification and two independent audits are complete. Host-floor
  interlock deferred executable tests, formatting, and lint checks to PR CI.

## Slice S2 — checked Agda backend and QuickCheck mirrors

- [x] **T4** Extend `specifications/Cardano/Wallet/Delegation.agda` with D1/D2,
  F3/F4, and proofs for `AGDA-5350-EMPTY`, `AGDA-5350-LAST`,
  `AGDA-5350-HISTORY`, and `AGDA-5350-SAME` without placeholder postulates.
- [x] **T5** Add a repository-owned, Nix-pinned Agda check and wire it into PR
  CI. Record a negative control in which a deliberate Agda type error makes
  the exact check fail, then restore the valid source.
- [x] **T6** Add and register the one-to-one QuickCheck mirrors named in the
  functions model, retain `prop_joinDRepParityWithVoteRequest`, and record a
  RED control against the historical `active || any next` decision. Register
  `prop_drepEqualityMatchesStructure` for the formal backend's explicit DRep
  equality assumptions.
- [ ] **T7** Run the available non-Nix static gates, obtain CI evidence for the
  Agda check and focused QuickCheck suite under the standing host ruling, and
  submit one clean signed S2 candidate for fresh independent audit.

  The static gate, `git diff --check`, `fourmolu --mode check` and `nixfmt`
  are green locally and one clean signed S2 candidate is submitted. The
  standing no-local-Nix ruling defers the executed evidence to PR CI: no
  Agda binary exists on this host, so neither the model typecheck nor the
  four law mutations of `checks.x86_64-linux.delegation-agda` have been
  executed, and the focused QuickCheck suite has not been run. This task
  stays open until the `Check Delegation Agda Model` job and the
  `wallet-unit / Other Wallet` job report on PR #5363.
