# Implementation Plan: Unsigned Shelley ledger builder migration

**Branch**: `5285-unsigned-ledger` | **Date**: 2026-05-19 | **Spec**: [spec.md](./spec.md)  
**Input**: Feature specification from `/specs/009-unsigned-ledger-builder/spec.md`

## Summary

Migrate `mkUnsignedTransaction` and `mkUnsignedTx` away from `Cardano.createTransactionBody` by routing them through the ledger-native builder that #5288 made script-witness complete. Because `Transaction.Ledger` currently imports `Shelley.Transaction`, first make the unsigned builder/certificate pieces available through an acyclic module boundary. Then switch the remaining certificate call sites to ledger-native cert helpers, delete the obsolete cardano-api helper modules, and prune their cabal exposed-module entries.

## Technical Context

**Language/Version**: Haskell, GHC pinned by the repository flake  
**Primary Dependencies**: `cardano-ledger-api`, `cardano-balance-tx`, existing `cardano-api` conversion utilities  
**Storage**: N/A  
**Testing**: HSpec / QuickCheck through `cardano-wallet-unit:unit`  
**Target Platform**: multi-platform library code, verified locally on Linux  
**Project Type**: Haskell monorepo under `lib/`  
**Performance Goals**: preserve existing transaction bytes; no throughput target  
**Constraints**: no `lib/integration/**` edits, no signing rewrite, no final `cardano-api` dependency removal, Dijkstra remains out of scope  
**Scale/Scope**: one behavior-changing slice touching wallet transaction modules, Shelley transaction unit specs, and cabal module exposure

## Constitution Check

| Principle | Status | Notes |
|---|---|---|
| Maintenance-first stability | OK | Uses the builder proven by #5288 and preserves existing public shape. |
| Era-aware design | OK | Conway path only; Dijkstra remains deferred to #5209. |
| Type safety as security | OK | Moves cert flow to `[TxCert era]`; avoids API-to-ledger conversion helpers. |
| Reproducible builds | OK | `gate.sh` runs under `nix develop --quiet`. |
| Comprehensive testing | OK | Existing golden/binary and #5288 parity specs remain in scope. |
| Code quality gates | OK | Format, build, focused tests, and HLint are in `gate.sh`. |

No justified violations.

## Project Structure

```text
specs/009-unsigned-ledger-builder/
├── spec.md
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── tasks.md
├── briefs/
│   └── T010-T013.md
└── checklists/
    └── requirements.md
```

Expected source/test write set for the behavior slice:

```text
lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Build.hs
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/<new-acyclic-module>.hs (if needed)
lib/wallet/src/Cardano/Wallet/Transaction/Delegation.hs (delete)
lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs (delete)
lib/wallet/cardano-wallet.cabal
lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs
lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs
```

Forbidden write set:

```text
lib/integration/**
```

## Vertical Slice Contract

One serial worker implements one bisect-safe behavior-changing commit:

`feat(5285): migrate unsigned Shelley tx builder`

The commit must contain:

1. RED proof in Shelley transaction unit tests for the desired ledger-cert / ledger-builder path.
2. Acyclic shared ledger builder boundary to avoid `Shelley.Transaction` importing `Transaction.Ledger`.
3. `mkUnsignedTransaction` / `mkUnsignedTx` migrated off `createTransactionBody` and `TxBodyContent`.
4. Remaining cert call sites switched to `certificateFromDelegationActionLedger` / `certificateFromVotingActionLedger`.
5. Deleted `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting`; cabal pruned.
6. `./gate.sh` green, with no `lib/integration/**` diff.

If the worker cannot make this compile within three serious implementation attempts, it must stop and report the blocker in `WIP.md`; do not split the commit without orchestrator review.

## Proof Strategy

Development commands:

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Wallet.Shelley.Transaction"'
```

Acceptance command:

```sh
./gate.sh
```

The orchestrator reruns `./gate.sh` after the worker returns and checks:

```sh
rg -n "createTransactionBody|TxBodyContent" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs
rg -n "Cardano.Wallet.Transaction.(Delegation|Voting)" lib/wallet lib/unit
git diff --name-only origin/master...HEAD | rg '^lib/integration/'
```

The first two must have no relevant matches for this ticket's removed surface; the integration diff must be empty.

## Risks And Mitigations

- **Import cycle**: avoid by moving shared body construction below both callers.
- **Test weakening**: preserve existing assertions; update tests only to follow the new builder/cert types.
- **Scope creep into signing**: do not rewrite `signTransaction`; leave #5289 intact.
- **Final dependency removal too early**: do not remove package-level `cardano-api`; leave #5290 intact.
