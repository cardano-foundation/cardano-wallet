# Implementation Plan: Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Branch**: `006-drop-api-shelley-tx` | **Date**: 2026-05-13 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/006-drop-api-shelley-tx/spec.md`

## Status

- **Completed**: Phase 0 research, Phase 1 design.
- **Current**: Awaiting plan review, then tasks.
- **Blockers**: None for Story 1. Story 2 blocked on `Transaction.Ledger` minting + script-witness support (`mempty -- TODO: minting support` at `Shelley/Transaction/Ledger.hs:433,496`; pervasive cardano-api coupling acknowledged at `Ledger.hs:47-49`). Story 3 blocked on a ledger-native `signTransaction` rewrite (no `signTransactionLedger` exists; `mkShelleyWitnessLedger`/`mkByronWitnessLedger` already in place).

## Summary

Three vertical slices remove `cardano-api` from `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` and delete two cert-helper modules. Replacements (`certificateFrom{Voting,Delegation}ActionLedger`) already live in `Shelley/Transaction/Ledger.hs` from #5270.

Story 1 (cert helpers) is unblocked and mechanical — 5 callsites in `Shelley/Transaction.hs` (the only remaining importer; `Cardano/Wallet.hs` was migrated separately and already uses the `*Ledger` variants).

Stories 2 and 3 are gated on prerequisite Ledger-side work tracked under parent #5243; this plan documents their shape so they can be scheduled, not implemented here.

## Technical Context

**Language/Version**: Haskell, GHC 9.6 (per cabal.project; node 11.0.1 target).
**Primary Dependencies**: `cardano-ledger-*`, `cardano-wallet-read`, `cardano-api` (being removed).
**Storage**: N/A — this is a refactor of in-memory transaction-construction code.
**Testing**: Hspec + QuickCheck for unit/property; golden CBOR vectors for body bytes; integration against local cardano-node clusters.
**Target Platform**: Linux musl static, macOS (Intel + Apple Silicon), Windows cross-compiled — must not regress on any.
**Project Type**: Library refactor inside a multi-package Cabal monorepo (`lib/wallet/`).
**Performance Goals**: Byte-equivalent output; no measurable change in build time, restoration benchmark, or API latency benchmark.
**Constraints**: `-Wall -Werror` in release; Fourmolu formatting; HLint clean; era-aware (Babbage / Conway / Dijkstra).
**Scale/Scope**: ~1500 lines in `Shelley/Transaction.hs`; ~120 lines each in `Voting.hs` / `Delegation.hs`; 7 callsites across 2 files for Story 1.

## Constitution Check

Gates from `.specify/memory/constitution.md`:

| Principle | Status | Notes |
|---|---|---|
| I. Maintenance-First Stability | PASS | Behaviour-preserving refactor; no semantic change. SC-003 enforces byte-equivalence over existing suites. |
| II. Era-Aware Design | PASS | `Transaction.Ledger` builders are `RecentEra era` parameterised; replacements match. |
| III. Type Safety as Security | PASS | Switches from cardano-api typed-by-`era` to ledger `TxCert era` — equally typeful. |
| IV. Formal Specification | N/A | No public API change; no OpenAPI surface touched. |
| V. Reproducible Builds | PASS | Cabal-only changes (no new deps; removes one dep stanza-by-stanza). |
| VI. Comprehensive Testing | PASS | Existing unit + property + golden suites cover the affected paths (`TransactionSpec.hs`, `TransactionLedgerSpec.hs`). No new tests required unless coverage gap surfaces. |
| VII. Code Quality Gates | PASS | Standard local CI (Fourmolu, HLint, `cabal build`, unit tests) gates every slice. |

No violations. Complexity tracking section omitted.

## Project Structure

### Documentation (this feature)

```text
specs/006-drop-api-shelley-tx/
├── plan.md              # this file
├── research.md          # Phase 0: cardano-api inventory + replacement mapping
├── data-model.md        # Phase 1: domain type mapping (cardano-api → ledger)
├── quickstart.md        # Phase 1: implementation walkthrough for Story 1
├── contracts/
│   └── module-interface-invariants.md  # which exports MUST stay byte-identical
├── checklists/
│   └── requirements.md  # spec quality checklist (from speckit-specify)
└── tasks.md             # Phase 2 (speckit-tasks)
```

### Source Code (touched paths)

```text
lib/wallet/
├── cardano-wallet.cabal                            # prune exposed-modules; eventually drop build-depends
└── src/Cardano/Wallet/
    ├── Transaction/
    │   ├── Voting.hs                                # DELETE in Story 1
    │   └── Delegation.hs                            # DELETE in Story 1
    └── Shelley/
        ├── Transaction.hs                           # switch 5 callsites in Story 1; body+sign in Stories 2/3
        └── Transaction/Ledger.hs                    # READ-ONLY here; receives mint/witness/sign work as separate tickets
```

**Structure Decision**: Refactor within `lib/wallet/`. No new packages, no new modules. Two modules deleted, two modules edited per slice.

## Phase 0 (Research) — Summary

Full report in `research.md`. Headlines:

- `Shelley/Transaction.hs` has 4 distinct `Cardano.Api*` imports across ~40 symbol references. 2 of those references (cert builders) are replaced in Story 1; the remaining ~38 are body-construction (~25 symbols) and signing/witness (~13 symbols) and belong to Stories 2 and 3.
- `Voting.hs` and `Delegation.hs` are each ~120 lines with one exported function. Their internal cardano-api dependencies (`makeStakeAddress*Certificate`, `StakeCredentialBy*`, `ConwayEraOnwards*`) are fully encapsulated by the existing `*Ledger` replacements.
- The two helpers are called from **one** file: `Shelley/Transaction.hs` (lines 400, 778, 784, 849, 855). `Cardano/Wallet.hs` was already migrated to the `*Ledger` variants in earlier work — its 4 callsites at 2729/2738/3533/3542 already use the new names. Story 1 only edits `Shelley/Transaction.hs`.
- Test coverage is concentrated in `lib/unit/test/unit/Cardano/Wallet/Shelley/{Transaction,TransactionLedger}Spec.hs`. No dedicated tests for the cert helpers — they are exercised indirectly via signing-property tests.
- Story 2 prerequisite is acknowledged in code at `Ledger.hs:47-49` (TODO comment) and `Ledger.hs:433,496` (minting stubbed `mempty`).

## Phase 1 (Design) — Summary

- `data-model.md`: symbol-by-symbol map cardano-api → ledger replacement, grouped by Story.
- `contracts/module-interface-invariants.md`: enumerates the exported names of `Cardano.Wallet.Transaction` (out of scope), `Cardano.Wallet.Shelley.Transaction`, and `Cardano.Wallet.Shelley.Transaction.Ledger` that MUST keep their signatures unchanged. Each slice's diff is constrained to "stop importing cardano-api"; export lists are invariants.
- `quickstart.md`: ordered walkthrough for Story 1 (the unblocked slice) including the exact `Edit` operations on the 7 callsites, cabal pruning, and the `cabal test cardano-wallet-unit` command to confirm green.

## Vertical Slice Contract

Per project PR policy (RED + GREEN in one bisect-safe commit):

- **Story 1 commit** carries: deletion of `Voting.hs` + `Delegation.hs`, all 5 callsite updates in `Shelley/Transaction.hs`, cabal `exposed-modules` prune. Existing property/golden suites for cert construction serve as the RED proof — they pass pre-change and post-change because the `*Ledger` builders are byte-equivalent. No new tests required.
- **Story 2 commit** (once unblocked): body-construction rewrite in `mkUnsignedTx` / `mkWithdrawalTx`. RED proof is the existing property + golden body-bytes suite for those functions; will be tightened with explicit per-era byte-equality assertions if the existing coverage is insufficient.
- **Story 3 commit** (once unblocked): `signTransaction` rewrite. RED proof is the existing signing-property suite plus on-chain verification tests.
- The cabal `build-depends: cardano-api` entry stays in `cardano-wallet.cabal` until **no module under `lib/wallet/`** imports it; removal lands in whichever slice closes the last importer.

## Complexity Tracking

Not applicable — no constitution violations.
