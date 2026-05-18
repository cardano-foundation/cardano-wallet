# Implementation Plan: Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Branch**: `006-drop-api-shelley-tx` | **Date**: 2026-05-13 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/006-drop-api-shelley-tx/spec.md`

## Status

- **Completed**: Phase 0 research, Phase 1 design, Phase 2 tasks, implementation pre-flight, and one narrow API-removal slice.
- **Current**: PR #5286 is stacked on #5287 and now removes the legacy signed transaction wrapper from `Shelley/Transaction.hs`.
- **Delivered slice**: `Cardano.Wallet.buildAndSignTransaction` now calls `Cardano.Wallet.Shelley.Transaction.Ledger.mkTransaction`; the old `Cardano.Wallet.Shelley.Transaction.mkTransaction` wrapper and its private decode helper are gone from `Cardano.Wallet.Shelley.Transaction`.
- **Blockers**: The original helper-deletion and full body/signing stories still depend on remaining follow-up work.
  - Story 2 is no longer blocked on mint plumbing: #5287 removed the `mempty -- TODO: minting support` placeholders. The remaining blocker is script-witness parity in the ledger body builder.
  - Story 1 blocked on Story 2 — discovered at implementation time on `dce6abbbf1`. The `*Ledger` cert builders return `[Ledger.TxCert era]`, but `mkUnsignedTransaction` in `Shelley/Transaction.hs` feeds the cert lists into `constructUnsignedTx` (cardano-api body builder) which expects `[ApiCert.Certificate (CardanoApiEra era)]`. Until the body builder is swapped to the ledger variant (Story 2), the cert-builder switch will not compile. Full analysis in `research.md` §G.
  - Story 3 blocked on a ledger-native `signTransaction` rewrite (no `signTransactionLedger` exists; `mkShelleyWitnessLedger`/`mkByronWitnessLedger` already in place).

## Summary

Three vertical slices remove `cardano-api` from `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` and delete two cert-helper modules. Replacements (`certificateFrom{Voting,Delegation}ActionLedger`) already live in `Shelley/Transaction/Ledger.hs` from #5270.

Story 1 (cert helpers) was originally believed unblocked — 5 callsites in `Shelley/Transaction.hs` (the only remaining importer; `Cardano/Wallet.hs` was migrated separately and already uses the `*Ledger` variants). Implementation pre-flight on `dce6abbbf1` proved it is in fact blocked transitively on Story 2: the cert lists feed the cardano-api body builder `constructUnsignedTx`, which requires the cardano-api cert type. See `research.md` §G.

After #5287, one smaller path became safe to move: the signed-wallet path can call `Cardano.Wallet.Shelley.Transaction.Ledger.mkTransaction` directly, because that constructor now preserves mint/burn values. This PR takes that slice and removes the old `Cardano.Wallet.Shelley.Transaction.mkTransaction` wrapper from `Shelley/Transaction.hs`. The unsigned `mkUnsignedTransaction` migration and helper deletion remain gated on script-witness parity.

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
        ├── Transaction.hs                           # remove signed wrapper here; switch 5 cert callsites later
        └── Transaction/Ledger.hs                    # provides ledger-native mkTransaction; receives witness work later
```

**Structure Decision**: Refactor within `lib/wallet/`. No new packages, no new modules. This PR's source slice edits only `Cardano.Wallet` and `Cardano.Wallet.Shelley.Transaction`.

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

- **Story 2 commit** (once unblocked): body-construction rewrite in `mkUnsignedTx` / `mkWithdrawalTx`. The body-builder swap forces the cert-list type to `[Ledger.TxCert era]`, which removes the `*Ledger`-vs-cardano-api type mismatch at the cert callsites. As a consequence, the Story 1 work (delete `Voting.hs` + `Delegation.hs`, switch the 5 cert callsites to `*Ledger` variants, prune cabal `exposed-modules`) folds naturally into this same commit. RED proof is the existing property + golden body-bytes suite for those functions; will be tightened with explicit per-era byte-equality assertions if the existing coverage is insufficient.
- **Signed path commit** (this PR): route `buildAndSignTransaction` to `Cardano.Wallet.Shelley.Transaction.Ledger.mkTransaction`, then delete the obsolete `Cardano.Wallet.Shelley.Transaction.mkTransaction` wrapper. Proof is compile, format, HLint, and the grep guard that the old wrapper no longer exists in the target files.
- **Story 1 commit** (subsumed by Story 2): not a separate slice anymore. If for some reason the team chooses to keep a separate Story 1 PR after Story 2 lands, it would carry only the helper deletion + cabal prune, and the RED proof is the same signing-property suite that today already covers the cert paths indirectly.
- **Story 3 commit** (once unblocked): `signTransaction` rewrite. RED proof is the existing signing-property suite plus on-chain verification tests.
- The cabal `build-depends: cardano-api` entry stays in `cardano-wallet.cabal` until **no module under `lib/wallet/`** imports it; removal lands in whichever slice closes the last importer.

## Complexity Tracking

Not applicable — no constitution violations.
