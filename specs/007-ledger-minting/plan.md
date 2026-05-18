# Implementation Plan: Ledger Body Builder Minting Support

**Branch**: `007-ledger-minting` | **Date**: 2026-05-13 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification at `/code/cardano-wallet-5243/specs/007-ledger-minting/spec.md`

## Summary

Plumb a mint specification through `buildLedgerTx` and `buildLedgerTxRaw`, translate the wallet's `(mint :: TokenMap, burn :: TokenMap)` pair to a single signed ledger `MultiAsset`, and feed the real value from `mkTransactionLedger` and `constructUnsignedTxLedger`. The two TODO sites in `Ledger.hs` (lines 433, 496) become parameters. No script witnesses, no `mkUnsignedTransaction` migration; those live in follow-up features.

## Technical Context

**Language/Version**: Haskell (GHC 9.6, per `cabal.project`).
**Primary Dependencies**: `cardano-ledger-api`, `cardano-ledger-mary` (`MultiAsset`, `mintTxBodyL`), in-repo `primitive`, `wallet`.
**Storage**: N/A (transaction body construction, no persistence).
**Testing**: `hspec` + `QuickCheck`, in `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`.
**Target Platform**: Linux (musl), Windows (cross), macOS (Intel + Apple Silicon) — unchanged.
**Project Type**: Haskell library (`cardano-wallet:lib:wallet`).
**Performance Goals**: Inherits the body-builder's existing cost profile; the translation is O(#policies × #assets) over a typically tiny map, so no new performance concern.
**Constraints**: Information-preserving translation; empty input → empty mint field; existing callers unchanged on the empty path.
**Scale/Scope**: ~3 source modules touched (`Ledger.hs`, `Convert.hs`, `Wallet.hs`), one test module extended.

## Constitution Check

*GATE: must pass before Phase 0; re-checked after Phase 1.*

| Principle | Status | Note |
|---|---|---|
| I. Maintenance-First Stability | ✓ | Smallest possible plumbing; new behaviour is unreachable in production until the still-blocked `mkUnsignedTransaction` migration lands. |
| II. Era-Aware Design | ✓ | Follows the existing `IsRecentEra era => RecentEra era ->` shape; only `RecentEraConway` is implemented, matching `mkLedgerTx`'s current support. `Dijkstra` continues to error. |
| III. Type Safety as Security | ✓ | New translation is a total function; no runtime checks, no partiality, no script-witness handling. |
| IV. Formal Specification | ✓ | No OpenAPI surface change; no Lean spec affected. |
| V. Reproducible Builds | ✓ | No new dependencies; no `cabal.project` edits. |
| VI. Comprehensive Testing | ✓ | New unit + property tests in `TransactionLedgerSpec.hs`; no integration / E2E changes required. |
| VII. Code Quality Gates | ✓ | Fourmolu / HLint / `-Wall` apply unchanged. |

**Result: PASS. No complexity-tracking entries.**

## Project Structure

### Documentation (this feature)

```text
specs/007-ledger-minting/
├── plan.md              # this file
├── research.md          # Phase 0 findings
├── data-model.md        # mint-translation contract
├── contracts/
│   └── ledger-mint-translation.md   # invariants + property statements
├── quickstart.md        # reviewer/maintainer walkthrough
├── spec.md
└── checklists/
    └── requirements.md
```

### Source Code (repository root)

```text
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/
├── Ledger.hs            # buildLedgerTx / buildLedgerTxRaw — add mint parameter; drop the two `mempty -- TODO` sites
└── Build.hs             # unchanged: mkLedgerTx already accepts a MultiAsset (line 104)

lib/wallet/src/Cardano/Wallet.hs
└── line ~2746            # update the call to constructUnsignedTxLedger to pass mint

lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/
└── Convert.hs           # add `toLedgerMintValue :: TokenMap -> TokenMap -> MultiAsset`

lib/unit/test/unit/Cardano/Wallet/Shelley/
└── TransactionLedgerSpec.hs   # add minting test group (unit + property)

lib/primitive/test/.../Ledger/
└── ConvertSpec.hs       # add tests for toLedgerMintValue (if a spec module exists; otherwise extend an existing convert spec — confirm during T002)
```

**Structure Decision**: existing layered layout; the smallest viable change touches the Shelley transaction layer plus a new conversion in the primitive ledger-convert module. The translation lives next to `toLedgerTokenBundle` because it concerns the same `wallet ↔ ledger` boundary.

## Design Decisions (extracted from research.md)

1. **Mint parameter shape**: pass a single `MultiAsset` (already-translated ledger value) as a new positional argument to both `buildLedgerTx` and `buildLedgerTxRaw`. **Not** an extraction from `SelectionOf`, because `buildLedgerTxRaw` accepts `Either PreSelection (SelectionOf TxOut)` and `PreSelection` does not carry mint fields. Explicit parameter keeps the two surfaces symmetric and forces every caller to decide.

2. **Translation location**: add `toLedgerMintValue :: TokenMap -> TokenMap -> MultiAsset` in `Cardano.Wallet.Primitive.Ledger.Convert`, sitting beside `toLedgerTokenBundle`. Reuses `toLedgerTokenPolicyId`, `toLedgerAssetName`, and a new `toLedgerSignedQuantity` helper that maps `TokenQuantity (Natural) → Integer`. Mint quantities go in positive, burn quantities go in negated.

3. **Mint–burn overlap**: if the same `(policy, asset)` appears in both `assetsToMint` and `assetsToBurn`, net them (`mint − burn`). Drop zero entries (the ledger admits but does not require them, and dropping keeps the body deterministic across equivalent inputs). Make this explicit in `contracts/ledger-mint-translation.md`.

4. **Caller wiring**:
   - `mkTransactionLedger` (Ledger.hs:306) extracts `txAssetsToMint` and `txAssetsToBurn` from its `TransactionCtx` and passes `toLedgerMintValue mint burn` to `buildLedgerTx`.
   - `constructUnsignedTxLedger` (Ledger.hs:399) grows an explicit `(TokenMap, TokenMap)` argument; updated at its single call site in `Cardano.Wallet.hs:2746`.

5. **Out-of-scope, explicit**: no change to `mkLedgerTx` (its `MultiAsset` parameter already exists); no script witnesses; no Dijkstra-era work; no change to the `mkUnsignedTransaction` cardano-api path.

## Phase 0: Outline & Research

`research.md` consolidates the Phase 0 Explore findings: exact call sites, line numbers, signatures, the TokenMap/MultiAsset asymmetry, and risks. No unresolved `NEEDS CLARIFICATION` markers remain.

**Output**: `research.md` (present).

## Phase 1: Design & Contracts

- `data-model.md`: defines the two domain types (`TokenMap` mint, `TokenMap` burn) and the translated `MultiAsset`, plus the netting/zero-dropping rule.
- `contracts/ledger-mint-translation.md`: states the translation's invariants as property-test statements (totality, faithfulness, identity-on-empty, mint–burn netting, zero filter).
- `quickstart.md`: reviewer walkthrough — five files to read, in order, to verify the slice. Includes the exact `cabal test` command for the affected spec module.

**Output**: `data-model.md`, `contracts/ledger-mint-translation.md`, `quickstart.md` (all present).

**Agent context update**: skipped — the Spec Kit agent-context script is for CLAUDE.md-style files this repo does not maintain; this plan and its supporting documents are the durable hand-off.

## Constitution re-check post-design

Re-evaluated after writing `data-model.md` and `contracts/`. All seven principles still pass. No violations introduced by the chosen translation rule.

## Phase 2 (next)

`/speckit.tasks` will turn this plan into a bisect-safe, TDD-first task list: one RED slice for the translation, one GREEN slice for the translation + its caller plumbing, and one slice for the explicit-parameter rewiring of `constructUnsignedTxLedger`. See `quickstart.md` for the intended commit shape.
