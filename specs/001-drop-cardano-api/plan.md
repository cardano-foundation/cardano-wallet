# Implementation Plan: Remove cardano-api Dependency

**Branch**: `001-drop-cardano-api` | **Date**: 2026-04-09 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/001-drop-cardano-api/spec.md`
**Issue**: #5237

## Summary

Remove all direct and transitive dependencies on `cardano-api` from `cardano-wallet`. Single PR, bisect-safe commit sequence using the additive-then-remove pattern: add new ledger-native paths alongside old cardano-api paths, migrate consumers one at a time, then atomically remove all cardano-api remnants.

## Technical Context

**Language/Version**: Haskell (GHC 9.6.x via Nix flake)
**Primary Dependencies**: cardano-ledger-*, ouroboros-consensus-cardano, cardano-wallet-read, bech32, cardano-ledger-binary
**Storage**: SQLite via persistent (CBOR blobs + JSON text for metadata)
**Testing**: HUnit + QuickCheck + tasty; integration tests against local cardano-node cluster
**Target Platform**: Linux (musl static), macOS (x86_64 + arm64), Windows (cross-compiled)
**Project Type**: Haskell monorepo (32 cabal packages in `lib/`)
**Constraints**: Wire-compatible serialization (CBOR, JSON) — no database migration. Every commit must compile and pass tests.
**Scale/Scope**: 44 files across 9 packages importing Cardano.Api modules

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | PASS | Fewer dependencies, incremental commits, each green |
| II. Era-Aware Design | PASS | cardano-wallet-read era GADTs already cover Byron–Dijkstra |
| III. Type Safety as Security | PASS | Direct ledger types are more precise than wrappers |
| IV. Formal Specification | PASS | OpenAPI spec unchanged |
| V. Reproducible Builds | PASS | Standard Nix/cabal.project workflow |
| VI. Comprehensive Testing | PASS | Each commit passes full test suite |
| VII. Code Quality Gates | PASS | Each commit passes CI |

## Commit Strategy

**Principle**: Every commit compiles and passes tests. No broken intermediate states.

**Pattern**: Additive-then-remove.

### Phase A: Pin bump

Update `cardano-balance-transaction` pin to `964e8a2`. Fix compilation against the new API. Both old and new cardano-api paths coexist — the wallet still depends on cardano-api directly, but `cardano-balance-transaction` no longer does.

Each fix is a separate commit. Verify CBOR output is byte-identical.

### Phase B: Add new paths alongside old

Additive-only commits. Nothing is removed. The codebase temporarily has dual paths.

- Wallet-owned `TxMetadata`/`TxMetadataValue` module (new file, not imported yet)
- Ledger-native `SealedTx` constructor (new constructor alongside old one)
- `networkIdToLedger` / `sNetworkIdToLedger` functions (alongside existing `networkIdVal`)
- Direct `cardano-ledger-api` certificate constructors (new helper functions)
- `mkShelleyTx` + `HardForkGenTx` submission helpers (alongside `toConsensusGenTx`)
- Ledger-native tx body construction helpers (alongside `TxBodyContent`)
- Ledger testlib generators (alongside `cardano-api-extra` generators)

Each addition is one or more commits. All compile because nothing depends on the new paths yet.

### Phase C: Migrate consumers

Switch call sites from old cardano-api paths to new ledger-native paths, one module at a time. Each commit:
- Touches one module (or a small group of tightly-coupled modules)
- Replaces cardano-api imports with new paths
- Compiles and passes tests

Order: start from leaf modules, work toward core. Suggested grouping:
1. Re-export swaps (SlotNo, EpochNo, etc.) — mechanical, per-package
2. Era type swaps (AnyCardanoEra → EraValue) — per-module
3. Core type swaps (SealedTx, TxMetadata, NetworkId) — per-consumer
4. Certificate and tx construction swaps — per-module
5. Submission path swap
6. Test generator swaps

At the end of Phase C, no code uses cardano-api, but it's still in `build-depends`.

### Phase D: Remove old paths (atomic)

Single commit:
- Delete bridge functions, old constructors, compatibility shims
- Remove `cardano-api` from all `.cabal` `build-depends`
- Remove `cardano-api` constraints from `cabal.project`
- Delete `lib/cardano-api-extra/`

This is the only commit that actually removes cardano-api. It compiles because Phase C already migrated all consumers.

### Verification

Run after Phase D:
- `cardano-api` absent from `cabal build all --dry-run`
- Zero `Cardano.Api` imports
- Unit + integration tests green across Babbage, Conway, Dijkstra
- API golden tests pass
- Database snapshot from pre-removal loads without errors
- Benchmarks show no regression

## Project Structure

### Documentation

```text
specs/001-drop-cardano-api/
├── plan.md              # This file
├── research.md          # Design decisions
├── data-model.md        # Entity changes
├── quickstart.md        # Dev environment
├── checklists/
│   └── requirements.md
├── spec.md              # Feature specification
└── tasks.md             # Task checklist
```

### Affected source code

```text
lib/
├── primitive/           # SealedTx, TxMetadata, NetworkId (13 files)
├── api/                 # REST handlers, metadata schema (8 files)
├── wallet/              # Transaction construction, certificates (7 files)
├── network-layer/       # Node submission, consensus bridge (3 files)
├── cardano-wallet-read/ # Era GADTs (bridge removal only)
├── unit/                # Unit tests (4 files)
├── integration/         # Integration tests (2 files)
├── cardano-api-extra/   # DELETED in Phase D
├── local-cluster/       # Test cluster setup (1 file)
└── benchmarks/          # Benchmark harness (1 file)
```
