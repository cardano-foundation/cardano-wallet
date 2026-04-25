# Tasks: Remove cardano-api Dependency

**Input**: Design documents from `/specs/001-drop-cardano-api/`
**Issue**: #5237

## Commit discipline

Every task produces one or more commits. Every commit compiles and passes tests. The additive-then-remove pattern ensures no broken intermediate states.

---

## Phase A: Pin Bump

- [ ] T001 Update `cardano-balance-transaction` pin in `cabal.project` from `98e7f41` to `964e8a2`, update sha256, fix compilation
- [ ] T002 Fix `CardanoApiEra` type family removal — replace with direct ledger era types in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
- [ ] T003 Remove `toCardanoApiTx`/`fromCardanoApiTx` calls in `lib/primitive/` and `lib/wallet/`
- [ ] T004 Update era constraint signatures in `lib/api/src/Cardano/Wallet/Api/Http/Server.hs` and related handlers
- [ ] T005 Run unit + integration tests, fix any remaining failures from API changes
- [ ] T006 Compare CBOR output of balanced transactions before and after pin bump — verify byte-identical

**Checkpoint**: `cardano-balance-transaction` no longer pulls in `cardano-api`. Wallet compiles, all tests pass.

---

## Phase B: Add New Paths Alongside Old (additive only)

Each task adds new code. Nothing is removed or changed. Dual paths coexist.

- [ ] T007 [P] Create wallet-owned `TxMetadata`/`TxMetadataValue` in new module `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/TxMetadata.hs` with JSON schema conversion ported from cardano-api (FR-007)
- [ ] T008 [P] Add ledger-native constructor path to `SealedTx` (`EraValue LedgerTx` + CBOR) alongside existing `InAnyCardanoEra Cardano.Api.Tx` in `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs` (FR-008)
- [ ] T009 [P] Add `networkIdToLedger :: NetworkId -> Ledger.Network` and `sNetworkIdToLedger :: SNetworkId n -> Ledger.Network` in `lib/primitive/lib/Cardano/Wallet/Primitive/NetworkId.hs` alongside existing `networkIdVal` (FR-005)
- [ ] T010 [P] Add direct `cardano-ledger-api` certificate helper functions alongside `Cardano.Api.Certificate` usage in `lib/wallet/` and `lib/api/` (FR-006)
- [ ] T011 [P] Add `mkShelleyTx` + `HardForkGenTx` submission helpers alongside `toConsensusGenTx` in `lib/network-layer/` (FR-009)
- [ ] T012 [P] Add ledger-native tx body construction helpers alongside `TxBodyContent`/`createTransactionBody` in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` (FR-010)
- [ ] T013 [P] Add ledger testlib `Arbitrary` instances and wallet-native generators for `SealedTx`/`TxMetadata` alongside `cardano-api-extra` generators (FR-011)
- [ ] T014 [P] Add Byron-era ledger-native tx helpers alongside `Cardano.Api.Byron` usage (FR-014)

**Checkpoint**: All new paths exist and compile. Old paths still used by consumers. No behavior change.

---

## Phase C: Migrate Consumers (one module at a time)

Each task switches imports in one module or small group from cardano-api to the new paths. Each commit compiles and passes tests.

### C1: Re-export swaps (mechanical — FR-003)

Replace `Cardano.Api` imports of re-exported ledger types (SlotNo, EpochNo, Lovelace, Hash, TxId, etc.) with direct `cardano-ledger-*` / `ouroboros-*` imports.

- [ ] T015 [P] Swap re-exports in `lib/primitive/` (13 files)
- [ ] T016 [P] Swap re-exports in `lib/api/` (8 files)
- [ ] T017 [P] Swap re-exports in `lib/wallet/` (7 files)
- [ ] T018 [P] Swap re-exports in `lib/network-layer/` (3 files)
- [ ] T019 [P] Swap re-exports in `lib/unit/`, `lib/integration/`, `lib/local-cluster/`, `lib/benchmarks/` (8 files)

### C2: Era type swaps (FR-012)

Replace `AnyCardanoEra`, `CardanoEra`, `InAnyCardanoEra`, `ShelleyBasedEra`, `IsCardanoEra` with `cardano-wallet-read` `Era`, `EraValue`, `IsEra`.

- [ ] T020 Migrate era types in `lib/primitive/` modules
- [ ] T021 Migrate era types in `lib/wallet/` modules
- [ ] T022 Migrate era types in `lib/api/` modules
- [ ] T023 Migrate era types in `lib/network-layer/` modules
- [ ] T024 Migrate era types in test/benchmark packages

### C3: Core type swaps

- [ ] T025 Switch `SealedTx` consumers to ledger-native constructor — `lib/primitive/` internal modules (FR-008)
- [ ] T026 Switch `SealedTx` consumers in `lib/wallet/` (FR-008)
- [ ] T027 Switch `SealedTx` consumers in `lib/api/` and `lib/network-layer/` (FR-008)
- [ ] T028 Switch `TxMetadata` consumers from `Cardano.Api.TxMetadata` to wallet-owned type — `lib/primitive/`, `lib/api/` (FR-007)
- [ ] T029 Switch `NetworkId` consumers that do not need testnet magic from `networkIdVal` to `networkIdToLedger` (FR-005)
- [ ] T030 Switch CBOR serialization from `serialiseToCBOR`/`deserialiseFromCBOR` to `cardano-ledger-binary` across all call sites (FR-004)
- [ ] T031 Switch bech32 serialization to direct `bech32` library (FR-004)

### C4: Certificate + tx construction + submission swaps

- [ ] T032 Switch certificate construction to `cardano-ledger-api` in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` (FR-006)
- [ ] T033 Switch certificate construction in API-layer delegation/voting handlers in `lib/api/` (FR-006)
- [ ] T034 Switch tx body construction from `TxBodyContent`/`createTransactionBody` to ledger-native in `lib/wallet/` (FR-010)
- [ ] T035 Switch submission from `toConsensusGenTx` to `mkShelleyTx` + `HardForkGenTx` in `lib/network-layer/` (FR-009)
- [ ] T036 Switch Byron-era tx handling from `Cardano.Api.Byron` to ledger-native in all packages (FR-014)

### C5: Test generator swaps

- [ ] T037 Switch generators in `lib/unit/` from `Cardano.Api.Gen` to ledger testlib / wallet-native (FR-011)
- [ ] T038 Switch generators in `lib/integration/` (FR-011)

**Checkpoint**: No code uses cardano-api. `cardano-api` still in `build-depends` but all imports gone.

---

## Phase D: Remove Old Paths (atomic commit)

Single commit. Everything removed at once.

- [ ] T039 Delete old `SealedTx` constructor (`InAnyCardanoEra` path), `networkIdVal` bridge, era bridge functions, `toConsensusGenTx` wrapper, old tx body construction, old certificate helpers
- [ ] T040 Remove all remaining `Cardano.Api` imports (should be zero, but verify)
- [ ] T041 Remove `cardano-api` from `build-depends` in all `.cabal` files
- [ ] T042 Remove `cardano-api` constraints from `cabal.project`
- [ ] T043 Delete `lib/cardano-api-extra/` directory

**Checkpoint**: `cardano-api` gone from the build. Project compiles. All tests pass.

---

## Phase E: Verification

- [ ] T044 Verify `cardano-api` absent from `cabal build all --dry-run` transitive closure (SC-001)
- [ ] T045 Verify zero `Cardano.Api` imports across codebase (SC-007)
- [ ] T046 Run API golden tests — no response format changes (SC-004)
- [ ] T047 Load database snapshot from pre-removal version, verify all data loads (SC-005)
- [ ] T048 Verify Byron-era transactions in database deserialize correctly
- [ ] T049 Verify TxMetadata JSON round-trips identically through new path
- [ ] T050 Run benchmarks, compare with pre-removal baseline (FR-013)
- [ ] T051 Run `just ci` — full lint + build + test pass

---

## Dependencies

```
Phase A (pin bump)
  ↓
Phase B (add new paths) — all tasks parallel
  ↓
Phase C (migrate consumers) — C1 parallel per-package, then C2→C3→C4→C5 sequential
  ↓
Phase D (atomic removal) — single commit
  ↓
Phase E (verification) — all tasks parallel
```

Within Phase C, the ordering matters:
- C1 (re-exports) before C2 (era types) — fewer import conflicts
- C2 (era types) before C3 (core types) — SealedTx rewrite needs new era types
- C3 (core types) before C4 (tx construction) — tx building uses SealedTx and TxMetadata
- C4 before C5 (generators) — generators produce the new types
