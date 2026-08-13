# Tasks: Byron random wallets cannot sign for pre-2018 addresses

**Input**: Design documents from `/specs/011-byron-random-address-signing/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/key-resolution.md, quickstart.md
**Issue**: [#5368](https://github.com/cardano-foundation/cardano-wallet/issues/5368)

**Tests**: Required. The spec's User Scenarios section is mandatory, and the Session 2026-08-11
clarification fixed automated integration coverage for both User Story 1 and User Story 2.

**Organization**: Grouped by user story. The library change is shared, so it sits in Phase 2 and
Phase 3; User Story 2 needs no further library work and User Story 3 is proven at the unit boundary.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1, US2, US3 — maps to the user stories in spec.md
- Every command runs from the repository root inside `nix develop --quiet`

## Path Conventions

Haskell monorepo. Library under `lib/address-derivation-discovery/lib/`, unit specs under
`lib/unit/test/unit/`, integration scenarios under `lib/integration/`.

---

## Phase 1: Setup

**Purpose**: Establish the baseline and settle the one open question that changes an existing test's
expectation.

- [X] T001 Confirm the baseline is green: `nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 --test-options '--match="Cardano.Wallet.Address.Discovery.Random"'` and `just check-fmt`
- [X] T002 Resolve the `golden03` question with the paired spec case in quickstart.md step 1, added to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` and run with `--match="golden03 provenance"`; keep the passing assertion renamed to state the finding, delete the other, and record the outcome in `specs/011-byron-random-address-signing/research.md` §"Open item"

**Blocking**: If T002 shows neither the recorded nor the hardened path reproduces `golden03`, stop and
report — the candidate set in data-model.md needs revisiting before any code is written.

**Outcome (2026-08-13)**: T001 green at 19 examples; T002 resolved — `golden03` commits to the key at
its recorded soft path, so the golden stands unchanged and becomes FR-004 evidence (research.md
§"Resolved"). Phase 1 complete.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Address reconstruction — the primitive every story's verification depends on.

**⚠️ CRITICAL**: No user story work can begin until this phase is complete.

- [X] T003 Add reconstruction specs to `lib/unit/test/unit/Cardano/Byron/Codec/CborSpec.hs`: round-trip from a key's own address returns that address on both network discriminations, a different public key does not, and a non-address `ByteString` returns `Nothing` (contracts/key-resolution.md §"reconstruction")
- [X] T004 Implement the payload attribute decoder and `reconstructAddress :: XPub -> Address -> Maybe Address` in `lib/address-derivation-discovery/lib/Cardano/Byron/Codec/Cbor.hs`, reusing `encodeAddress` for the root and copying attributes verbatim; add both to the export list with Haddock
- [X] T005 Replace the duplicate local attribute decoder at `lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Encoding.hs:301` with the exported one from T004
- [X] T006 Run `nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 --test-options '--match="Cardano.Byron.Codec.Cbor"'` and confirm green

**Checkpoint**: Reconstruction is proven independently of key resolution. User story work can begin.

---

## Phase 3: User Story 1 — Spending From An Affected Address (Priority: P1) 🎯 MVP

**Goal**: A Byron random wallet can spend a UTxO whose address records index *i* while its key is at
*i* hardened, and the node accepts the transaction.

**Independent Test**: Construct such an address, fund it, spend it. Fails before, succeeds after.

### Tests for User Story 1 ⚠️

> Write these first and confirm they FAIL before implementing T009–T011.

- [X] T007 [US1] Add the affected-address fixture and its expectations to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs`: build the address with `CBOR.encodeAddress` over the public key at the hardened path and `CBOR.encodeDerivationPathAttr` recording the soft path, then assert `isOwned` returns the key at the hardened path and that the returned key reproduces the address
- [X] T008 [US1] Add a QuickCheck property to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` asserting that for every `Just` result of `isOwned`, the returned key's public key reproduces the address (FR-001, SC-005); run the focused match and capture both failures

### Implementation for User Story 1

- [X] T009 [US1] Add index hardening and `candidatePaths :: DerivationPath -> [DerivationPath]` to `lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs` — recorded, address index hardened, account index hardened, both, order-preserving dedup — and export `candidatePaths` for tests (data-model.md §"Candidate derivation")
- [X] T010 [US1] Rewrite `isOwned` in `lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs` to derive each candidate, verify with `reconstructAddress`, return the first match and `Nothing` otherwise; add the Haddock note that only `getKey` may be relied upon, since the returned key's `derivationPath` is the candidate path
- [X] T011 [US1] Confirm `golden03` still passes unmodified. T002 established that its key is at its recorded soft path, so its `accIndex`/`addrIndex` expectation must **not** change; a failure here means the candidate ladder is not returning the recorded-path key first (FR-004)
- [X] T012 [US1] Add unit coverage to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` for the account-index-hardened and both-hardened variants (FR-007, SC-007)
- [X] T013 [US1] Add a unit assertion to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` that `candidatePaths` on an already-hardened path is a singleton, which is the structural form of the one-derivation-one-reconstruction bound (FR-008, SC-006)
- [X] T014 [US1] Confirm SC-002: the existing mainnet and testnet goldens and `prop_derivedKeysAreOwned` (`RandomSpec.hs:354`) pass unmodified, with the single exception permitted by T011
- [X] T015 [P] [US1] Add an affected-address constructor to `lib/integration/framework/Test/Integration/Framework/DSL.hs` next to `randomAddresses` (`DSL.hs:3062`), building the address from the public key at the hardened path with the soft path recorded, and the protocol-magic attribute for testnet
- [X] T016 [US1] Add the spend scenario to `lib/integration/scenarios/Test/Integration/Scenario/API/Byron/Transactions.hs`: empty random wallet from a fresh mnemonic, fund one affected and one unaffected address with `moveByronCoins` (`DSL.hs:2811`), submit a payment that selects both UTxOs, expect `202` and the transaction reaching `in_ledger` (SC-001, SC-003)
- [X] T017 [US1] Run `just integration-tests-cabal-match "BYRON_TRANS"` and confirm the new scenario passes

**Checkpoint**: Affected addresses are spendable and every bootstrap witness validates. This is the
MVP — it closes the defect for ordinary payments.

**Outcome (2026-08-13)**: `BYRON_TRANS_CREATE_01b` passes against a local cluster in 87s — a
transaction spending an affected and an unaffected UTxO together was accepted and reached the
ledger. SC-001 and SC-003 verified.

---

## Phase 4: User Story 2 — Migrating An Affected Wallet (Priority: P2)

**Goal**: `POST /v2/byron-wallets/{id}/migrations` empties a wallet holding an affected address.

**Independent Test**: Run the migration endpoint against such a wallet and confirm it completes.

**Note**: No library change. Migration reaches the same `isOwned` through
`W.buildAndSignTransaction` (`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:4774`), so this
phase is the proof, not the fix.

### Tests for User Story 2 ⚠️

- [X] T018 [P] [US2] Add the migration scenario to `lib/integration/scenarios/Test/Integration/Scenario/API/Byron/Migrations.hs`: fund an affected address on a fresh random wallet with the T015 constructor, create a migration plan and assert it includes that UTxO, execute it, and assert the wallet balance reaches zero (SC-004)
- [X] T019 [US2] Run `just integration-tests-cabal-match "BYRON_MIGRATE"` and confirm the new scenario passes alongside the existing migration scenarios

**Checkpoint**: The recommended path off legacy wallets works for affected wallets.

**Outcome (2026-08-13)**: `BYRON_MIGRATE_01b` passes against a local cluster in 104s — the plan
selected both UTxOs with zero leftover, the migration executed, and the wallet reached a zero
balance. SC-004 verified.

Both scenarios were run with `integration-exe` as CI invokes it, not through the justfile:

```sh
nix shell --quiet '.#cardano-node' '.#cardano-cli' -c \
  nix shell --quiet '.#cardano-wallet' '.#local-cluster' '.#integration-exe' \
    -c integration-exe --match "BYRON_TRANS_CREATE_01b"
```

with `LOCAL_CLUSTER_CONFIGS`, `CARDANO_WALLET_TEST_DATA`, `LOCAL_CLUSTER_ERA=conway`,
`CLUSTER_LOGS_DIR_PATH` and `INTEGRATION_TEST_DIR` set to absolute paths. `INTEGRATION_TEST_DIR`
must be removed between runs.

---

## Phase 5: User Story 3 — No Silent Wrong Keys (Priority: P3)

**Goal**: An address that decrypts as ours but that no candidate reproduces yields no signing key.

**Independent Test**: Present such an address and confirm no key is produced.

**Note**: This class cannot arise before the change — `isOurs` and `isOwned` test the same condition
today — so these tests assert a property the fix creates. Per FR-009 nothing else changes: the input
carries no witness and the node rejects the transaction, as it does today.

### Tests for User Story 3 ⚠️

- [X] T020 [US3] Add a fixture to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` whose derivation-path attribute is encrypted under the wallet's `hdPassphrase` but whose root comes from an unrelated public key, and assert `isOurs` is `Just` while `isOwned` is `Nothing` (FR-003, SC-005)
- [X] T021 [US3] Assert in `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` that an address belonging to a different wallet still yields `Nothing`, unchanged from today

### Implementation for User Story 3

- [X] T022 [US3] Verify FR-009 by inspection: `git diff master...HEAD` introduces no new error constructor, no change to any signing signature, and no edit to `specifications/api/swagger.yaml`

**Checkpoint**: Verification is total — no unverified key can be returned, and no new failure surface
was introduced.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [X] T023 [P] Run `just check-fmt` and `just hlint`, and fix any finding in the files touched
- [X] T024 Run the full focused unit set: `just unit-tests-cabal-match "Cardano.Wallet.Address.Discovery.Random"` and `just unit-tests-cabal-match "Cardano.Byron.Codec"`
- [X] T025 Confirm the write set: `git diff --name-only master...HEAD` lists only the files in plan.md §"Project Structure", and none of `lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs`, `lib/wallet/src/Cardano/Wallet.hs`, `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`, `specifications/api/swagger.yaml`
- [X] T026 Commit as `fix(byron): verify derived keys reproduce the address before signing`, single concern, Conventional Commits

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: no dependencies. T002 blocks T011 only, but should be answered before any code is written
- **Foundational (Phase 2)**: depends on Setup. Blocks all user stories — nothing can be verified without reconstruction
- **User Story 1 (Phase 3)**: depends on Phase 2
- **User Story 2 (Phase 4)**: depends on Phase 2 for the library change and on T015 for the fixture constructor; independently observable
- **User Story 3 (Phase 5)**: depends on T010; independently observable at the unit boundary
- **Polish (Phase 6)**: depends on all desired stories

### Task-Level Dependencies

- T004 → T005, T010 (reconstruction must exist and be exported)
- T007, T008 → T009, T010 (RED before GREEN)
- T009 → T010 (candidate ladder before it is consumed)
- T002 → T011
- T010 → T012, T013, T014, T020, T021
- T015 → T016, T018

### File Contention

`RandomSpec.hs` is touched by T007, T008, T011, T012, T013, T020 and T021. None of those are marked
`[P]` for that reason. `DSL.hs` (T015), `Byron/Transactions.hs` (T016) and `Byron/Migrations.hs`
(T018) are distinct files.

### Parallel Opportunities

- T015 runs in parallel with any `RandomSpec.hs` task once T010 lands
- T016 and T018 are different files in different stories: parallel once T015 lands
- T023 is independent of the test runs

---

## Parallel Example: after T010

```bash
# Different files, no shared state:
Task: "T015 affected-address constructor in lib/integration/framework/Test/Integration/Framework/DSL.hs"
Task: "T012 account-level candidate coverage in lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1)

1. Phase 1 — settle `golden03` before writing code
2. Phase 2 — reconstruction, proven on its own
3. Phase 3 — RED, then the candidate ladder and verification, then integration
4. **STOP and VALIDATE**: affected addresses are spendable; unaffected wallets are unchanged

The library change is complete at T010. Everything after it is proof.

### Incremental Delivery

- Phases 1–3 close the defect for payments (SC-001, SC-002, SC-003, SC-006, SC-007)
- Phase 4 adds the migration proof (SC-004) with no further library change
- Phase 5 adds the no-silent-wrong-keys proof (SC-005)

### Commit Shape

The plan's slice contract allows the integration scenarios (T016–T019) to land as a second commit if
local cluster time is the only thing separating them. The library change must never land without
T007, T008 and T014.

---

## Notes

- Verification is unconditional: every input pays one public-key computation and one reconstruction.
  Only the fallback path pays further derivations (spec Edge Cases, "Performance")
- Discovery is out of scope (FR-005). `isOurs`, `addressToPath`, `importAddress`, `genChange` and the
  keying of `discoveredAddresses` must not change
- The pre-existing path collision in `discoveredAddresses` (research.md §"Known limitation") is not
  addressed here and must not be "fixed" in passing
