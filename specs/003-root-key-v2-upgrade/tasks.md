# Tasks: Root Key Protection Upgrade to V2

**Input**: Design documents from `specs/003-root-key-v2-upgrade/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Note**: Core implementation is complete in commit `7aa70dffe6`. All remaining tasks
close the two constitution gaps identified in `plan.md`: missing OpenAPI spec update
(Principle IV) and missing unit test coverage (Principle VI).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel with other [P] tasks in the same phase
- **[US#]**: Which user story this task belongs to

---

## Phase 1: Setup (Shared Infrastructure)

**Status**: Complete. Core implementation, `PassphraseScheme` extension, `HashedCredentials`
sum type, `PersistPrivateKey` V1/V2 serialisation, `withRootKey` migration logic,
`ApiWalletPassphraseInfo` extension, and `Arbitrary` instance are all landed in
commit `7aa70dffe6`. No setup tasks remain.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Confirm the existing implementation compiles cleanly before adding tests.

- [ ] T001 Verify `cabal build lib:cardano-wallet lib:cardano-wallet-api` passes with zero warnings on the `003-root-key-v2-upgrade` branch

**Checkpoint**: Clean build confirmed — test and spec work can proceed in parallel.

---

## Phase 3: User Story 1 — Automatic Key Upgrade on Unlock (Priority: P1) 🎯 MVP

**Goal**: The migration path is correct, atomic, and tested. A V1 wallet that is unlocked
comes out as V2 with the scheme metadata updated; a failed unlock leaves V1 intact.

**Independent Test**: `cabal test lib:unit --test-options '--match "/PersistPrivateKey"'`
and `--match "/withRootKey"` both green.

### Implementation for User Story 1

- [ ] T002 [P] [US1] Create `lib/unit/test/unit/Cardano/Wallet/Address/Keys/PersistPrivateKeySpec.hs` with property tests:
  - V1 Shelley roundtrip: `serializeXPrv` → `unsafeDeserializeXPrv` yields identical `HashedCredentialsV1`
  - V1 Byron roundtrip: same, with non-empty payload passphrase
  - V2 Shelley roundtrip: yields identical `HashedCredentialsV2`
  - V2 Byron roundtrip: same, with non-empty payload passphrase
  - Distinguisher: first segment of a V2-serialised key is longer than 256 hex chars

- [ ] T003 [P] [US1] Add `withRootKey` migration tests to `lib/unit/test/unit/Cardano/Wallet/` (new describe block, e.g. in `WalletSpec.hs` or a dedicated `WithRootKeySpec.hs`):
  - Given V1 creds + correct passphrase → stored key becomes `HashedCredentialsV2`, metadata scheme becomes `EncryptWithArgon2idV2`
  - Given V1 creds + wrong passphrase → `ErrWithRootKeyWrongPassphrase` returned, stored key unchanged
  - Given V2 creds + correct passphrase → stored key unchanged (no redundant re-encryption)
  - Given V2 creds + wrong passphrase → `ErrWithRootKeyWrongPassphrase` returned

- [ ] T004 [US1] Register the new spec(s) in the unit test `Main.hs` or cabal `other-modules` so they are picked up by `cabal test lib:unit`

**Checkpoint**: User Story 1 fully tested — migration correctness and atomicity verified.

---

## Phase 4: User Story 2 — Frontend Detection via `encryption_method` (Priority: P2)

**Goal**: The OpenAPI contract is updated and the JSON serialisation is golden-tested for
all three scheme values. Frontends can rely on the documented field.

**Independent Test**: `cabal test lib:unit --test-options '--match "/ApiWalletPassphraseInfo"'`
green; `swagger.yaml` validates cleanly.

### Implementation for User Story 2

- [ ] T005 [P] [US2] Update `specifications/api/swagger.yaml` — add `encryption_method` property to the `walletPassphraseInfo` anchor (lines ~1213–1219). Apply the diff in `specs/003-root-key-v2-upgrade/contracts/wallet-passphrase-info.yaml`. Field is optional (not in `required` list) for forward-compatibility.

- [ ] T006 [P] [US2] Add golden JSON assertions to `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs`:
  - `ApiWalletPassphraseInfo` with `EncryptWithScrypt` round-trips as `{"last_updated_at": …, "encryption_method": "scrypt"}`
  - same for `EncryptWithPBKDF2` → `"pbkdf2-hmac-sha512"`
  - same for `EncryptWithArgon2idV2` → `"argon2id-v2"`
  - `FromJSON` rejects unknown `encryption_method` strings

**Checkpoint**: OpenAPI contract matches implementation; JSON serialisation verified for all three scheme values.

---

## Phase 5: User Story 3 — Explicit Upgrade via Passphrase Change (Priority: P3)

**Goal**: Confirm that going through the standard passphrase-change flow on a V1 wallet
results in a V2 wallet, verifiable via the API field.

**Independent Test**: `cabal test lib:unit --test-options '--match "/updateWalletPassphrase"'`
(or equivalent) green.

### Implementation for User Story 3

- [ ] T007 [US3] Add a unit test for `updateWalletPassphraseWithOldPassphrase` in the wallet unit suite:
  - Given a V1 wallet, when passphrase is changed (old → new, or old → same), then stored key is `HashedCredentialsV2` and scheme is `EncryptWithArgon2idV2`

**Checkpoint**: All three user stories independently verified.

---

## Phase 6: Polish & Cross-Cutting Concerns

- [ ] T008 [P] Add tracer events to `migrateV1toV2` in `lib/wallet/src/Cardano/Wallet.hs`:
  - Emit a trace message on successful migration (scheme + wallet id, no key material)
  - Emit a trace message on silent failure (to make silent errors observable in logs)

- [ ] T009 [P] Verify Fourmolu formatting on all modified/new files:
  - `lib/wallet/src/Cardano/Wallet.hs`
  - `lib/wallet/src/Cardano/Wallet/Address/Keys/PersistPrivateKey.hs`
  - `lib/wallet/src/Cardano/Wallet/Primitive/Types/Credentials.hs`
  - `lib/api/src/Cardano/Wallet/Api/Types.hs`
  - `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
  - all new test files

- [ ] T010 Run full unit test suite: `cabal test lib:unit` — must be 100% green

- [ ] T011 Run `cabal build` with `-Werror` to confirm no warnings remain

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: Complete
- **Phase 2 (Foundational)**: T001 — run once before parallel test work
- **Phase 3 (US1)**: T002, T003 can run in parallel after T001; T004 depends on T002+T003
- **Phase 4 (US2)**: T005, T006 can run in parallel after T001, independent of Phase 3
- **Phase 5 (US3)**: T007 can start after T001, independent of Phase 3 and Phase 4
- **Phase 6 (Polish)**: T008–T011 after all story phases complete

### User Story Dependencies

- **US1 (P1)**: Depends only on T001 (clean build)
- **US2 (P2)**: Depends only on T001 — fully independent of US1
- **US3 (P3)**: Depends only on T001 — fully independent of US1 and US2

### Parallel Opportunities

```
After T001 (clean build confirmed):
  ├─ T002 [US1] PersistPrivateKeySpec.hs
  ├─ T003 [US1] withRootKey tests
  ├─ T005 [US2] swagger.yaml update
  └─ T006 [US2] ApiWalletPassphraseInfo golden tests
  └─ T007 [US3] passphrase change migration test

After T002 + T003:
  └─ T004 [US1] register specs in cabal/Main.hs

After all story phases:
  ├─ T008 tracer events
  ├─ T009 Fourmolu check
  ├─ T010 full test suite
  └─ T011 -Werror build
```

---

## Implementation Strategy

### MVP (US1 only — closes constitution gap for testing of migration path)

1. T001 — verify clean build
2. T002, T003 in parallel — write PersistPrivateKey + withRootKey tests
3. T004 — register new specs
4. **STOP**: confirm `cabal test lib:unit` green for new tests
5. US1 is independently verified

### Full Delivery

1. MVP above
2. T005, T006 in parallel — close OpenAPI + JSON test gap (US2)
3. T007 — passphrase change test (US3)
4. T008–T011 — polish, formatting, full green suite

---

## Notes

- `[P]` tasks operate on different files and have no intra-phase dependencies
- All test file additions must also be registered in the relevant cabal `other-modules` stanza
- The swagger.yaml change (T005) is the only change to `specifications/`; verify it does not break the swagger validation tooling
