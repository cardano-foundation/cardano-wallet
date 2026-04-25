# Feature Specification: Remove cardano-api Dependency

**Feature Branch**: `001-drop-cardano-api`
**Created**: 2026-04-06
**Status**: Draft
**Input**: User description: "Remove cardano-api dependency from cardano-wallet"

## Context

The `cardano-balance-transaction` and `cardano-coin-selection` libraries have been extracted as standalone packages. However, the version of `cardano-balance-transaction` currently pinned in `cabal.project` (commit `98e7f41`) still depends on `cardano-api`. An active branch (`002-remove-cardano-api`) in `cardano-foundation/cardano-balance-transaction` removes that dependency with breaking API changes:

- **Removed**: `CardanoApiEra` type family, `cardanoEraFromRecentEra`, `shelleyBasedEraFromRecentEra`, `toCardanoApiTx`, `fromCardanoApiTx`
- **Changed**: transaction types are now ledger-native — no conversion layer needed
- **Added**: `TimeTranslation`, `TxWithUTxO`, `UTxOAssumptions` modules; Dijkstra era support

This means the wallet's cardano-api removal has a hard prerequisite: pin a version of `cardano-balance-transaction` that no longer depends on `cardano-api`. Without this, `cardano-api` remains in the transitive closure regardless of what the wallet does directly.

After pinning the updated `cardano-balance-transaction`, the remaining direct cardano-api usage in the wallet spans 9 packages (71 imports across 45 files), concentrated in transaction construction, certificate handling, metadata types, and era GADTs.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Upstream dependency is cardano-api-free (Priority: P1)

The wallet pins a version of `cardano-balance-transaction` that does not depend on `cardano-api`. The wallet code adapts to the breaking API changes this introduces: replacing `CardanoApiEra` type family usage with direct ledger era types, removing `toCardanoApiTx`/`fromCardanoApiTx` conversion calls, and updating era constraint handling.

**Why this priority**: This is a gate. Until the transitive dependency through `cardano-balance-transaction` is cut, no amount of direct removal achieves the goal. The API changes also cascade into the wallet's transaction construction, delegation, and voting code.

**Independent Test**: Pin the new `cardano-balance-transaction` version, build the wallet, run unit and integration tests. Verify `cardano-api` no longer appears in the build plan via `cardano-balance-transaction`.

**Acceptance Scenarios**:

1. **Given** the wallet with the updated `cardano-balance-transaction` pin, **When** the build plan is inspected, **Then** `cardano-balance-transaction` does not pull in `cardano-api`.
2. **Given** the updated pin, **When** wallet code that used `CardanoApiEra`, `toCardanoApiTx`, or `fromCardanoApiTx` is compiled, **Then** it builds successfully using direct ledger types.
3. **Given** a funded wallet, **When** a transaction is balanced, signed, and submitted, **Then** it succeeds identically to before the pin update.

---

### User Story 2 - Wallet builds without cardano-api (Priority: P1)

A developer building cardano-wallet from source no longer needs to compile the cardano-api package. The build dependency graph is shorter, version constraint conflicts involving cardano-api are eliminated. The wallet produces identical artifacts as before the removal.

**Why this priority**: This is the end goal. Every other story supports getting here.

**Independent Test**: Build the full project, run unit and integration tests. Verify `cardano-api` does not appear in the build plan output.

**Acceptance Scenarios**:

1. **Given** the wallet codebase with cardano-api removed from all .cabal files, **When** a developer builds all packages, **Then** the build succeeds with no compilation errors.
2. **Given** a successfully built wallet, **When** the full unit test suite runs, **Then** all tests pass with the same results as the pre-removal baseline.
3. **Given** a successfully built wallet, **When** the integration test suite runs against a local cluster, **Then** all tests pass across all supported eras (Babbage, Conway, Dijkstra).
4. **Given** the built project, **When** the build plan is inspected, **Then** `cardano-api` does not appear anywhere in the transitive dependency closure of any wallet package.

---

### User Story 3 - Transaction lifecycle works identically (Priority: P1)

A wallet user constructs, signs, and submits transactions through the REST API. The full transaction lifecycle — coin selection, balancing, signing, submission, confirmation — behaves identically. Transaction metadata, certificates (delegation, voting, registration), and multi-asset minting all work unchanged.

**Why this priority**: Transaction handling is the wallet's core function and the area most deeply entangled with cardano-api (SealedTx, TxBodyContent, toConsensusGenTx). Any regression is a funds-at-risk issue.

**Independent Test**: Submit delegation, voting, and payment transactions with metadata on a local cluster. Verify transaction hashes, fees, and on-chain effects match expected values.

**Acceptance Scenarios**:

1. **Given** a funded wallet, **When** a user submits a payment transaction with metadata via the REST API, **Then** the transaction is accepted by the node and the metadata is visible on-chain.
2. **Given** a funded wallet, **When** a user delegates to a stake pool, **Then** the delegation certificate is correctly formed for the current era and the delegation takes effect.
3. **Given** a funded wallet, **When** a user submits a transaction with multi-asset minting, **Then** the minted assets appear in the wallet balance.
4. **Given** a funded wallet, **When** a user submits a governance vote (Conway era), **Then** the vote certificate is correctly formed and submitted.

---

### User Story 4 - REST API responses unchanged (Priority: P1)

External consumers of the wallet REST API (Daedalus, third-party integrations) receive identical JSON responses. No fields change names, types, or serialization formats. The OpenAPI specification remains unchanged.

**Why this priority**: The REST API is the wallet's public contract. Any change to response formats breaks downstream consumers.

**Independent Test**: Run the API golden tests. Compare API responses against the pre-removal baseline for all transaction, address, and metadata endpoints.

**Acceptance Scenarios**:

1. **Given** a wallet with transaction history, **When** the transaction list endpoint is queried, **Then** the JSON response (including metadata serialization) is byte-identical to the pre-removal format.
2. **Given** the wallet server, **When** the OpenAPI spec is regenerated, **Then** it matches the existing specification with no differences.

---

### User Story 5 - Serialization round-trips preserved (Priority: P2)

Existing wallet databases created by the current version can be read by the new version. Transaction CBOR stored in the database deserializes correctly. TxMetadata JSON stored in SQLite parses identically.

**Why this priority**: Users upgrading their wallet must not lose data or encounter corruption. Database compatibility is a hard constraint.

**Independent Test**: Start the new version against a database snapshot from the current version, verify all data loads correctly.

**Acceptance Scenarios**:

1. **Given** a wallet database containing transactions from multiple eras, **When** the wallet starts with the new code, **Then** all historical transactions deserialize and display correctly.
2. **Given** stored TxMetadata JSON in the database, **When** the new code reads it, **Then** the metadata round-trips identically through the new serialization path.

---

### User Story 6 - Test generators migrated (Priority: P2)

Property-based tests that currently use `Cardano.Api.Gen` (from `cardano-api-extra`) work with replacement generators that produce identical value distributions. Test coverage does not regress.

**Why this priority**: The `cardano-api-extra` package exists solely to extend cardano-api with test generators. It cannot be eliminated until the generators are replaced with ledger-native equivalents (using `Arbitrary` instances from `cardano-ledger-conway:testlib`).

**Independent Test**: Run all property-based tests. Verify they pass with the same confidence level and cover the same value space.

**Acceptance Scenarios**:

1. **Given** the wallet test suite, **When** property-based tests run with the replacement generators, **Then** all tests pass.
2. **Given** the updated codebase, **When** the `cardano-api-extra` package directory is inspected, **Then** it no longer exists.

---

### User Story 7 - Reduced build complexity (Priority: P3)

The total number of transitive dependencies decreases. Build times improve because cardano-api (and its own transitive dependencies) no longer need compilation.

**Why this priority**: This is the motivating benefit — simpler builds, fewer version conflicts, faster CI.

**Independent Test**: Compare dependency counts and CI build times before and after.

**Acceptance Scenarios**:

1. **Given** a clean build, **When** build time is measured, **Then** it is measurably shorter than the baseline.
2. **Given** the build plan, **When** the transitive dependency count is measured, **Then** it is lower than the baseline.

---

### Edge Cases

- What happens when deserializing a transaction from a pre-Shelley (Byron) era stored in the database?
- How does the wallet handle a transaction CBOR blob serialized by the old cardano-api path — does the new deserialization path accept it?
- What happens when a node returns era-specific error types during transaction submission?
- How does cross-era transaction construction behave at era boundaries (a transaction constructed just before a hard fork)?
- What happens if an upstream `cardano-ledger-*` package does not expose an equivalent for a function currently accessed via cardano-api?
- Does the updated `cardano-balance-transaction` produce byte-identical CBOR for balanced transactions?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The wallet MUST pin a version of `cardano-balance-transaction` that does not depend on `cardano-api`, and adapt to its breaking API changes (`CardanoApiEra` removal, direct ledger types, removed conversion functions).
- **FR-002**: The wallet MUST build and pass all tests without `cardano-api` in any package's build dependencies or transitive closure.
- **FR-003**: The wallet MUST use `cardano-ledger-*` and `ouroboros-*` libraries directly for all types previously re-exported by cardano-api (SlotNo, ShelleyGenesis, NodeToClientVersion, era types).
- **FR-004**: The wallet MUST provide its own CBOR serialization/deserialization replacing cardano-api's wrappers (serialiseToCBOR, deserialiseFromCBOR, serialiseToRawBytes, deserialiseFromRawBytes, serialiseToBech32, deserialiseFromBech32).
- **FR-005**: The wallet MUST unify its internal NetworkId type (in `Cardano.Wallet.Primitive.NetworkId`) with the one previously imported from cardano-api, eliminating the dual-type bridge and conversion functions.
- **FR-006**: The wallet MUST construct delegation, registration, and voting certificates directly via `cardano-ledger` APIs, handling pre-Conway vs Conway certificate format differences currently managed by `Cardano.Api.Certificate`.
- **FR-007**: The wallet MUST define its own TxMetadata and TxMetadataValue types (or adopt ledger equivalents) that are wire-compatible with the current JSON serialization used in the REST API and database. The TxMetadataJsonSchema-based conversion (metadataFromJson, metadataToJson) must be preserved.
- **FR-008**: The wallet MUST replace SealedTx's internal use of `InAnyCardanoEra Cardano.Api.Tx` with a ledger-native transaction representation, preserving all existing serialization and submission behavior.
- **FR-009**: The wallet MUST replace `toConsensusGenTx` with a direct conversion from ledger Tx to Ouroboros GenTx for transaction submission, preserving the multi-era hard fork combinator dispatch.
- **FR-010**: The wallet MUST replace `TxBodyContent` / `createTransactionBody` with direct ledger transaction body construction, covering all current fields: inputs, outputs, fees, withdrawals, certificates, metadata, minting, validity intervals, and voting procedures.
- **FR-011**: The `cardano-api-extra` package MUST be removed from the repository after its generators are replaced with wallet-native equivalents (using ledger testlib Arbitrary instances).
- **FR-012**: The wallet's era GADT system (`cardano-wallet-read`) MUST fully replace cardano-api's `AnyCardanoEra`, `CardanoEra`, `InAnyCardanoEra`, `InAnyShelleyBasedEra`, `IsCardanoEra`, and `ShelleyBasedEra` types across all 45 files currently importing them.
- **FR-013**: All existing benchmarks MUST continue to produce comparable results (no performance regressions beyond measurement noise).
- **FR-014**: Byron-era transaction support (currently via `Cardano.Api.Byron`) MUST be preserved using direct ledger types.

### Key Entities

- **cardano-balance-transaction pin**: The `source-repository-package` entry in `cabal.project`. Currently at commit `98e7f41` which depends on cardano-api. Must be bumped to a commit from the `002-remove-cardano-api` branch (or later) that removes this dependency.
- **SealedTx**: The wallet's canonical serialized transaction type. Currently wraps `InAnyCardanoEra Cardano.Api.Tx`. Must be re-implemented around ledger-native Tx. Used in REST API, database, and submission layer.
- **TxMetadata / TxMetadataValue**: Transaction metadata types exposed in the REST API and persisted in SQLite as JSON. Must be inlined or replaced with a compatible definition.
- **NetworkId**: Network identifier. Two versions coexist (wallet's own in `Cardano.Wallet.Primitive.NetworkId` + cardano-api's). Must be unified to the wallet's own type.
- **Era GADTs**: The type-level era representation. `cardano-wallet-read` has its own system; the bridge to cardano-api's era types must be removed.
- **CardanoApiEra type family**: Currently used in wallet type signatures for certificates, key witnesses, and tx bodies. Will cease to exist after the `cardano-balance-transaction` bump — all these must switch to direct ledger era types.
- **Cardano.Api.Gen generators**: QuickCheck generators for Cardano types used in property-based tests. Must be replaced with ledger testlib generators.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `cardano-api` does not appear in the transitive dependency closure of any wallet package (including via `cardano-balance-transaction` and `cardano-coin-selection`).
- **SC-002**: All existing unit tests pass without modification to test assertions (test infrastructure changes are acceptable).
- **SC-003**: All existing integration tests pass across Babbage, Conway, and Dijkstra eras.
- **SC-004**: REST API golden tests pass with no output differences.
- **SC-005**: A wallet database from the pre-removal version loads without errors or data loss.
- **SC-006**: The `lib/cardano-api-extra` directory no longer exists in the repository.
- **SC-007**: The total import count for `Cardano.Api` modules across the codebase is zero.

## Assumptions

- The `cardano-balance-transaction` branch `002-remove-cardano-api` lands and is available to pin before or during this work. If it is delayed, phases that don't touch the balance-tx interface can still proceed, but SC-001 cannot be achieved.
- The `cardano-ledger-*` and `ouroboros-*` packages expose sufficient public API surface to replace all functionality currently accessed through cardano-api. Where they don't, upstream contributions or local shims may be needed.
- The `cardano-wallet-read` era system is mature enough to fully replace cardano-api's era GADTs without introducing new abstraction layers.
- Database schema changes (if any) will use the existing persistent migration framework.
- This removal can proceed incrementally — each phase independently mergeable — rather than requiring a single atomic change.
- The `cardano-balance-transaction` removal produces byte-identical CBOR output, so no database migration is needed for stored transaction data.
- Cross-platform compatibility (Linux, Windows, macOS) is preserved throughout the migration.
