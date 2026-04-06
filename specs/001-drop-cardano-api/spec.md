# Feature Specification: Remove cardano-api Dependency

**Feature Branch**: `001-drop-cardano-api`
**Created**: 2026-04-06
**Status**: Draft
**Input**: User description: "Remove cardano-api dependency from cardano-wallet"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Wallet builds without cardano-api (Priority: P1)

A developer building cardano-wallet from source no longer needs to compile the cardano-api package. The build dependency graph is shorter, compile times are reduced, and version constraint conflicts involving cardano-api are eliminated. The wallet produces identical artifacts (binaries, API responses, database state) as before the removal.

**Why this priority**: This is the core goal. Every other story depends on the wallet successfully building and passing all tests without cardano-api in its dependency closure.

**Independent Test**: Build the full project, run unit tests and integration tests. All existing tests pass with no behavioral changes. Verify `cardano-api` does not appear in the build plan output.

**Acceptance Scenarios**:

1. **Given** the wallet codebase with cardano-api removed from all .cabal files, **When** a developer builds all packages, **Then** the build succeeds with no compilation errors.
2. **Given** a successfully built wallet, **When** the full unit test suite runs, **Then** all tests pass with the same results as the pre-removal baseline.
3. **Given** a successfully built wallet, **When** the integration test suite runs against a local cluster, **Then** all tests pass across all supported eras (Babbage, Conway, Dijkstra).
4. **Given** the built project, **When** the build plan is inspected, **Then** `cardano-api` does not appear anywhere in the transitive dependency closure of any wallet package.

---

### User Story 2 - Transaction lifecycle works identically (Priority: P1)

A wallet user constructs, signs, and submits transactions through the REST API. The transaction lifecycle — from coin selection through to node submission and confirmation — behaves identically to the current implementation. Transaction metadata, certificates (delegation, voting, registration), and multi-asset minting all work unchanged.

**Why this priority**: Transaction handling is the wallet's core function and the area most deeply entangled with cardano-api (SealedTx, TxBodyContent, toConsensusGenTx). Any regression here is a funds-at-risk issue.

**Independent Test**: Submit delegation, voting, and payment transactions with metadata on a local cluster. Verify transaction hashes, fees, and on-chain effects match expected values.

**Acceptance Scenarios**:

1. **Given** a funded wallet, **When** a user submits a payment transaction with metadata via the REST API, **Then** the transaction is accepted by the node and the metadata is visible on-chain.
2. **Given** a funded wallet, **When** a user delegates to a stake pool, **Then** the delegation certificate is correctly formed for the current era and the delegation takes effect.
3. **Given** a funded wallet, **When** a user submits a transaction with multi-asset minting, **Then** the minted assets appear in the wallet balance.
4. **Given** a funded wallet, **When** a user submits a governance vote (Conway era), **Then** the vote certificate is correctly formed and submitted.

---

### User Story 3 - REST API responses unchanged (Priority: P1)

External consumers of the wallet REST API (Daedalus, third-party integrations) receive identical JSON responses. No fields change names, types, or serialization formats. The OpenAPI specification remains unchanged.

**Why this priority**: The REST API is the wallet's public contract. Any change to response formats would break downstream consumers.

**Independent Test**: Run the API golden tests. Compare API responses field-by-field against the pre-removal baseline for all transaction, address, and metadata endpoints.

**Acceptance Scenarios**:

1. **Given** a wallet with transaction history, **When** the transaction list endpoint is queried, **Then** the JSON response (including metadata serialization) is byte-identical to the pre-removal format.
2. **Given** the wallet server, **When** the OpenAPI spec is regenerated, **Then** it matches the existing specification with no differences.

---

### User Story 4 - Serialization round-trips preserved (Priority: P2)

Existing wallet databases created by the current version can be read by the new version. Transaction CBOR stored in the database deserializes correctly. TxMetadata JSON stored in SQLite parses identically.

**Why this priority**: Users upgrading their wallet must not lose data or encounter corruption. Database compatibility is a hard constraint.

**Independent Test**: Take a wallet database snapshot from the current version, start the new version against it, verify all data loads and displays correctly.

**Acceptance Scenarios**:

1. **Given** a wallet database containing transactions from multiple eras, **When** the wallet starts with the new code, **Then** all historical transactions deserialize and display correctly.
2. **Given** stored TxMetadata JSON in the database, **When** the new code reads it, **Then** the metadata round-trips identically through the new serialization path.

---

### User Story 5 - Reduced build complexity (Priority: P3)

The `cardano-api-extra` package is eliminated entirely. The total number of transitive dependencies decreases. Build times improve because cardano-api (and its own dependencies) no longer need compilation.

**Why this priority**: This is the motivating benefit of the removal — simpler builds, fewer version conflicts, faster CI.

**Independent Test**: Compare dependency counts and CI build times before and after.

**Acceptance Scenarios**:

1. **Given** the updated codebase, **When** the cardano-api-extra package directory is inspected, **Then** it no longer exists.
2. **Given** a clean build, **When** build time is measured, **Then** it is measurably shorter than the baseline.

---

### Edge Cases

- What happens when deserializing a transaction from a pre-Shelley (Byron) era stored in the database?
- How does the wallet handle a transaction CBOR blob that was serialized by the old cardano-api path — does the new deserialization path accept it?
- What happens when a node returns era-specific error types during transaction submission?
- How does cross-era transaction construction behave at era boundaries (e.g., a transaction constructed just before a hard fork)?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The wallet MUST build and pass all tests without `cardano-api` in any package's build dependencies.
- **FR-002**: The wallet MUST use `cardano-ledger-*` and `ouroboros-*` libraries directly for all types previously re-exported by cardano-api (SlotNo, ShelleyGenesis, NodeToClientVersion, era types).
- **FR-003**: The wallet MUST provide its own CBOR serialization/deserialization replacing cardano-api's wrappers around ledger binary encoding.
- **FR-004**: The wallet MUST unify its internal NetworkId type with the one previously imported from cardano-api, eliminating the dual-type bridge.
- **FR-005**: The wallet MUST construct delegation, registration, and voting certificates directly via cardano-ledger, handling pre-Conway vs Conway format differences.
- **FR-006**: The wallet MUST define its own TxMetadata type (or adopt a ledger equivalent) that is wire-compatible with the current JSON serialization used in the REST API and database.
- **FR-007**: The wallet MUST replace SealedTx's internal use of cardano-api's era-indexed transaction type with a ledger-native transaction representation.
- **FR-008**: The wallet MUST replace the cardano-api-based conversion to Ouroboros GenTx with a direct conversion from ledger Tx for transaction submission.
- **FR-009**: The wallet MUST replace the TxBodyContent transaction builder with direct ledger transaction body construction.
- **FR-010**: The `cardano-api-extra` package MUST be removed from the repository.
- **FR-011**: The wallet's own era GADT system MUST fully replace cardano-api's AnyCardanoEra, CardanoEra, and InAnyCardanoEra types.
- **FR-012**: All existing benchmarks MUST continue to produce comparable results (no performance regressions beyond measurement noise).

### Key Entities

- **SealedTx**: The wallet's canonical serialized transaction type. Currently wraps cardano-api's era-indexed Tx. Must be re-implemented around ledger-native Tx.
- **TxMetadata / TxMetadataValue**: Transaction metadata types exposed in the REST API and persisted in SQLite. Must be inlined or replaced with a compatible definition.
- **NetworkId**: Network identifier. Two versions currently coexist (wallet's own + cardano-api's). Must be unified.
- **Era GADTs**: The type-level era representation system. cardano-wallet-read already has its own; the bridge to cardano-api's system must be removed.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `cardano-api` does not appear in the transitive dependency closure of any wallet package.
- **SC-002**: All existing unit tests pass without modification to test assertions (test infrastructure changes are acceptable).
- **SC-003**: All existing integration tests pass across Babbage, Conway, and Dijkstra eras.
- **SC-004**: REST API golden tests pass with no output differences.
- **SC-005**: Database migration from a pre-removal wallet version completes without data loss.
- **SC-006**: CI build time for a clean build decreases compared to the pre-removal baseline.
- **SC-007**: The `cardano-api-extra` package directory no longer exists in the repository.

## Assumptions

- The `cardano-ledger-*` and `ouroboros-*` packages expose sufficient public API surface to replace all functionality currently accessed through cardano-api. Where they don't, upstream contributions or local shims may be needed.
- The `cardano-wallet-read` era system is mature enough to fully replace cardano-api's era GADTs without introducing new abstraction layers.
- Database schema changes (if any) will use the existing persistent migration framework. No manual migration scripts are expected.
- This removal can proceed incrementally (phase by phase) with each phase independently mergeable, rather than requiring a single atomic change.
- The project remains in maintenance mode — this removal is a maintenance/infrastructure improvement, not a feature addition.
- Cross-platform compatibility (Linux, Windows, macOS) is preserved throughout the migration.
