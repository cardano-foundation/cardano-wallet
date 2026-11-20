# Spec: List wallets whose worker has exited

**Feature Branch**: `fix/dead-wallet-listing`
**Issue**: #5473
**Status**: Draft

## User Scenarios & Testing

### User Story 1 - A wallet whose worker has exited is still listed (Priority: P1)

As a wallet client, I want `GET /v2/wallets` and `GET /v2/wallets/{walletId}` to return a wallet whose worker has exited with the state `not_responding`, so that the wallet does not disappear from the list and the client can tell why its endpoints fail.

**Why this priority**: the wallet is dropped from the response without any error, so the client cannot distinguish a dead worker from a deleted wallet.

**Independent Test**: build an API layer whose registry holds no worker for a wallet that has a database file, then list and get that wallet.

**Acceptance Scenarios**:

1. **Given** a Shelley wallet with a database file and no worker, **When** the wallet list is requested, **Then** the wallet is in the list and its `state.status` is `not_responding`.
2. **Given** the same wallet, **When** it is requested by id, **Then** the response is the wallet with `state.status` equal to `not_responding`.
3. **Given** a shared wallet or a Byron wallet with an Argon2id V2 passphrase in the same condition, **When** it is listed or requested by id, **Then** the same holds.

### User Story 2 - A request opens the database of an exited worker once (Priority: P2)

As an operator, I want a poll of the wallet list to open the database of an exited worker once, so that a client that polls every few seconds does not repeat seven file opens per wallet per poll.

**Independent Test**: count the single-connection opens traced while one list request and one get request run for a wallet with no worker.

**Acceptance Scenarios**:

1. **Given** a wallet whose database this process has already loaded, **When** its database is loaded again, **Then** the file is opened once and no migration is run.
2. **Given** a database file this process has not loaded or created, **When** it is loaded, **Then** the migration step runs first and the file is then opened once, five opens in all for a file at the current schema.
3. **Given** a recorded wallet whose file no longer reports the latest schema version, **When** it is loaded, **Then** the migration step runs and the record is dropped and set again after the load.

### Edge Cases

- A wallet whose load failed in a migration, or whose database layer failed to load, is not recorded, so the next load runs the migration step again.
- Removing a wallet's database clears its record, so a file placed at the same path afterwards is loaded with the migration step. A wallet created through the factory is recorded when it is created.

## Requirements

- **FR-001**: The function that builds an API wallet MUST receive the wallet's resources from the caller, so that it does not look the wallet up in the worker registry a second time.
- **FR-002**: `getWallet` MUST pass the resources of the live worker when there is one, and the resources of the freshly loaded database when the worker has exited.
- **FR-003**: `DBFactory.withDatabaseLoad` MUST skip the migration step for a wallet whose database file this factory has already migrated, or created, in this process, provided the open connection reports the latest schema version.
- **FR-004**: A load that skips migrations MUST open the database file once.
- **FR-005**: `removeDatabase` MUST clear the record, and a load that fails MUST NOT set it. The record is set after the database layer has loaded.
- **FR-006**: Loading a database that has not been recorded MUST run the migration step and then open the file once.
- **FR-007**: Every file-backed `SqliteContext` MUST be built by one function, with the migration step as a separate step before it.

## Success Criteria

- **SC-001**: A unit spec lists and gets a Shelley wallet, a shared wallet and a Byron wallet with an Argon2id V2 passphrase, each without a worker, and observes `not_responding`.
- **SC-002**: The same spec counts one single-connection open for one list request and for one get request, and more than one for the first load after a restart. It also checks that every connection opened through the factory is closed, that `removeDatabase` clears the record, that a failed migration is not recorded, that a recorded file whose version is not the latest runs the migration step, and that a failed version read falls back to the full load and closes its connection.

## Assumptions

- A database file that reports the latest schema version is fully migrated, because each migration step writes its version after its statements.
- A database file is not migrated by another process while the wallet server runs.
