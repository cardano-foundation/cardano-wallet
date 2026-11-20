# Feature Specification: Keep a SQLite connection open until its running query finishes

**Feature Branch**: `fix/dead-worker-teardown`
**Created**: 2026-10-28
**Status**: Draft
**Input**: Issue #5477: SQLite connection is closed under a running query when a
wallet worker stops

Base: `master` at `2fc5c383c94bb907deaa0cd27b1b407f86224b6c`

## Current defect

`withSqliteContextFile` (`lib/wallet/src/Cardano/DB/Sqlite.hs:262-283`)
serializes the queries of a `SqliteContext` with a lock, but the connection is
closed by the release action of `withDBHandle` (`:297-303`), which does not take
the lock. persistent's `close'` finalizes the cached statements and calls
`sqlite3_close`, and persistent-sqlite does not check that the connection is
open before `prepare`. A query that is running in another thread when the scope
ends, or that starts after it, therefore hands SQLite a finalized statement or a
closed connection: SQLite reports `ErrorMisuse` when it detects this, and the
memory may already have been reused when it does not. The close action returned
by `newInMemorySqliteContext` (`:241`) closes the connection without taking its
lock either.

Every wallet database and the stake pool database are opened this way, and other
threads use them after their scope ends: API handlers through `withWorkerCtx`
(`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:6021-6029`) when a
worker dies, is unregistered by `deleteWallet` (`:1953`) or is drained
(`lib/wallet/src/Cardano/Wallet/Registry.hs:189-200`, `:265-287`), and the stake
pool metadata thread (`lib/wallet/src/Cardano/Wallet/Pools.hs:336-337`) when the
server shuts down. Without `--database`, the stake pool database is an in-memory
context (`lib/wallet/src/Cardano/Pool/DB/Layer.hs:277-285`).

A query can also block inside its transaction: `listTransactions`
(`lib/wallet/src/Cardano/Wallet.hs:3802-3833`) reads the time interpreter inside
`runQuery`, and that read waits until the node's first reply
(`lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs:706-707`). A
close that waits for the running query without a bound can therefore wait for as
long as the node is unreachable, and a supervising process that waits a bounded
time for the wallet to stop before it kills it would kill the wallet during the
close.

## User Scenarios & Testing

### User Story 1: a stopping worker does not break the request using its database (Priority: P1)

As a wallet operator, when a wallet worker stops, whether it died, its wallet
was deleted, or the server is shutting down, a request that is running a query
on that wallet's database completes that query, and later queries on the
database fail with an error that names the closed connection.

**Why this priority**: using a closed SQLite connection is undefined behavior.
It surfaces as `ErrorMisuse` when SQLite detects it.

**Independent Test**: unit tests on `withSqliteContextFile` and
`newInMemorySqliteContext` alone, and on a worker registry whose resource is a
file-backed `SqliteContext`.

**Acceptance Scenarios**:

1. **Given** a query paused inside `runQuery` in another thread and a second
   query blocked waiting for the connection, **When** the
   `withSqliteContextFile` scope ends, **Then** the waiting query throws
   `ErrConnectionClosed`, the scope does not return before the paused query, the
   paused query commits, and the scope then returns.
2. **Given** a context whose scope has ended, **When** `runQuery` is called on
   it, **Then** it throws `ErrConnectionClosed` with the database path and runs
   nothing.
3. **Given** a query paused inside `runQuery` that is not resumed, **When** the
   scope ends, **Then** the scope returns once the bound has passed,
   `MsgCloseWaitStarted` and `MsgCloseWaitTimedOut` are traced, a later
   `runQuery` throws `ErrConnectionClosed`, and the paused query, once resumed,
   commits and the connection is then closed.
4. **Given** two contexts, each with a paused query, and a shutdown deadline
   that begins before the first scope ends, **When** both scopes end, and the
   deadline is begun again with a larger limit between the two closes, **Then**
   the first close gives up at the deadline, the second, which starts after it,
   gives up after a single polling step, and the deadline is not moved.
5. **Given** a context with a paused query and a shutdown deadline that has not
   begun, **When** its scope ends, **Then** it gives up after the single bound,
   as without a deadline.
6. **Given** an in-memory context with a paused query and a waiting query,
   **When** its close action runs, **Then** the waiting query throws
   `ErrConnectionClosed` for `:memory:`, the close action returns only after the
   paused query, and running the close action again does nothing.
7. **Given** a registered worker whose resource is a `SqliteContext`, a request
   paused inside a query on it, and a second query waiting, **When** the
   worker's `main` throws, or the worker is unregistered, **Then** the waiting
   query is refused, the worker's release completes only after the request's
   query commits, the request's next query throws `ErrConnectionClosed`, the
   worker leaves the registry, and another worker's database still answers.
8. **Given** a wallet database loaded through `withDatabaseLoad` of a factory
   made with `newDBFactoryUsing` and `shutdownCloseBounds`, a query paused in
   it, and a shutdown deadline that begins before the scope ends, **When** the
   scope ends, **Then** the close waits until that deadline, not for the single
   bound, and traces `MsgCloseWaitTimedOut`.

### Edge Cases

- A query that is waiting for the connection when closing starts is woken by the
  change of the closing flag and throws `ErrConnectionClosed`; it does not run.
- A query retried by `retryOnBusy` takes the connection again on each attempt,
  so an attempt after closing starts throws `ErrConnectionClosed` and is not
  retried.
- The wait at the end of a `withSqliteContextFile` scope runs in the release
  action of an unliftio `bracket`, under `uninterruptibleMask`, so a second kill
  of the worker cannot shorten it. It ends at the deadline.

## Requirements

### Functional Requirements

- **FR-001**: Closing a context MUST wait until no query is running on it,
  before closing the connection, until a deadline taken from the monotonic clock
  when the wait starts. A connection that closes on its own waits for up to
  `closeWaitTimeout` (10 seconds). A query that runs SQL finishes well within
  that, and only a query blocked outside SQLite reaches it.
- **FR-002**: Once closing has started, `runQuery` MUST throw
  `ErrConnectionClosed`, carrying the database path, without calling SQLite,
  including for a query that was waiting for the connection.
- **FR-003**: If a query is still running at the deadline, the connection MUST
  NOT be closed under it. `MsgCloseWaitTimedOut` MUST be traced at Warning with
  the database path and the time waited, and a background thread MUST close the
  connection once the query has returned it. If that close fails, the failure
  MUST be traced as `MsgCloseFailed` at Warning.
- **FR-004**: The close action of `newInMemorySqliteContext` MUST behave as in
  FR-001 to FR-003, with `:memory:` as the path, and MUST do nothing when it
  runs again.
- **FR-005**: Queries on an open context MUST keep their current behavior: one
  at a time, traced as before, and, for file-backed contexts, retried on
  `SQLITE_BUSY`.
- **FR-006**: `ErrConnectionClosed`, `closeWaitTimeout` and
  `shutdownCloseTimeout` MUST be exported from `Cardano.DB.Sqlite` with Haddock
  documentation.
- **FR-007**: `MsgCloseWaitStarted` MUST be traced when a close starts waiting
  for a running query.
- **FR-008**: When the process shuts down, all the connections closed by the
  drains of the four wallet API layers and by the release of the stake pool
  database MUST wait for one shared deadline, `shutdownCloseTimeout` (40
  seconds) after the first drain begins, not for a bound each. A close that
  started before the first drain keeps its own bound. A supervising process
  waits a bounded time for the wallet to stop before it kills it, and the closes
  plus the exit of the process have to fit within that time.

### Key Entities

- **`ErrConnectionClosed`**: exception thrown by `runQuery` once the connection
  of its `SqliteContext` is closing or closed; carries the database path,
  `:memory:` for an in-memory context.
- **`MsgCloseWaitStarted`**: `DBLog` message, at Info, for a close that has to
  wait for a running query.
- **`MsgCloseWaitTimedOut`**: `DBLog` message, at Warning, for a close that gave
  up waiting for a query that was still running at the deadline.
- **`MsgCloseFailed`**: `DBLog` message, at Warning, for the failure of a close
  made by the background thread.
- **`ShutdownDeadline`**: the deadline shared by the closes that start after
  `beginShutdown`; unset until then, and not moved by a later `beginShutdown`. A
  close that started earlier keeps its own deadline.

## Success Criteria

### Measurable Outcomes

- **SC-001**: the `withSqliteContextFile` and `newInMemorySqliteContext` tests
  of acceptance scenarios 1, 2 and 6, and the two registry tests, fail with
  `ErrorMisuse` from SQLite against the base commit's `Cardano.DB.Sqlite`, with
  the test modules reduced to what compiles against it (a stand-in
  `ErrConnectionClosed`, without the bound tests and without the assertions on
  the new trace messages), and pass with the change.
- **SC-002**: the tests of acceptance scenarios 3, 4, 5 and 8 pass.
- **SC-003**: the affected specs pass in three consecutive runs, the
  `cardano-wallet-unit:unit` suite has no failure that the base commit does not
  also have, and `cardano-wallet-unit:test:unit` and
  `cardano-wallet-application:lib:shelley` build with `cardano-wallet +release`,
  `cardano-wallet-application +release` and `cardano-wallet-unit +release`
  (`-Werror`).
- **SC-004**: fourmolu, hlint and cabal-fmt report no changes for the touched
  files.

## Assumptions

- The change is in `Cardano.DB.Sqlite`. The shared deadline is created in
  `serveWallet` and handed to the wallet database factory
  (`Cardano.Wallet.DB.Layer`) and to the stake pool database
  (`Cardano.Pool.DB.Layer`, `Cardano.Wallet.Pools`). The HTTP API, the database
  schema, migrations and the worker registry are unchanged.
- The stake pool metadata thread is not stopped by this change. After the stake
  pool database is closed its next query fails with `ErrConnectionClosed`; that
  thread's lifetime is tracked in #5475.
- A request that loses its database to a stopping worker fails; it does not wait
  for a new worker.
