# Implementation Plan: Keep a SQLite connection open until its running query finishes

**Branch**: `fix/dead-worker-teardown` | **Date**: 2026-10-28 | **Spec**:
`specs/5477-dead-worker-teardown/spec.md`

## Summary

Both context constructors in `Cardano.DB.Sqlite` share a query lock made of a
`TMVar SqlBackend`, which a query takes for as long as it runs, and a `TVar
Bool`, which is set to `False` when closing starts. A query takes the backend in
one STM transaction that first reads the flag, so it throws
`ErrConnectionClosed` if closing has started, also when it was already blocked
waiting for the backend. Closing sets the flag, takes a deadline from the
monotonic clock, and polls `tryTakeTMVar` every 10 ms until it takes the backend
or the deadline passes. If it takes the backend, it closes the connection. If
not, a query is still running: it traces `MsgCloseWaitTimedOut` at Warning and
forks a thread that blocks on `takeTMVar` and closes the connection once the
query returns it. `MsgCloseWaitStarted` is traced when the wait begins. If the
background close fails, `MsgCloseFailed` is traced at Warning. A second close
finds the flag already `False` and does nothing.

The deadline is `closeWaitTimeout` (10 seconds) after the wait starts for a
connection that closes on its own. During shutdown it is one `ShutdownDeadline`,
`shutdownCloseTimeout` (40 seconds) after the first drain begins, that all the
connections closed by the drains and by the release of the stake pool database
wait for. `serveWallet` creates it and passes it to `newDBFactoryUsing` (wallet
databases, as `shutdownCloseBounds`) and to `withStakePoolDbLayer`;
`withDrainedApiLayer` and the release of the stake pool database begin it.

## Where the pieces live

| Thing | File | Lines (at `2fc5c383c`) |
|---|---|---|
| In-memory context and its close action | `lib/wallet/src/Cardano/DB/Sqlite.hs` | 211-241 |
| File-backed context, query lock, scope | `lib/wallet/src/Cardano/DB/Sqlite.hs` | 250-283 |
| Connection release | `lib/wallet/src/Cardano/DB/Sqlite.hs` | 297-303, 340-392 |
| Wallet database load and boot | `lib/wallet/src/Cardano/Wallet/DB/Layer.hs` | 548-617 |
| Stake pool database, in memory and file | `lib/wallet/src/Cardano/Pool/DB/Layer.hs` | 277-289 |
| Worker resource bracket and removal | `lib/wallet/src/Cardano/Wallet/Registry.hs` | 262-287 |
| Handler access to a worker's resource | `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs` | 6021-6029 |
| Wallet deletion unregisters the worker | `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs` | 1953 |
| Registry drain on shutdown | `lib/application/shelley/Cardano/Wallet/Application.hs` | 657-668 |
| Stake pool database layer | `lib/wallet/src/Cardano/Wallet/Pools.hs` | 345-358 |
| A query that blocks on the node | `lib/wallet/src/Cardano/Wallet.hs` | 3802-3833 |

## Technical Context

**Language/Version**: Haskell, GHC 9.12.3
**Primary Dependencies**: persistent 2.14.6.3, persistent-sqlite 2.13.3.0,
unliftio 0.2.25.1, stm
**Testing**: hspec, in `cardano-wallet-unit:unit`
**Target Platform**: Linux, macOS, Windows
**Project Type**: library change in `cardano-wallet`

## Constitution Check

- **I. Maintenance-First Stability**: a bug fix in `Cardano.DB.Sqlite`, with the
  deadline handed on through `Cardano.Wallet.DB.Layer`, `Cardano.Pool.DB.Layer`,
  `Cardano.Wallet.Pools` and `serveWallet`; the behavior of an open context is
  unchanged.
- **III. Type Safety as Security**: the backend is reachable only through the
  lock, and the lock refuses it once closing has started, so no query can reach
  a closed connection. The new exception and constant are exported with Haddock.
- **VI. Comprehensive Testing**: unit tests fail on the base commit and pass
  with the change.
- **VII. Code Quality Gates**: fourmolu, hlint, cabal-fmt, `-Wall`, and a
  `+release` (`-Werror`) build of `cardano-wallet`, `cardano-wallet-application`
  and `cardano-wallet-unit`, including the `shelley` library that holds
  `serveWallet`.

## Design decisions

- **Wait in the context, not in the registry.** Holding a worker open for the
  duration of each request (a use count in `Registry`, taken by `withWorkerCtx`)
  would make a worker's exit and `drain` wait for whole requests, and would
  leave the stake pool database unprotected. Waiting in the context covers every
  user of the two constructors.
- **Refuse waiting queries.** Setting the closing flag before the wait means
  only the query that already holds the connection is waited for. Queries
  waiting behind it fail at once instead of each extending the close.
- **Bounded wait, then close in the background.** A single query can block for
  as long as the node takes to answer (`listTransactions`), so the wait has a
  bound. At the bound the close does not free the connection under the query; a
  background thread closes it when the query returns it, so the file is released
  and a wallet deletion on Windows does not fail on a held file.
- **Polling instead of a timer.** The wait at the end of a
  `withSqliteContextFile` scope runs under `uninterruptibleMask`, where
  `System.Timeout.timeout` cannot interrupt it, and `registerDelay` needs the
  threaded runtime. A 10 ms poll of `tryTakeTMVar` against a monotonic deadline
  needs neither, and its length does not depend on how long each sleep lasts.
- **10 seconds for a single close, 40 seconds for shutdown.** A query that runs
  SQL finishes well within 10 seconds; only a query blocked outside SQLite
  reaches the bound. At shutdown the four wallet API layers are drained one
  after another and the stake pool database is closed after them, so separate
  bounds would add up to 50 seconds. A supervising process waits a bounded time
  for the wallet to stop before it kills it, and the closes plus the exit of the
  process have to fit within that time, so they share one deadline of 40
  seconds. The deadline is read when a close starts, so a close that started
  before the first drain keeps its own 10 seconds. A connection that is closed
  after its wait keeps the existing retry on `SQLITE_BUSY` in
  `closeDBHandleRetrying`.
- **Typed exception.** `ErrConnectionClosed` names the database file. A request
  that receives it fails with the 500 response that the HTTP server sends for an
  exception the handlers do not catch (#5474).

## Verification

- RED: against the base commit's `Cardano.DB.Sqlite`, with the test modules
  reduced to what compiles against it (a stand-in `ErrConnectionClosed`, without
  the bound tests and without the assertions on the new trace messages), the
  remaining tests fail with `ErrorMisuse` from SQLite.
- GREEN: the tests pass with the change in three consecutive runs of the
  affected specs, and the `cardano-wallet-unit:unit` suite has no failure that
  the base commit does not also have.
- Release build: `cabal build --constraint='cardano-wallet +release'
  --constraint='cardano-wallet-application +release'
  --constraint='cardano-wallet-unit +release' cardano-wallet-unit:test:unit
  cardano-wallet-application:lib:shelley`.
