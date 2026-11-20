# Tasks: Keep a SQLite connection open until its running query finishes

**Input**: `specs/5477-dead-worker-teardown/spec.md`, `plan.md`

## Slice `connection-close` — OWNER

- [x] T5477-S1 Add `Cardano.DB.SqliteSpec` with tests for
      `withSqliteContextFile` (a query running and a query waiting across the
      close; a query after the close; a query still running at the bound, which
      is closed once it returns; two contexts closing against one shared
      deadline, and a deadline that has not begun) and for
      `newInMemorySqliteContext` (a query running and a query waiting across the
      close; a query after the close, with the close action run twice), and list
      it in `lib/unit/cardano-wallet-unit.cabal`.
- [x] T5477-S2 Add to `Cardano.Wallet.RegistrySpec` the two "Keeps its database
      open for a running query" tests (worker `main` fails; worker
      unregistered), with a worker resource that is a file-backed
      `SqliteContext` and a waiting query whose refusal shows that closing has
      started, and generalize the resource type of `waitUntilAbsent`. Add to
      `Cardano.Wallet.DB.LayerSpec` a test that a database loaded through
      `withDatabaseLoad` closes under the shutdown deadline of its factory.
- [x] T5477-S3 RED: against the base commit's `Cardano.DB.Sqlite`, with the test
      modules reduced to what compiles against it (a stand-in
      `ErrConnectionClosed`, without the bound tests and without the assertions
      on the new trace messages), the remaining tests fail with `ErrorMisuse`
      from SQLite.
- [x] T5477-S4 In `Cardano.DB.Sqlite`, add the query lock (a `TMVar SqlBackend`
      and a closing `TVar`), use it in `withSqliteContextFile` and
      `newInMemorySqliteContext`, and close through it with a wait against a
      monotonic deadline and a background close after the deadline. Add
      `ErrConnectionClosed`, `closeWaitTimeout`, `shutdownCloseTimeout`,
      `ShutdownDeadline`, `MsgCloseWaitStarted`, `MsgCloseWaitTimedOut` and
      `MsgCloseFailed`, with Haddock.
- [x] T5477-S5 Hand one `ShutdownDeadline` from `serveWallet` to the wallet
      database factory and the stake pool database, and begin it in
      `withDrainedApiLayer` and in the release of the stake pool database.
- [x] T5477-S6 GREEN: the affected specs pass in three consecutive runs, and the
      `cardano-wallet-unit:unit` suite has no failure that the base commit does
      not also have.
- [x] T5477-S7 Build `cardano-wallet-unit:test:unit` and
      `cardano-wallet-application:lib:shelley` with `cardano-wallet +release`,
      `cardano-wallet-application +release` and `cardano-wallet-unit +release`.
- [x] T5477-S8 fourmolu, hlint and cabal-fmt clean on the touched files.
- [x] T5477-S9 Commit as `fix(wallet): close a SQLite connection only after its
      running query`.
