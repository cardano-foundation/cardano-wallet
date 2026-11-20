# Plan: List wallets whose worker has exited

## Summary

Make `getWallet` hand the wallet's resources to the function that builds the API wallet, in both the live and the exited case, and let the database factory skip the migration step for a database it has already migrated in this process.

## Technical Context

- Language: Haskell, packages `cardano-wallet-api` and `cardano-wallet`.
- Test: `cabal test cardano-wallet-unit:unit` (hspec), with a real file database factory in a temporary directory.

## Constitution Check

- Explicit export lists: `withMigratedDBLayerFromFile` and `withSqliteContextFileUnmigrated` are added to the existing export lists.
- Haddock on public functions: both new functions have Haddock comments.
- Fourmolu and HLint: run through the dev shell.
- Conventional Commits, one concern per commit: the `docs(5473)` commit, a `fix(api)` commit for the listing, and a `fix(wallet)` commit for the database opens.
- Tests run locally before pushing, including the `release` flag (`-Werror`).

## Where the pieces live

| Thing | File | Lines (at `2fc5c383c`) |
|---|---|---|
| `MkApiWallet` | `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs` | 978-986 |
| `mkShelleyWallet` registry lookup | same | 1148-1151 |
| `mkSharedWallet` registry lookup | same | 1515-1517 |
| `mkLegacyWallet` registry lookup | same | 1708-1711, 1748-1752 |
| `getWallet` | same | 1971-1991 |
| `listWallets` | same | 2001-2019 |
| `withLoadDBLayerFromFile` | `lib/wallet/src/Cardano/Wallet/DB/Layer.hs` | 567-579 |
| `newDBFactory` | same | 315-400 |
| `withSqliteContextFile` | `lib/wallet/src/Cardano/DB/Sqlite.hs` | 255-280 |

## Design

### Omission

`whenNotResponding` passes the original context to the builder, and each builder repeats the registry lookup to reach the wallet's database. For a wallet without a worker that lookup throws `ErrWalletNotResponding`, which `listWallets` discards. `MkApiWallet` gains a `WorkerCtx ctx` argument. `getWallet` passes the worker's resources in `whenAlive` and the resources hoisted from the loaded database in `whenNotResponding`. The builders read the reward balance and the empty-passphrase check from that argument, so none of them consults the registry.

### Database opens

Loading a database file already at the current schema opened it seven times: one for the manual migration step, three to read the schema version across the migrations from 2 to 5, and three in `withSqliteContextFile` (manual migration, automatic migration, the context).

`withSqliteContextFileUnmigrated` is now the single function that builds a `SqliteContext` for a file: it opens the file once. `withSqliteContextFile` runs its two migration steps and then calls it. `withLoadDBLayerFromFile` runs `migrateDBFile` (four opens) and then loads through it (one open), so an unrecorded load of a file at the current schema opens it five times. `withMigratedDBLayerFromFile` loads through it without the migration step.

The factory records the wallets whose file it has migrated, or created at the current schema, since the process started; the record is set after the database layer has loaded. For a recorded wallet `withDatabaseLoad` calls `withMigratedDBLayerFromFile`, which reads the schema version on the open connection and, if it is the latest, runs the action. If it is not, the record is dropped and the wallet is loaded with `withLoadDBLayerFromFile`. Any other wallet runs `withLoadDBLayerFromFile`.

A file that reports the latest schema version needs no migration: the old manual migrations run only on version 2 or lower, and each later step skips a file that is already past it. The version check on the open connection is what makes the record safe, including for a file replaced behind the factory's back. A load that fails is not recorded, and `removeDatabase` clears the record.

A cached read-only handle per exited wallet would have to be closed before `removeDatabase` deletes the file, which adds a second owner of the connection life cycle.

## Project Structure

Documentation: `specs/5473-dead-wallet-listing/{spec,plan,tasks}.md`, committed separately as `docs(5473): ...`.

Source: the three modules above, and the new spec module `Cardano.Wallet.Api.DeadWalletListingSpec` registered in `cardano-wallet-unit.cabal`.
