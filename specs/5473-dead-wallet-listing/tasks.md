# Tasks: List wallets whose worker has exited

**Input**: [spec.md](spec.md), [plan.md](plan.md)
**Tests**: required by the specification (SC-001, SC-002).

Format: `[ID] [P?] [Story] Description`.

## Slice 1: listing

- [x] **T001** [US1] Add the `WorkerCtx ctx` argument to `MkApiWallet`, pass it from both branches of `getWallet`, and use it in `mkShelleyWallet`, `mkSharedWallet` and `mkLegacyWallet` instead of the registry lookup. Drop the constraints this leaves unused.
- [x] **T002** [US1] Add `Cardano.Wallet.Api.DeadWalletListingSpec` with the list and get cases for Shelley, shared and Byron wallets, and list it in `cardano-wallet-unit.cabal`.

## Slice 2: database opens

- [x] **T003** [US2] Make `withSqliteContextFileUnmigrated` the single builder of a file `SqliteContext`, called by `withSqliteContextFile` after its migration steps, and add `withMigratedDBLayerFromFile` to `Cardano.Wallet.DB.Layer`.
- [x] **T004** [US2] Record migrated wallets in `newDBFactory`, skip the migration step for them in `withDatabaseLoad` after a schema version check, record them after `withDatabaseBoot` and a completed load, and clear the record in `removeDatabase`.
- [x] **T005** [US2] Add the open-count and factory cases to `Cardano.Wallet.Api.DeadWalletListingSpec`.

## Verification

- [x] **T006** Run fourmolu, HLint and cabal-fmt (clean), the affected specs, the unit suite, and the `release` flag build.
- [x] **T007** Commit as `fix(api): list wallets whose worker has exited` and `fix(wallet): open a recorded wallet database once`. The spec folder lands as its own `docs(5473): ...` commit.
