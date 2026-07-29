# Plan — #5103 SQLite unit-test diagnostics

## Technical approach

Extend `Test.Hspec.Extra`, the unit-suite runner already used by
`cardano-wallet-unit`, with two test-only capabilities:

1. A Hspec V2 formatter derived from `specdoc` that emits line-buffered
   `SQLITE TEST START` and `SQLITE TEST FINISH` events for paths containing
   `Sqlite`.
2. A diagnostic timeout example helper. The action receives a callback for
   publishing its latest state; on timeout, the helper fails with the test
   title and the last published state.

Use the diagnostic helper in the two SQLite specs that can block:
`Cardano.DB.Sqlite.DeleteSpec` and
`Cardano.Wallet.DB.Sqlite.Migration.NewSpec`. The pure persistence tests in
`Cardano.Wallet.DB.Sqlite.TypesSpec` receive progress logging from the
formatter and need no fabricated DB state.

## Slice 1 — observable SQLite test execution

One bisect-safe commit owns:

- `lib/test-utils/src/Test/Hspec/Extra.hs`
- `lib/test-utils/test/Test/Hspec/ExtraSpec.hs`
- `lib/unit/test/unit/Cardano/DB/Sqlite/DeleteSpec.hs`
- `lib/unit/test/unit/Cardano/Wallet/DB/Sqlite/Migration/NewSpec.hs`

### RED

Add negative-control tests that demand:

- a deliberately timed-out example reports its title and injected state;
- a nested SQLite example produces both progress events.

Run the focused `cardano-wallet-test-utils` proof and observe failure before
implementing the helper and formatter.

### GREEN

Implement the formatter and timeout helper, then apply 60-second diagnostic
timeouts to the blocking Delete and Migration examples. Re-run the negative
controls and the three target SQLite modules.

### Verification

- Focused helper tests with `nix run .#unit-cardano-wallet-test-utils`.
- Target module runs with `nix run .#unit-cardano-wallet-unit` and one
  `--match` per module.
- `./gate.sh`, which also performs formatting, HLint, actionlint, and the
  repository Nix build matrix.

## Commit

Subject: `test(sqlite): add progress and timeout diagnostics`

Trailer: `Tasks: T510301, T510302, T510303, T510304, T510305`
