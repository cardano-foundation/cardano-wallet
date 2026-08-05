# Spec: Reseed Byron RndState.gen from CSPRNG on checkpoint load

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5303
Parent (epic): https://github.com/cardano-foundation/cardano-wallet/issues/5304

## P1 user story

As a Byron-random wallet user, when my wallet checkpoint is loaded from
disk, the address-generation RNG is reseeded from the system CSPRNG
rather than restored from the predictable value stored on disk — with no
change to wallet recovery and no migration of existing databases.

## Background

`RndState.gen :: StdGen` is persisted to the wallet DB but functionally
redundant: address *discovery* decrypts derivation paths via the HD
passphrase, and change-address *generation* avoids collisions via the
explicit `used` set. The stored generator value is never required for
correctness. Sibling ticket #5300 (merged, PR #5307) already moved
wallet-*creation* to seed `gen` from the CSPRNG via a local `secureSeed
:: IO Int` helper (`genSalt 8` from `crypto-primitives`) in
`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`. This ticket
covers the remaining gap: checkpoint *load* still restores the
persisted seed verbatim.

## Functional requirements

- FR1: On checkpoint load (`loadPrologue` for `RndState` in
  `lib/wallet/src/Cardano/Wallet/DB/Store/Checkpoints/Store.hs`), `gen`
  is built from a fresh CSPRNG draw, not the DB column value.
- FR2: No schema/codec change — `rnd_state.gen` column and
  `stdGenToString`/`stdGenFromString` (`Sqlite/Types.hs`) stay; the
  stored value is simply ignored on read.
- FR3: `insertPrologue` (the write side) is unchanged — still persists
  whatever `gen` the in-memory state currently holds.
- FR4: Discovery and change-address generation are unaffected.
- FR5: The `StoreSpec.hs` roundtrip property for RndState no longer
  asserts `gen` survives load byte-identical, while still asserting
  every other field round-trips exactly.

## Security guarantee

The reseeded `gen` is itself re-persisted on the next checkpoint write
(FR3), so an attacker with live DB read access still sees the current
value. The guarantee is **forward-unpredictability** of future
change-address paths from a stale/known seed across restarts — not
secrecy of the persisted value.

## Success criteria

- `./gate.sh` green (build + focused unit tests + fmt + hlint).
- Two consecutive loads of the same on-disk checkpoint produce
  different `gen` values (proves reseed, not restore).
