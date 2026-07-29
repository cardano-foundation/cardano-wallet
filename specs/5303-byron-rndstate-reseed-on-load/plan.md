# Plan: Reseed Byron RndState.gen from CSPRNG on checkpoint load

One slice. The fix is localized to the `loadPrologue` instance for
`RndState` plus its roundtrip test.

## Precise code pointers (orchestrator-verified against current master)

- `lib/wallet/src/Cardano/Wallet/DB/Store/Checkpoints/Store.hs`,
  `instance PersistAddressBook (Rnd.RndState n)`, `loadPrologue` (around
  line 823-840): currently
  `let (RndState _ ix gen (HDPassphrase pwd)) = entityVal st` ... then
  builds `Rnd.RndState { ..., gen = gen }` — the DB column value flows
  straight into the returned state unchanged.
- `loadPrologue :: W.WalletId -> SqlPersistT IO (Maybe (Prologue s))` —
  runs in `SqlPersistT IO`, which has `MonadIO`, so a `liftIO` entropy
  draw is available.
- `RndState.gen :: StdGen`
  (`lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs:177`).
- Established CSPRNG pattern (from merged #5300 / PR #5307, do not
  reinvent): `Server.hs` defines a local
  `secureSeed :: IO Int; secureSeed = do { bytes <- genSalt 8; pure
  (fromIntegral (BS.foldl' (\acc w -> acc * 256 + fromIntegral w) (0 ::
  Word64) bytes)) }`, using `genSalt` from `Cryptography.Core`
  (`crypto-primitives`, already a dependency of `lib/wallet` — see
  `lib/wallet/cardano-wallet.cabal`). `mkStdGen :: Int -> StdGen` turns
  a seed into a generator (see `System.Random`, already imported
  transitively via `Random.hs`).
- Test:
  `lib/unit/test/unit/Cardano/Wallet/DB/Store/Checkpoints/StoreSpec.hs:100-101`
  — `it "loadPrologue . insertPrologue = id  for RndState" $ property .
  prop_prologue_load_write @(RndState 'Mainnet) id`. This asserts full
  `Eq`-based round-trip (including `gen`) via `prop_loadAfterWrite`
  (`res == fa`, `StoreSpec.hs:113-146`). Once `gen` is reseeded on load,
  this specific field can no longer round-trip byte-identical. The
  `SharedState` sibling test already shows the pattern for excluding a
  field from the comparison: it passes a `preprocess :: s -> s` that
  normalizes the field on the *write* side
  (`\s -> s{ready = Pending}`). That alone isn't sufficient here because
  the *loaded* value's `gen` is what changes, not the written value's —
  the comparison needs the loaded-side `gen` normalized too. Do not
  change `RndState`'s derived `Eq` instance (wider blast radius); keep
  the fix local to this one `it` block, e.g. by writing a small
  dedicated property for RndState that normalizes `gen` on both the
  written and the loaded value before comparing (`mkStdGen 0` on both
  sides is fine — the values are never compared to each other for
  meaning, only structurally to prove the *other* fields round-trip).

## Slice A — reseed on load + adjust the roundtrip test

Owned files:
- `lib/wallet/src/Cardano/Wallet/DB/Store/Checkpoints/Store.hs`
- `lib/unit/test/unit/Cardano/Wallet/DB/Store/Checkpoints/StoreSpec.hs`

Forbidden scope: `Random.hs` (Show/Buildable already handled by merged
#5300 — do not touch), `Sqlite/Types.hs` (codec must not change), any
schema/migration file, `Server.hs` (creation path already correct).
