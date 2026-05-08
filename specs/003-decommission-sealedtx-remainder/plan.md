# Plan: finish decommissioning the SealedTx cardano-api surface

Follow-up to #5271. That PR moved every call site it could reach
without rewriting the old tx builder (#5243) or the unit-test
fixtures built on top of it. This PR handles the remainder so that
`lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs`
can drop its three `Cardano.Api*` imports and the deprecated
bridges can be deleted.

## Remaining callers (exhaustive, pre-PR snapshot)

### Production code

`lib/wallet/src/Cardano/Wallet.hs`
- line 599 — re-export `sealedTxFromCardano`
- line 2576 — `sealedTxFromCardano $ inAnyCardanoEra unsignedBalancedTx`
  in `balanceTransaction` result wrapping.

`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
- line 911 — import `sealedTxFromCardano`
- line 5529 — `W.sealedTxFromCardano` in the decode-sign path.

### Old tx builder (partially overlaps with #5243)

`lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
- 170–172 — imports `cardanoTxIdeallyNoLaterThan`,
  `sealedTxFromCardano`, `sealedTxFromCardano'`
- 442 — `sealedTxFromCardano' signed`
- 677–680 — `sealedTxFromCardano` / `cardanoTxIdeallyNoLaterThan`
- 872 — `cardanoTxIdeallyNoLaterThan`

If #5243 lands first this file disappears and those sites go with
it. If #5243 lags, migrate the four sites in place.

### Integration tests

`lib/integration/scenarios/Test/Integration/Scenario/API/Shared/Transactions.hs`
- 92 — import `cardanoTxIdeallyNoLaterThan`
- 533, 559 — two `cardanoTxIdeallyNoLaterThan` sites, mirror of the
  metadata extraction already ported in `TransactionsNew.hs` in
  #5271.

`lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/TransactionsNew.hs`
- 7133 — `case getSealedTxBody tx of`
- 7141 — `sealedTxFromCardanoBody (Cardano.ShelleyTxBody …)`

This is the `addRequiredSigners` helper: injects extra key-hash
witnesses into an Alonzo tx body to test required-signer plumbing.
Needs a ledger-native helper on `Read.Tx era` that operates on
`reqSignerHashesTxBodyL` / equivalent; Byron and pre-Alonzo paths
do not apply.

### Unit tests

`lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs`
~10 sites across imports, arbitrary instance, witness-count
assertions, tx-construction helpers:
- 172, 173, 176, 177 — import `cardanoTxIdeallyNoLaterThan`,
  `getSealedTxWitnesses`, `sealedTxFromCardano`,
  `sealedTxFromCardano'`
- 416 — `arbitrary = sealedTxFromCardano <$> genTx`
- 529, 549, 621, 641, 769, 791, 837, 857 — per-test
  `sealedTxFromCardano' $ Cardano.Tx …` + witness subset checks
- 881, 897, 898 — before/after witness-count comparison for
  `addRequiredSigners`
- 927, 983, 1314, 1507 — remaining `sealedTxFromCardano'` /
  `cardanoTxIdeallyNoLaterThan maxBound` sites

`lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`
~9 sites with the same shape as `TransactionSpec.hs`
(it is the ledger-balancer variant of the same test file):
- imports at 194, 195, 198, 199
- arbitrary at 454
- per-test sites at 567, 587, 659, 679, 807, 829, 875, 895, 919,
  935, 936, 965, 1021, 1357, 1572

## Step-by-step plan

Each step is one bisect-safe commit. Gate per commit: fourmolu +
hlint + `nix build .#cardano-wallet .#unit-cardano-wallet-unit`.

### Phase A — reachable without #5243

1. `Shared/Transactions.hs` metadata extraction → ledger-native
   (mirror the `TransactionsNew.hs:getMetadataFromTx` port from
   #5271). Two call sites + import cleanup.

2. `addRequiredSigners` in `TransactionsNew.hs` → ledger-native.
   Operate on `Read.Tx era`, restricted to the Alonzo-and-later
   arms that the test actually uses; drop the
   `getSealedTxBody` + `sealedTxFromCardanoBody` round-trip.

3. `Cardano.Wallet.hs:2576` balance-result wrapping →
   `sealedTxFromLedgerTx` (already exported by SealedTx).

4. `Server.hs:5529` decode-sign path → either
   `sealedTxFromLedgerTx` or `sealedTxFromBytes`, depending on
   what the surrounding code already has in hand. Drop the import.

### Phase B — unit-test migration

5. `TransactionSpec.hs` — swap the arbitrary instance and all
   per-test call sites to `sealedTxFromLedgerTx` / a ledger-native
   witness accessor. `getSealedTxWitnesses` assertions move to the
   `addrTxWitsL` set via `sealedTxWitnessCount` or a new
   `sealedTxWitnesses` that returns the ledger witness set.

6. `TransactionLedgerSpec.hs` — same migration as step 5 applied
   to the ledger-balancer variant. Likely a straight diff from
   step 5.

### Phase C — surface removal

7. Delete `sealedTxFromCardano`, `sealedTxFromCardano'`,
   `sealedTxFromCardanoBody` from `SealedTx.hs` and its
   `Cardano.Wallet.Primitive.Types.Tx` re-export.

8. Delete `getSealedTxBody`, `getSealedTxWitnesses`,
   `cardanoApiTxToReadTx`, `cardanoTxFromBytes`,
   `cardanoTxIdeallyNoLaterThan` from `SealedTx.hs`.

9. Drop the three `Cardano.Api*` imports from `SealedTx.hs`. Tip
   compiles with zero `Cardano.Api` references in the file.

## Dependencies and ordering

- Phase A is independent of #5243.
- Phase B assumes the old tx builder is still live — it tests
  `mkTransaction` / `balanceTransaction` which live in
  `Shelley/Transaction.hs`. If #5243 lands first, large parts of
  Phase B get deleted rather than migrated, and this plan shrinks
  accordingly.
- Phase C requires A and B (or #5243) done.

## Not in this PR

- `cardano-api` in `cardano-wallet-primitive.cabal` (seven other
  primitive files still import it; separate initiative).
- `lib/cardano-api-extra/` removal (Phase D of #5237).
- `Shelley/Transaction.hs` module deletion itself (#5243).

## Test plan

- [ ] Plan committed, bisect-safe (docs-only)
- [ ] Each subsequent commit passes the per-commit gate
- [ ] Final tip: zero `Cardano.Api*` imports in
      `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs`
- [ ] Conway integration tests green
