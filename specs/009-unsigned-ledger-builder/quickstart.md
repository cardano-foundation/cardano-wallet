# Quickstart: Unsigned Shelley ledger builder migration

## Baseline

```sh
git status --short --branch
./gate.sh
```

Expected bootstrap result: format passes, wallet library and unit test target build, focused Shelley transaction specs pass, and HLint reports no hints.

## Implementation Loop

1. Add/adjust Shelley transaction unit specs so the desired ledger-cert `mkUnsignedTx` path is exercised.
2. Run the focused test and capture the RED failure in `WIP.md`:

   ```sh
   nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
     --test-options '--match="Cardano.Wallet.Shelley.Transaction"'
   ```

3. Move shared unsigned ledger-builder code behind an acyclic module boundary.
4. Migrate `mkUnsignedTransaction` / `mkUnsignedTx` to the shared ledger builder.
5. Switch certificate call sites to ledger-native helpers.
6. Delete `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting`; remove them from `lib/wallet/cardano-wallet.cabal`.
7. Run:

   ```sh
   ./gate.sh
   ```

8. Confirm:

   ```sh
   rg -n "createTransactionBody|TxBodyContent" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs
   rg -n "Cardano.Wallet.Transaction.(Delegation|Voting)" lib/wallet lib/unit
   git diff --name-only origin/master...HEAD | rg '^lib/integration/'
   ```

The integration diff command must print nothing. If it prints a path, revert that edit and stop.
