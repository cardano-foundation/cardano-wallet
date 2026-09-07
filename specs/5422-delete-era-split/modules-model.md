# Modules model — 5422

No module is created, moved, renamed or deleted. No dependency edge changes.
The slice removes a branch inside one existing module and adds a test to one
existing spec module.

| Module | Change | Responsibility after |
|---|---|---|
| `Cardano.Wallet.Shelley.Transaction.Build` | the era match inside `mkLedgerTx` is removed; imports that the removal orphans are trimmed | unchanged: build a ledger `Tx` from its components using `mkBasicTxBody` and ledger lenses, for **every** recent era rather than for Conway only |
| `Cardano.Wallet.Shelley.TransactionLedgerSpec` (test) | gains one property plus its extent guard | unchanged: unit coverage for the ledger transaction-construction path |
| `Cardano.Wallet.Delegation` | comment only, at sites 22, 23, 24 | unchanged |
| `Cardano.Wallet.Shelley.Transaction.Unsigned` | comment only, at sites 27, 28, 29 | unchanged |

Dependency direction is untouched: `Build` continues to depend on
`Cardano.Balance.Tx.Eras` and the `cardano-ledger-*` API, and nothing new
depends on `Build`.

## Promotion

None. Nothing here belongs further upstream. In particular the era-polymorphism
this slice reveals is already owned upstream by `IsRecentEra` /
`RecentEraConstraints` in `cardano-balance-transaction`; the slice stops
shadowing it, it does not re-implement it.

## Non-module artifacts changed

| Path | Owner | Change |
|---|---|---|
| `scripts/ci/dijkstra-stub-gate.sh` | #5209's census gate | the ratchet default `MAX` moves 39 → 38. Nothing else in the script changes: not the counter, not the controls, not the exit contract. |
| `specs/5422-delete-era-split/sweep.md` | this ticket | new: the six evidenced verdicts |
