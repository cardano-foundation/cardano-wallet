# Modules model

No new module. No new package. No new dependency.

| module | change in responsibility |
|---|---|
| `Cardano.Wallet.Primitive.Ledger.Convert` | its output and UTxO conversions become expressible at the Dijkstra era. Ownership is unchanged: it remains the single place where a wallet output becomes a ledger output for the recent eras. |
| `Cardano.Wallet.Primitive.Ledger.Shelley` | the Praos block-producer accessor is stated once instead of twice. Responsibility unchanged. |
| `Cardano.Wallet.Pools` | consumes the accessor above for the Dijkstra block constructor as it already does for Babbage and Conway. |
| `Cardano.Wallet` | `utxoIndexFromWalletUTxO` gains its Dijkstra case; it keeps delegating the conversion to `…Ledger.Convert` rather than doing it inline. |
| `Cardano.Wallet.Shelley.Transaction`, `…Transaction.Unsigned` | both gain their Dijkstra case by the same delegation. Neither acquires conversion logic of its own. |
| `…Launch.Cluster.Http.ServiceSpec` (test) | its local per-era output helpers gain the Dijkstra case. This is test-local and does not promote to `…Ledger.Convert`; the spec's helpers deliberately discard datum and script and are not the production conversion. |

## Dependency direction

Unchanged and one-way: `Cardano.Wallet`, `…Shelley.Transaction*` and
`…Wallet.Pools` depend on `…Primitive.Ledger.*`, never the reverse.

## Promotion

The test-local helpers in the local-cluster spec are **not** promoted into
`…Ledger.Convert`. They are lossy by design and promoting them would put a
second, weaker conversion behind the same module's name.
