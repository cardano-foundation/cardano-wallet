# Functions model

Signature-level only. Names are the implementer's to finalise; the constraints
below are not.

## Changed

| signature | constraint |
|---|---|
| wallet output → ledger output, in `…Ledger.Convert` | must be inhabited at the Dijkstra era. Stated once over the era rather than once per era: its arguments are a wallet output; its result is the ledger output at the era demanded by the caller. |
| ledger output → wallet output, in `…Ledger.Convert` | same shape, opposite direction, same requirement. |
| wallet UTxO → ledger UTxO, in `…Ledger.Convert` | must be inhabited at the Dijkstra era, and defined by the output conversion above rather than by a parallel body. |
| Praos block → `PoolId`, in `…Ledger.Shelley` | already era-polymorphic; stated once. The Babbage-named and Conway-named copies do not both survive this change. |

## Unchanged signatures gaining a case

| function | note |
|---|---|
| `Cardano.Wallet.utxoIndexFromWalletUTxO` | its recent-era case analysis gains the Dijkstra arm; the type is untouched. |
| `Cardano.Wallet.Shelley.Transaction.mkLedgerTxOut` | same. |
| `Cardano.Wallet.Shelley.Transaction.Unsigned.toLedgerTxOut` | same. |
| `…Http.ServiceSpec.txOutFromOutput` | its era case analysis gains the Dijkstra arm; the type is untouched. |
| `Cardano.Wallet.Pools.forAllBlocks` | its block case analysis gains the Dijkstra arm; the type is untouched. |

## Forbidden

- Any new function whose body is a copy of an existing one at a different era
  index.
- Any signature that widens a case analysis into a catch-all.
