# Data model

No new type, no new constructor, no changed field.

| type | note |
|---|---|
| ledger output at the Dijkstra era | `TxOut DijkstraEra = BabbageTxOut DijkstraEra`, defined upstream. The wallet adds no representation of its own. |
| `Cardano.Wallet.Primitive.Types.Tx.TxOut.TxOut` | unchanged. It is the wallet-side input and output of the conversions. |
| `UTxO` | unchanged. The Dijkstra ledger UTxO is a map with the same key set. |
| `PoolId` | unchanged. |

## State invariants

- An output converted to the Dijkstra era and back is the output that went in.
- A UTxO converted to the Dijkstra era has the same key set as the one that
  went in, and each value is the conversion of the corresponding value.
- Converting an output at the Dijkstra era and at the Conway era yields values
  that differ only in their era index; no field is populated differently.
- Datum and reference script are absent in every output this ticket
  constructs, exactly as on the existing Conway path. This is a preserved
  limitation, not a new one, and it is recorded here so the next reader does
  not mistake it for a Dijkstra-specific gap.
