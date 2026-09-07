# Data model — 5422

No data type is added, removed, or changed. No field, relationship, validation
rule or persisted representation moves.

The slice's whole subject is a **term-level** branch on the `RecentEra era`
GADT. The GADT itself, its two constructors, and every type reachable from
`mkLedgerTx`'s signature are untouched.

## State invariants relied upon (not introduced)

| Invariant | Where it lives | Why this slice depends on it |
|---|---|---|
| `RecentEra era` has exactly the constructors `RecentEraConway` and `RecentEraDijkstra` | `Cardano.Balance.Tx.Eras` | the extent guard in the new test quantifies over `allRecentEras` rather than over a literal list, so this invariant is *observed*, not assumed |
| `IsRecentEra era` entails `RecentEraConstraints era` | `Cardano.Balance.Tx.Eras` | this is what makes the deletion type-check at all; leg A at site 12 is the check |
| Dijkstra expunges `RegTxCert` / `UnRegTxCert`, retaining only the deposit-carrying forms | `Cardano.Ledger.Dijkstra.TxCert` | leg B evidence at the certificate-building sites, and the standing hazard for any path that builds a certificate in a lower era and **upgrades** it |

The third row is a fact about a dependency, and it is version-bound. The
resolved `cardano-ledger-dijkstra` version must be read from this worktree's own
build plan and recorded with the citation.
