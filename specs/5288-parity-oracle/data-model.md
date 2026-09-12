# data-model — #5288 / #5427

| ID | Datum | Shape | Invariant |
|---|---|---|---|
| D1 | parity scenario set | the enumerated script-witness cases the oracle covers | non-empty; if the implementation quantifies over it rather than writing one `it` per case, the quantifier carries a guard that fails on an empty or truncated set, and that guard is itself demonstrated red |
| D2 | `reviewInputScript0/1`, `reviewPreservedScript`, `reviewAddedInputScript` | `Script KeyHash` fixtures, currently `mkScript 91/92/94/93` | `reviewAddedInputScript`'s hash is **absent** from the declared witness set built from the other three, and present in the balanced set |
| D3 | comparison surface | `serialize (eraProtVerLow @Conway) (tx ^. bodyTxL)` and `tx ^. witsTxL . scriptTxWitsL` | unchanged by this ticket; both sides are compared for every scenario |

D2 is the whole of #5427: the current assertion constrains only the second half
of that invariant, which is why the `93 -> 91` collision passes.
