# Modules model

| module | responsibility | change |
|---|---|---|
| `Cardano/Wallet/Primitive/Ledger/Convert` | ledger to wallet conversions | owns the single native script conversion; its era constraint is widened |
| `.../Read/Tx/Features/Mint` | decode the mint/burn script map of an observed transaction | Dijkstra arm calls the conversion instead of failing |
| `.../Read/Tx/Features/Scripts` | decode explicit scripts for witness counting | same |
| `Cardano/Wallet/Primitive/Types/TokenMapWithScripts` | `AnyScript` | changed only if the open decision requires a case for unrepresentable native scripts |
| `Cardano/Wallet/Primitive/Types/AnyExplicitScripts` | `AnyExplicitScript` | same |

Dependency direction is unchanged: the two readers depend on `Convert`, never
the reverse, and no module gains a dependency on `cardano-ledger-dijkstra` that
does not already have one.

No abstraction is promoted. The conversion already lives in the module both
readers import; the change removes a constraint rather than introducing a layer.
