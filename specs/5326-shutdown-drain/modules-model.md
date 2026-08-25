# Modules model: Shutdown drain

Artifact ceiling: 90 lines / 6 KiB

| ID | Stable owner | Changed responsibility | Dependency rule |
|---|---|---|---|
| MOD-5326-REGISTRY | `Cardano.Wallet.Registry` | Own registry-wide termination and completion-barrier semantics. | May depend on concurrency primitives; must not depend on API, application, or SQLite modules. |
| MOD-5326-APPLICATION | `Cardano.Wallet.Application` | Own the acquisition/release lifetime of the four wallet API layers used by `serveWallet`. | May consume Registry drain through each API layer's existing registry capability; must not absorb Registry internals. |
| MOD-5326-REGISTRY-PROOF | `Cardano.Wallet.RegistrySpec` | Own direct multi-worker, idempotence, race, selected-unregister, and finalizer-barrier proofs. | Exercises the public Registry contract and synthetic bracketed resources. |
| MOD-5326-SIGNAL-PROOF | nearest existing application/integration process-test owner | Own the bounded two-worker shipped-SIGTERM proof and its observable release count. | Uses the shipped signal/lifecycle boundary; must not replace it with direct `killThread` or a hand-thrown exception. |

## Directional invariants

- `MOD-5326-APPLICATION` depends on `MOD-5326-REGISTRY`; the reverse edge is
  forbidden.
- SQLite remains an acquired resource behind existing factories. Registry
  shutdown does not learn SQLite policy.
- Signal translation remains owned by `Cardano.Startup`; the ticket consumes
  that boundary without redefining it.
- Unrelated services retain their present lifetime owners.
