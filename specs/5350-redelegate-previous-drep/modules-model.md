# Modules model

- **M1 — `Cardano.Wallet.Delegation`:** owns the pure duplicate-vote decision
  used while constructing a DRep voting action. Data: D1. Function: F1.
- **M2 — `Cardano.Wallet.IO.Delegation`:** keeps the wallet-layer duplicate
  verdict aligned with M1 for its IO-facing path. Data: D1. Function: F2.
- **M3 — `Cardano.Wallet.DelegationSpec`:** owns executable examples that
  distinguish effective state from historical state. Functions: F1, F2.

Dependency direction remains wallet state → effective delegation selection →
duplicate-vote verdict; no new abstraction or persistence dependency is added.
