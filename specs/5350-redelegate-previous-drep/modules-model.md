# Modules model

- **M1 — `Cardano.Wallet.Delegation`:** owns the pure duplicate-vote decision
  used while constructing a DRep voting action. Data: D1. Function: F1.
- **M2 — `Cardano.Wallet.IO.Delegation`:** keeps the wallet-layer duplicate
  verdict aligned with M1 for its IO-facing path. Data: D1. Function: F2.
- **M3 — `Cardano.Wallet.DelegationSpec`:** owns executable examples that
  distinguish effective state from historical state. Functions: F1, F2.
- **M4 — `specifications/Cardano/Wallet/Delegation.agda`:** owns the formal
  projected-delegation backend, effective-status definition, duplicate-vote
  decision, and named #5350 laws. Data: D1, D2. Functions: F3, F4.
- **M5 — Agda check integration:** owns a reproducible, Nix-pinned command that
  typechecks M4 and a CI edge that actually invokes that command when the
  formal backend or its mirrors change. It produces no generated Haskell.
- **M6 — QuickCheck mirror mapping:** M3 owns the focused properties; the
  #5350 specification records the one-to-one mapping from each M4 law to the
  property that checks the Haskell implementation. Data: D1, D2. Functions:
  F1, F3, F4. M6 also checks the explicit DRep-equality assumptions used by M4
  against the structural Haskell representation.

Dependency direction remains wallet state → effective delegation selection →
duplicate-vote verdict. M4 specifies that direction, M6 checks the Haskell
realization, and M5 checks M4 itself. No persistence or public API dependency is
added, and M4/M5 must not depend on generated output from M6.
