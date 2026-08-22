# Specification: cardano-addresses 4.0.8

## Outcome

The Cabal and Nix dependency inputs resolve `cardano-addresses` 4.0.8 from the
same minimal CHaP snapshot, without unrelated source changes or unsafe solver
overrides.

## Requirements

- **R1:** Both dependency paths use one CHaP revision/index-state pair that
  contains `cardano-addresses` 4.0.8.
- **R2:** The Cabal plan records `cardano-addresses` 4.0.8.
- **R3:** Any additional Cardano package movement is limited to the smallest
  compatibility closure forced by 4.0.8; unrelated major ecosystem churn is
  rejected.
- **R4:** Directly affected wallet packages build and their focused unit tests
  pass with optimisation disabled.
- **R5:** No `allow-newer`, dependency-bound weakening, or unrelated cleanup is
  introduced.

## Invariants

- **I1 Pin alignment:** the CHaP flake revision and Cabal CHaP index-state name
  the same repository history horizon. A mismatched pair fails the gate.
- **I2 Exact resolution:** plan evidence reports version 4.0.8, not merely that
  the package exists in an index.
- **I3 Minimal closure:** the final diff and plan explain every changed direct
  Cardano pin; unexplained package movement fails acceptance.
- **I4 Buildability:** the affected build and focused tests run against the
  resolved plan and exit successfully.

## Rejection behaviour

Stop with evidence if the released compatibility closure requires a
non-trivial ecosystem upgrade or Haskell call-site redesign. Do not hide the
conflict with solver overrides.
