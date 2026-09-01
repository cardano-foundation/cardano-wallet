# cardano-wallet #5397 — Node 11.1.0 L0

## Outcome

Build the six L0 packages against the already-pinned Cardano Node 11.1.0
dependency set: `cardano-numeric`, `cardano-wallet-read`, `faucet`,
`flaky-tests`, `std-gen-seed`, and `text-class`.

## Requirements

- **R1:** Build L0 in the narrowed compiler environment authorised by epic
  ruling NOTE-002. The full dev shell's `tracer-transformers` failure is a
  later-level tracing-stack migration concern, not an L0 resolver repair; do
  not patch a third-party package or alter `cabal.project` to exclude packages.
- **R2:** Process the six packages in the declared L0 order. Each adapted
  package is committed once, signed, and is never changed again in this bump.
- **R3:** Build each package and its enabled tests with `-O0`; run fourmolu and
  hlint over every touched Haskell file. Record test failures rather than
  extending scope to repair them.
- **R4 (BLOCKING):** The Dijkstra stub census remains exactly 44. Existing
  `Dijkstra not yet supported` behavior is not implementation scope.
- **R5:** Do not modify L1–L7 packages, add Cabal bounds, or move the
  `cardano-node-runtime` flake input.

## Rejections

- No Dijkstra implementation, test-coverage expansion, refactor programme, or
  third-party source patch is accepted.
- A required edit outside L0, a solver error, or a behavior-changing design
  decision is a blocking question to the ticket owner.
