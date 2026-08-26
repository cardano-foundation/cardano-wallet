# cardano-wallet #5397 — Node 11.1.0 L1

## Outcome

Build the six L1 packages against the already-pinned Cardano Node 11.1.0
dependency set: `cardano-wallet-application-extras`,
`cardano-wallet-test-utils`, `crypto-primitives`, `iohk-monitoring-extra`,
`temporary-extra`, and `wai-middleware-logging`.

## Requirements

- **R1:** Process exactly the six L1 packages in the declared order. A package
  with no required source adaptation is a complete no-change result, recorded
  with build/test evidence; an adapted package receives one signed local
  commit and is not subsequently modified in this bump.
- **R2:** `iohk-monitoring-extra` mechanically adapts all ten compiler-reported
  tracing API sites under `contra-tracer-0.2.1.1`: two function-style
  `runTracer` uses become `traceWith`, and eight `Tracer $ \\x -> ...`
  constructor uses become `Tracer $ TA.emit $ \\x -> ...` with
  `Control.Tracer.Arrow` qualified as `TA`. Exports, types, emitted values,
  and control flow remain unchanged.
- **R3:** Build each package and its enabled tests with `-O0`; run fourmolu
  and hlint over touched Haskell files. Test failures are recorded, not fixed.
- **R4 (BLOCKING):** The Dijkstra unsupported-stub census remains exactly
  `44`; no Dijkstra behavior or coverage is added.
- **R5:** Do not modify L0 packages, L2+ packages, `cabal.project`,
  `flake.nix`, Cabal bounds, or the dependency pins.

## Rejections

- No Dijkstra implementation, test-coverage expansion, refactor programme,
  or third-party source patch is accepted.
- A necessary edit outside L1, a census other than `44`, or a design decision
  beyond the mechanical `runTracer` API adaptation is blocking.
