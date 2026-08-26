# Plan — #5397 Node 11.1.0 L1

1. Preserve preflight evidence: Dijkstra census `44`, Delegation positive
   control `3`, and the resolved `contra-tracer-0.2.1.1` plan identity.
2. Process L1 in topological order: application-extras, test-utils,
   crypto-primitives, iohk-monitoring-extra, temporary-extra, then
   wai-middleware-logging. Build before assuming an adaptation is needed.
3. In `iohk-monitoring-extra`, apply the compiler-derived ten-site adaptation:
   two `runTracer` to `traceWith` substitutions and eight `Tracer` constructor
   substitutions using `Tracer $ TA.emit $ \\x -> ...`, including the site in
   `ToTextTracer.hs`. Format and lint touched Haskell files, build its tests,
   and commit that one package.
4. Record no-change receipts for the other packages if their builds require no
   source change. Before each commit run the immutable L1 gate; after the
   level rerun the gate and census and hand off receipts.

Package-local Cabal bounds are intentionally excluded: ecosystem pins are
centralized in `cabal.project` and were advanced by `5d1a610`.
