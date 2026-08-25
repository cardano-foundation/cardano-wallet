# Plan — #5397 Node 11.1.0 L0

1. Preserve the preflight evidence: Dijkstra census `44`, Delegation positive
   control `3`, and the reproduced `tracer-transformers` compiler failure.
2. Make the smallest resolver/constraint adaptation supported by the freeze or
   a successful Cabal solve, then prove the dependency shell can build.
3. Visit L0 packages in order: numeric, wallet-read, faucet, flaky-tests,
   std-gen-seed, text-class. For each, make only mechanical API adaptation,
   format/lint touched files, build package and tests, commit once if changed,
   and run the immutable L0 gate before the next commit.
4. Re-run the Dijkstra census after the final L0 commit. Hand the six commit
   receipts, test outcomes, gate evidence, and final `44` to the ticket owner.

The package-local Cabal-bound step from the generic dependency workflow is
inapplicable: this repository centralizes ecosystem pins in `cabal.project`;
the sole existing Cabal bounds are for the two wallet-local delta packages.
