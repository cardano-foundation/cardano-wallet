# Spec: Stop building the cardano-cli library for local-cluster

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5088

## P1 user story

As a cardano-wallet contributor, I can build `local-cluster` without compiling
the `cardano-cli` Haskell library, while local clusters continue to use the
separately supplied `cardano-cli` executable at runtime.

## Problem

`ConfiguredPool.hs` imports `Cardano.CLI.Type.Key` only to read verification
key text-envelope files. That import requires `local-cluster` to declare a
Haskell dependency on `cardano-cli`, so the `.#local-cluster` Nix closure
builds `cardano-cli-lib-cardano-cli-11.0.0.0` from CHaP even though the runtime
CLI executable is already supplied separately.

The old git `source-repository-package` workaround is already absent on
`master`; this ticket removes the remaining library dependency.

## Functional requirements

- FR1: `local-cluster` reads the generated stake-pool, VRF, and stake
  verification-key files through the existing `cardano-api` dependency.
- FR2: `lib/local-cluster/` contains no Haskell import from `Cardano.CLI`.
- FR3: the `local-cluster` library stanza has no `cardano-cli` dependency.
- FR4: the `.#local-cluster` derivation closure contains no
  `cardano-cli` Haskell library derivation.
- FR5: the runtime `cardano-cli` executable remains available to cluster
  processes and tests; runtime command execution is not refactored.

## Success criteria

- Removing only the Cabal dependency makes the unchanged CLI import fail to
  build, proving the dependency check can go red.
- `nix build --quiet --no-link .#local-cluster` exits 0 after the reader
  replacement.
- `nix path-info --derivation --recursive .#local-cluster` contains no path
  matching `-lib-cardano-cli-`.
- `just test-local-cluster` reports 39 examples and 0 failures.
- Fourmolu, cabal-fmt, nixfmt, HLint, and `git diff --check` are clean.

## Out of scope

- Removing the `cardano-cli` executable from runtime shells or workflows.
- Changing project-wide `cardano-cli` constraints in `cabal.project`.
- Refactoring other cluster key or command helpers.
