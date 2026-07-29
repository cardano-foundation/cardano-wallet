# Tasks: Stop building the cardano-cli library for local-cluster

## Slice A — replace the CLI key reader and drop the dependency

- [x] T508801 Record RED evidence that removing the Cabal dependency exposes
  the remaining `Cardano.CLI.Type.Key` import.
- [x] T508802 Replace the CLI-only verification-key reader with
  `Cardano.Api.readFileTextEnvelope` and remove redundant imports and
  constraints.
- [x] T508803 Remove `cardano-cli` from the `local-cluster` library
  dependencies and prove the Nix closure no longer builds its Haskell library.
- [x] T508804 Run `./gate.sh` and commit the slice as
  `build(local-cluster): drop cardano-cli library dependency`.
