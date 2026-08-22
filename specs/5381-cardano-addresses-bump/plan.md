# Plan

## Strategy

Refresh the old pre-release candidate against current `origin/master`, locate
the first CHaP history point containing the released compatible package set,
and update only the dependency inputs and any direct pins proven necessary by
the solver. Preserve the common CHaP horizon across Cabal and Nix.

## Live boundaries

- CHaP package-index history determines availability and revised bounds.
- Cabal solver output proves the concrete dependency closure.
- Nix input metadata proves that the second resolver sees the same CHaP
  history; CI supplies the expensive Nix build proof.

## Slice S1

Produce one reviewable dependency bump with plan-resolution, affected-build,
and focused-test evidence. No wallet behaviour changes are planned.

## Resource constraints

Check root free space before build-inducing commands and stop below 40 GiB.
Use `-O0`; do not run a full local `nix flake check` or full Nix artifact build.
