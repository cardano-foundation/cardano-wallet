#!/usr/bin/env bash

set -euox pipefail

attic login adrestia https://attic.cf-app.org/ "$ATTIC_TOKEN"

# Dev shell
# shellcheck disable=SC2154
nix build --log-format raw-with-logs ".#devShells.${system}.default.inputDerivation" -o dev-shell
attic push adrestia dev-shell

# Core runtime derivations (shared across CI, E2E, benchmarks, mithril-sync)
nix build --log-format raw-with-logs \
  .#cardano-wallet \
  .#cardano-node \
  .#cardano-cli \
  .#local-cluster \
  .#integration-exe \
  .#e2e \
  -o ci-core
attic push adrestia ci-core

# Benchmarks
nix build --log-format raw-with-logs .#ci.benchmarks.all -o benchmarks
attic push adrestia benchmarks

# Linux release artifacts
nix build --log-format raw-with-logs .#ci.artifacts.linux64.release -o linux-release
attic push adrestia linux-release
