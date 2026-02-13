#!/usr/bin/env bash

set -euo pipefail

attic login adrestia https://attic.cf-app.org/ "$ATTIC_TOKEN"

# Dev shell
# shellcheck disable=SC2154
nix build --quiet ".#devShells.${system}.default.inputDerivation" -o dev-shell
attic push adrestia dev-shell

# Core runtime derivations (shared across CI, E2E, benchmarks, mithril-sync)
nix build --quiet \
  .#cardano-wallet \
  .#cardano-node \
  .#cardano-cli \
  .#local-cluster \
  .#integration-exe \
  .#e2e \
  -o ci-core
attic push adrestia ci-core

# Benchmarks
nix build --quiet .#ci.benchmarks.all -o benchmarks
attic push adrestia benchmarks

# Linux release artifacts
nix build --quiet .#ci.artifacts.linux64.release -o linux-release
attic push adrestia linux-release

# Windows cross-compiled release and test bundles
nix build --quiet \
  .#ci.artifacts.win64.release \
  .#ci.artifacts.win64.tests.wallet-unit \
  .#ci.artifacts.win64.tests.wallet-primitive \
  .#ci.artifacts.win64.tests.wallet-secrets \
  .#ci.artifacts.win64.tests.wallet-network-layer \
  .#ci.artifacts.win64.tests.wallet-test-utils \
  .#ci.artifacts.win64.tests.wallet-launcher \
  .#ci.artifacts.win64.tests.cardano-numeric \
  .#ci.artifacts.win64.tests.cardano-balance-tx \
  .#ci.artifacts.win64.tests.wallet-blackbox-benchmarks \
  .#ci.artifacts.win64.tests.delta-chain \
  .#ci.artifacts.win64.tests.delta-store \
  .#ci.artifacts.win64.tests.delta-table \
  .#ci.artifacts.win64.tests.delta-types \
  .#ci.artifacts.win64.tests.std-gen-seed \
  .#ci.artifacts.win64.tests.wai-middleware-logging \
  -o win-cross
attic push adrestia win-cross
