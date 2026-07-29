#!/usr/bin/env bash
set -euo pipefail

git diff --check

nix run --accept-flake-config nixpkgs#actionlint -- \
  -ignore 'label ".+" is unknown' \
  .github/workflows/ci.yml \
  .github/workflows/macos-boot-sync.yml

nix develop --quiet --command scripts/ci/check-code-format.sh
nix develop --quiet --command bash -c 'hlint lib'

nix build --quiet \
  .#cardano-wallet .#cardano-node .#cardano-cli \
  .#local-cluster .#integration-exe .#test-local-cluster-exe \
  .#unit-cardano-wallet-unit .#unit-cardano-numeric \
  .#unit-cardano-wallet-primitive \
  .#unit-cardano-wallet-secrets .#unit-cardano-wallet-test-utils \
  .#unit-cardano-wallet-launcher .#unit-cardano-wallet-network-layer \
  .#unit-cardano-wallet-application-tls .#unit-cardano-wallet-blackbox-benchmarks \
  .#unit-delta-chain .#unit-delta-store .#unit-delta-table \
  .#unit-delta-types .#unit-std-gen-seed .#unit-wai-middleware-logging \
  .#unit-benchmark-history \
  .#wallet-key-export .#wallet-key-export-test
