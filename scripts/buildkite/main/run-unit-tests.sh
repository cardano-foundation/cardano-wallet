#!/usr/bin/env bash
set -euo pipefail

# Run unit tests with memory limits to prevent OOM
# Each test suite runs with +RTS -M2G to cap heap at 2GB

echo "+++ Running unit tests with memory limits"

# List of unit test targets (from justfile)
TESTS=(
    "cardano-wallet-application-tls:unit"
    "cardano-balance-tx:test"
    "cardano-numeric:unit"
    "cardano-wallet-blackbox-benchmarks:unit"
    "cardano-wallet-launcher:unit"
    "cardano-wallet-network-layer:unit"
    "cardano-wallet-primitive:test"
    "cardano-wallet-secrets:test"
    "cardano-wallet-test-utils:unit"
    "cardano-wallet-unit:unit"
    "delta-chain:unit"
    "delta-store:unit"
    "delta-table:unit"
    "delta-types:unit"
    "std-gen-seed:unit"
    "wai-middleware-logging:unit"
)

# Run each test suite sequentially with memory limit
for test in "${TESTS[@]}"; do
    echo "--- Running $test"
    cabal test "$test" -O0 -v0 --test-options "+RTS -M2G -RTS" || exit 1
done

echo "+++ All unit tests passed"
