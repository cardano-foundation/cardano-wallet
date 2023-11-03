#! /usr/bin/env bash

set -euo pipefail

echo "+++ Build & run latency benchmark"

nix shell \
  '.#local-cluster' \
  '.#cardano-node' \
  '.#cardano-wallet' \
  '.#ci.benchmarks.latency' \
  -c latency
