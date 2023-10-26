#! /usr/bin/env bash

set -euo pipefail

echo "+++ Build & run benchmark - shelley"

nix run .#ci.benchmarks.latency
