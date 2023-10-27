#! /usr/bin/env bash

set -euo pipefail

echo "+++ Build & run latency benchmark"

nix run .#ci.benchmarks.latency
