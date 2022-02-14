#!/usr/bin/env -S nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#buildkite-agent --inputs-from . --command bash
# shellcheck shell=bash

set -euo pipefail

cd "$(dirname "$0" || exit 1)/.."

echo "--- Build"
nix build .#benchmarks.cardano-wallet.latency -o bench-latency-shelley

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

echo "+++ Run benchmark - shelley"

( cd lib/shelley && ../../bench-latency-shelley/bin/latency )
