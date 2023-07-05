#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils buildkite-agent

set -euo pipefail

cd $(dirname $0)/..

echo "--- Build"

nix build .#ci.benchmarks.latency -o bench-latency-shelley

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

echo "+++ Run benchmark - shelley"

./bench-latency-shelley/bin/latency
