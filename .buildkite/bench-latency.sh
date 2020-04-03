#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

bench_name=bench-latency

echo "--- Build"
nix-build -A benchmarks.cardano-wallet-itn.latency -o $bench_name

echo "+++ Run benchmark"

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

./$bench_name/bin/latency
