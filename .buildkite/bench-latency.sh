#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

bench_name_jormungandr=bench-latency-jormungandr
bench_name_byron=bench-latency-byron

echo "--- Build"
nix-build -A benchmarks.cardano-wallet-jormungandr.latency -o $bench_name_jormungandr
nix-build -A benchmarks.cardano-wallet-byron.latency -o $bench_name_byron

echo "+++ Run benchmark"

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

./$bench_name_jormungandr/bin/latency
./$bench_name_byron/bin/latency
