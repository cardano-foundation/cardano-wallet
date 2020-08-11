#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

cd `dirname $0`/..

echo "--- Build"
nix-build -A benchmarks.cardano-wallet.latency -o bench-latency-shelley
nix-build -A benchmarks.cardano-wallet-jormungandr.latency -o bench-latency-jormungandr

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

echo "+++ Run benchmark - shelley"

( cd shelley && ../bench-latency-shelley/bin/latency )

echo "+++ Run benchmark - jormungandr"

( cd jormungandr && ../bench-latency-jormungandr/bin/latency )
