#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

cd `dirname $0`/..

echo "--- Build"
nix-build -A benchmarks.cardano-wallet-jormungandr.latency -o bench-latency-jormungandr
nix-build -A benchmarks.cardano-wallet-byron.latency -o bench-latency-byron
nix-build -A benchmarks.cardano-wallet-shelley.latency -o bench-latency-shelley

# Note: the tracing will not work if program output is piped
# to another process (e.g. "tee").
# It says "Error: Switchboard's queue full, dropping log items!"

echo "+++ Run benchmark - byron"

( cd lib/byron && ../../bench-latency-byron/bin/latency )

echo "+++ Run benchmark - shelley"

( cd lib/shelley && ../../bench-latency-shelley/bin/latency )

echo "+++ Run benchmark - jormungandr"

( cd lib/jormungandr && ../../bench-latency-jormungandr/bin/latency )
