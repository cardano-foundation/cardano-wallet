#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils gnugrep gawk buildkite-agent

set -euo pipefail

# Set locale to C.UTF-8 to avoid issues with GHC compilation
export LC_ALL=C.UTF-8

# Set temporary directory for benchmarks
export TMPDIR="/${TMPDIR:-/tmp}/bench/api"
mkdir -p "$TMPDIR"

bench=api
log=api.log
results=api.txt
total_time=api-time.txt

echo "--- Build"
nix --version

# Build benchmarks using Nix
nix build .#ci.benchmarks.api -o bench-api
bench="./bench-api/bin/api lib/benchmarks/data/api-bench"

echo "--- Run benchmark"

# Run benchmarks with RTS parameters
$bench +RTS -N2 -qg -A1m -I0 -T -M16G -RTS 2>&1 | tee "$log"

echo "--- Results"

# Extract and display results
grep -v INFO "$log" | awk '/All results/,EOF { print $0 }' > "$results"
cat "$results"

# Upload results to Buildkite if environment variable is set
if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload "$results"

  for file in *.json; do
    buildkite-agent artifact upload "$file"
  done
fi

# Check for the presence of results and handle errors
if [ -z "$(cat "$results")" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  exit 1
fi
