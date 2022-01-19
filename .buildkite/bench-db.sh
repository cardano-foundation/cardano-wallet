#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils buildkite-agent

set -euo pipefail

bench_name=bench-db

rm -rf $bench_name

echo "--- Build"
nix build .#benchmarks.cardano-wallet-core.db -o $bench_name

echo "+++ Run benchmark"

if [ -n "${SCRATCH_DIR:-}" ]; then
  mkdir -p "$SCRATCH_DIR"
  export TMPDIR="$SCRATCH_DIR"
fi

./$bench_name/bin/db --json $bench_name.json -o $bench_name.html | tee $bench_name.txt

printf 'Link to \033]1339;url=artifact://'$bench_name.html';content='"Benchmark Report"'\a\n'

echo "--- Upload report"

if [ -n "${BUILDKITE:-}" ]; then
  buildkite-agent artifact upload "$bench_name.html"
  buildkite-agent artifact upload "$bench_name.json"

  # Requires buildkite-agent 3.x
  # cat << EOF | buildkite-agent annotate --style "info"
  # Read the <a href="artifact://$bench_name.html">benchmark results</a>
  # EOF
fi
