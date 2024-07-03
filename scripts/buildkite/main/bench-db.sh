#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils buildkite-agent
# shellcheck shell=bash

set -euo pipefail

bench_name=bench-db

export TMPDIR="/tmp/bench/db"
mkdir -p $TMPDIR

rm -rf $bench_name

echo "--- Build"
nix build .#ci.benchmarks.db -o $bench_name

echo "+++ Run benchmark"

./$bench_name/bin/db --json $bench_name.json \
    -o $bench_name.html \
    | tee $bench_name.txt

printf 'Link to \033]1339;url=artifact://'%s';content='"Benchmark Report"'\a\n' \
    $bench_name.html

echo "--- Upload report"

if [ -n "${BUILDKITE:-}" ]; then
  buildkite-agent artifact upload "$bench_name.html"
  buildkite-agent artifact upload "$bench_name.json"

  # Requires buildkite-agent 3.x
  # cat << EOF | buildkite-agent annotate --style "info"
  # Read the <a href="artifact://$bench_name.html">benchmark results</a>
  # EOF
fi
