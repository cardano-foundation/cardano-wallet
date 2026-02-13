#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils
# shellcheck shell=bash

set -euo pipefail

bench_name=bench-db

export TMPDIR="/tmp/bench/db"
mkdir -p $TMPDIR

rm -rf $bench_name

echo "--- Build"
nix build --quiet .#ci.benchmarks.db -o $bench_name

echo "+++ Run benchmark"

./$bench_name/bin/db --json $bench_name.json \
    -o $bench_name.html \
    | tee $bench_name.txt

printf 'Link to \033]1339;url=artifact://'%s';content='"Benchmark Report"'\a\n' \
    $bench_name.html

echo "--- Upload report"
