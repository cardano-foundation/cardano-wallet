#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

if [ "$#" -lt 3 ]; then
  echo "usage: $0 NETWORK BENCH NODE_DB"
  exit 1
fi

network=$1
bench=$2
node_db=$3
artifact_name=restore-$network
log=restore.log
results=restore-$network.txt
total_time=restore-time.txt

export TMPDIR="$TMPDIR/bench/restore"
mkdir -p "$TMPDIR"

echo "--- Build"
nix build .#ci.benchmarks.restore -o bench-restore

echo "--- Run benchmarks - $network"

CARDANO_NODE_CONFIGS=$(pwd)/configs/cardano

if [ -n "${BUILDKITE:-}" ]; then
    TO_TIP_TIMEOUT=$(buildkite-agent meta-data get to-tip-timeout --default '4')
else
    TO_TIP_TIMEOUT=4
fi

BENCH_CMD="./bench-restore/bin/restore $network --node-db $node_db --bench-name $bench"
BENCH_CMD+=" --cardano-node-configs $CARDANO_NODE_CONFIGS "
BENCH_CMD+=" +RTS -N -qg -A1m -I0 -T -M16G -hT -RTS"

if [ "$TO_TIP_TIMEOUT" != "infinite" ]; then
    BENCH_CMD+=" --to-tip-timeout $TO_TIP_TIMEOUT"
fi

# When testing this script itself,
# use the  timeout  command to cut short execution time
# BENCH_CMD="timeout -s INT 6s "$BENCH_CMD""
echo "$BENCH_CMD"

# shellcheck disable=SC2086
command time -o "$total_time" -v $BENCH_CMD 2>&1 | tee $log || true

grep -v INFO $log | awk '/All results/,EOF { print $0 }' >"$results"

echo "+++ Results - $network"

cat "$results"

mv restore.hp "$artifact_name".hp
hp2pretty "$artifact_name".hp

GNUPLOT_PROGRAM=$(
  cat <<EOP
set timefmt "%s";
set format x "%Hh%Mm";
set xdata time;

set xlabel "time";
set ylabel "block height";
show xlabel;
show ylabel;

set terminal svg dynamic size 1200,700 background rgb 'white';
set output "plot.svg";
set title "Restoring wallets on $network";

set key left top;

FILES = system("ls -1 *.dat");
LABEL = system("ls -1 *.dat");

plot for [i=1:words(FILES)] word(FILES,i) u 1:2 title word(LABEL,i) noenhanced with lines
EOP
)

if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload "$artifact_name".svg
  buildkite-agent artifact upload "$results"

  for file in *.timelog; do
    # upload raw data
    buildkite-agent artifact upload "$file"
    # clean data (make time relative) for plot
    # shellcheck disable=SC2002
    cat "$file" | awk 'BEGIN {t0 = 0}{if (t0 == 0) {t0 = $1} else {print $1-t0,$2}}' > "$file".dat
  done

  for file in *.json; do
    buildkite-agent artifact upload "$file"
  done

  # Plots all .log files in a single plot;
  echo "$GNUPLOT_PROGRAM" | gnuplot
  buildkite-agent artifact upload plot.svg

  echo "+++ Heap profile"
  printf '\033]1338;url='"artifact://%s"';alt='"Heap profile"'\a\n' "artifact_name.svg"
  echo "+++ Restore plot"
  printf '\033]1338;url='"artifact://plot.svg"';alt='"Restore plot"'\a\n'
fi

if [ -z "$(cat "$results")" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  exit 1
fi
