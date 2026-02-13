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
nix build -L .#ci.benchmarks.restore -o bench-restore

echo "--- Run benchmarks - $network"

CARDANO_NODE_CONFIGS=$(pwd)/configs/cardano

TO_TIP_TIMEOUT=4

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

echo "+++ Memory Summary - $network"

# time -v reports Maximum resident set size (kB) — this includes
# all child processes (notably cardano-node which uses ~11 GB).
time_v_rss=$(grep "Maximum resident set size" "$total_time" 2>/dev/null \
    | awk '{print $NF}' || true)
if [ -n "$time_v_rss" ]; then
    time_v_rss_mb=$(awk "BEGIN {printf \"%.1f\", $time_v_rss / 1024}")
    echo "  Total peak RSS (incl. cardano-node): ${time_v_rss_mb} MB  [from time -v]"
fi

# The wallet binary now self-reports its own RSS via /proc/self/status.
wallet_rss=$(grep "Wallet peak RSS:" "$log" 2>/dev/null | head -1 || true)
if [ -n "$wallet_rss" ]; then
    echo "  $wallet_rss"
fi

# GHC RTS stats from the benchmark (requires -T RTS flag).
rts_section=$(sed -n '/=== GHC RTS Memory Stats ===/,/^$/p' "$log" 2>/dev/null || true)
if [ -n "$rts_section" ]; then
    echo "$rts_section"
fi

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

if [ -z "$(cat "$results")" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  exit 1
fi
