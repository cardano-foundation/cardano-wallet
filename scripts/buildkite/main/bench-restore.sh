#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils gnugrep gawk time haskellPackages.hp2pretty buildkite-agent gnuplot

set -euo pipefail

if [ "$#" -lt 1 ]; then
  echo "usage: $0 NETWORK"
  exit 1
fi

network=$1
artifact_name=restore-$network
log=restore.log
results=restore-$network.txt
total_time=restore-time.txt

export TMPDIR="/$TMPDIR/bench/restore"
mkdir -p $TMPDIR

: "${node_db:=$HOME/node-db-$network}"

echo "--- Build"
nix build .#ci.benchmarks.restore -o bench-restore

CARDANO_NODE_CONFIGS=`pwd`/configs/cardano

bench="./bench-restore/bin/restore $network --node-db $node_db --cardano-node-configs $CARDANO_NODE_CONFIGS"

echo "--- Run benchmarks - $network"

command time -o $total_time -v $bench +RTS -N2 -qg -A1m -I0 -T -M16G -h -RTS 2>&1 | tee $log

grep -v INFO $log | awk '/All results/,EOF { print $0 }' >$results

echo "+++ Results - $network"

cat $results

mv restore.hp $artifact_name.hp
hp2pretty $artifact_name.hp

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
  buildkite-agent artifact upload $artifact_name.svg
  buildkite-agent artifact upload $results

  for file in *.timelog; do
    # upload raw data
    buildkite-agent artifact upload $file
    # clean data (make time relative) for plot
    cat $file | awk 'BEGIN {t0 = 0}{if (t0 == 0) {t0 = $1} else {print $1-t0,$2}}' >$file.dat
  done

  for file in *.json; do
    buildkite-agent artifact upload $file
  done

  # Plots all .log files in a single plot;
  echo $GNUPLOT_PROGRAM | gnuplot
  buildkite-agent artifact upload plot.svg

  echo "+++ Heap profile"
  printf '\033]1338;url='"artifact://$artifact_name.svg"';alt='"Heap profile"'\a\n'
  echo "+++ Restore plot"
  printf '\033]1338;url='"artifact://plot.svg"';alt='"Restore plot"'\a\n'
fi

if [ -z "$(cat $results)" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  exit 1
fi
