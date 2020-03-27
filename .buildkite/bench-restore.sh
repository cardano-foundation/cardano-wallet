#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gawk time haskellPackages.hp2pretty buildkite-agent gnuplot

set -euo pipefail

target=byron
artifact_name=restore-$target-$NETWORK
log=restore.log
results=restore-$target-$NETWORK.txt
total_time=restore-time.txt
export NODE_DB=$HOME/node-db-$NETWORK

echo "--- Build"

echo "pwd=$(pwd)"
echo "NODE_DB=$NODE_DB"

nix-build -A benchmarks.cardano-wallet-$target.restore -o bench-$target-restore

bench="./bench-$target-restore/bin/restore --$NETWORK"

echo "--- Run benchmarks - $target - $NETWORK"

if [ -n "${SCRATCH_DIR:-}" ]; then
  mkdir -p "$SCRATCH_DIR"
  export HOME="$SCRATCH_DIR"
fi

command time -o $total_time -v $bench +RTS -N2 -qg -A1m -I0 -T -M8G -h -RTS 2>&1 | tee $log

grep -v INFO $log | awk '/All results/,EOF { print $0 }' > $results

echo "+++ Results - $target - $NETWORK"

cat $results

mv restore.hp $artifact_name.hp
hp2pretty $artifact_name.hp

GNUPLOT_PROGRAM=$(cat <<EOP
set timefmt "%s";
set format y "%Hh%Mm";
set ydata time;

set ylabel "time to restore";
set xlabel "block height";
show ylabel;
show xlabel;

set term svg dynamic size 1200,700;
set output "plot.svg";
set title "Restoring byron wallets on $NETWORK";

set key left top;

FILES = system("ls -1 *.dat");
LABEL = system("ls -1 *.dat");

plot for [i=1:words(FILES)] word(FILES,i) u 2:1 title word(LABEL,i) noenhanced with lines
EOP
);


if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload $artifact_name.svg
  buildkite-agent artifact upload $results

  for file in *.timelog; do
     # upload raw data
     buildkite-agent artifact upload $file;
     # clean data (make time relative) for plot
     cat $file | awk 'BEGIN {t0 = 0}{if (t0 == 0) {t0 = $1} else {print $1-t0,$2}}' > $file.dat;
  done;

  # Plots all .log files in a single plot;
  echo $GNUPLOT_PROGRAM | gnuplot
  buildkite-agent artifact upload plot.svg

  echo "+++ Heap profile"
  printf '\033]1338;url='"artifact://$artifact_name.svg"';alt='"Heap profile"'\a\n'
  echo "+++ Restore plot"
  printf '\033]1338;url='"artifact://plot.svg"';alt='"Restore plot"'\a\n'
fi
