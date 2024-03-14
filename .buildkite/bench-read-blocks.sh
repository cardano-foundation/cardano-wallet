#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils gnugrep gawk buildkite-agent

set -euo pipefail

# C.UTF-8 = UTF-8 based locale that is not associated with a natural language.
# This seems necessary so that programs compiled with GHC don't choke,
# see also <https://stackoverflow.com/a/63751678>.
export LC_ALL=C.UTF-8


export TMPDIR="/$TMPDIR/bench/read-blocks"
mkdir -p $TMPDIR

log=read-blocks.log


echo "--- Build"
nix --version

nix build .#ci.benchmarks.read-blocks -o bench-read-blocks
bench="./bench-read-blocks/bin/read-blocks"


echo "--- Run benchmark"

$bench +RTS -N2 -qg -A1m -I0 -T -M16G -RTS 2>&1 | tee $log
# Reminder on GHC RTS options:
#   -N ⟨x⟩
#         Use ⟨x⟩ simultaneous threads when running the program.
#   -qg ⟨gen⟩
#         Use parallel GC in generation ⟨gen⟩ and higher.
#         Omitting ⟨gen⟩ turns off the parallel GC completely,
#         reverting to sequential GC.
#   -A    allocation area size used by the garbage collector
#   -I0   disables the idle GC.
#   -T    produce runtime-system statistics, such as
#         the amount of time spent executing the program
#         and in the garbage collector.
#         -T collects the data, but produces no output.
#   -M ⟨size⟩
#         Set the maximum heap size to ⟨size⟩ bytes.
#   -h    Generates a basic heap profile, in the file prog.hp.

echo "--- Results"

if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload $log

fi

if [ -z "$(cat $log)" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  exit 1
fi
