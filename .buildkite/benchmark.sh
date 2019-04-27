#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix git stack haskellPackages.hp2pretty buildkite-agent

set -euo pipefail

netname="${1-}"

if [ -z "$netname" ]; then
   echo "usage: benchmark.sh NETWORK"
   exit 1
fi

echo "--- Build code and benchmarks"
stack build --bench --no-run-benchmarks

export NETWORK=$netname

echo "+++ Run benchmarks"
stack bench cardano-wallet:restore --interleaved-output --ba "$netname +RTS -N2 -qg -A1m -I0 -T -M1G -h -RTS"

hp2pretty restore.hp

if [ -n "${BUILDKITE:-}" ]; then
  buildkite-agent artifact upload restore.svg
  printf '\033]1338;url='"artifact://restore.svg"';alt='"Heap profile"'\a\n'
fi
