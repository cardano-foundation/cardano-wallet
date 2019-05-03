#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix git stack haskellPackages.hp2pretty buildkite-agent

set -euo pipefail

echo "--- Build code and benchmarks"
stack build --bench --no-run-benchmarks

echo "+++ Run benchmarks - $NETWORK"
stack bench cardano-wallet-core:restore --interleaved-output --ba "$NETWORK +RTS -N2 -qg -A1m -I0 -T -M8G -h -RTS"

ARTIFACT=lib/core/restore.hp

hp2pretty $ARTIFACT

if [ -n "${BUILDKITE:-}" ]; then
  buildkite-agent artifact upload $ARTIFACT
  printf '\033]1338;url='"artifact://restore.svg"';alt='"Heap profile"'\a\n'
fi
