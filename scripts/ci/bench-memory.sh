#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils gnugrep gawk time haskellPackages.hp2pretty

# shellcheck shell=bash

set -euo pipefail
echo "------------------------ Setup ------------------------------------------"
TMPDIR="${TMPDIR:-/tmp}"
export TMPDIR="/$TMPDIR/bench/memory"
mkdir -p "$TMPDIR"
echo "TMPDIR: $TMPDIR"

artifact_name=memory
echo "artifact_name: $artifact_name"

export log="$artifact_name.log"
echo "log: $log"

export error_log="$artifact_name.error.log"
echo "error_log: $error_log"

echo "------------------------ Setup done -------------------------------------"

echo "------------------------ Nix call ---------------------------------------"
nix shell --quiet \
    '.#ci.benchmarks.memory' \
    '.#cardano-node' \
    '.#cardano-wallet' \
     -c "scripts/bench-memory.sh"

echo "------------------------ Nix call done ----------------------------------"

echo "------------------------ Results ----------------------------------------"

mv cardano-wallet.hp $artifact_name.hp
hp2pretty $artifact_name.hp

echo "------------------------ Results done -----------------------------------"

echo "------------------------ Cleanup ----------------------------------------"
rm -rf "$TMPDIR"
echo "------------------------ Cleanup done -----------------------------------"
