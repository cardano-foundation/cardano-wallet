#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils gnugrep gawk time haskellPackages.hp2pretty buildkite-agent gnuplot

set -euo pipefail

artifact_name=memory

export TMPDIR="/$TMPDIR/bench/memory"
mkdir -p $TMPDIR

echo "------------------------ Build ------------------------------------------"
nix build -o bench-wallet

echo "------------------------ Run --------------------------------------------"
wallet_exe=`pwd`/bench-wallet/bin/cardano-wallet
node_exe=`which cardano-node`
node_db="`pwd`/lib/wallet-benchmarks/data/membench-snapshot.tgz"
work_dir=`pwd`
bench="cabal bench cardano-wallet-benchmarks:bench:memory \
    --benchmark-option=--snapshot=$node_db \
    --benchmark-option=--wallet=$wallet_exe \
    --benchmark-option=--node=$node_exe \
    --benchmark-option=--work-dir=$work_dir "

$bench

echo "------------------------ Results ----------------------------------------"

mv cardano-wallet.hp $artifact_name.hp
hp2pretty $artifact_name.hp

if [ -n "${BUILDKITE:-}" ]; then
  echo "--- Upload"
  buildkite-agent artifact upload $artifact_name.svg

  echo "+++ Heap profile"
  printf '\033]1338;url='"artifact://$artifact_name.svg"';alt='"Heap profile"'\a\n'

fi

echo "------------------------ Cleanup ----------------------------------------"

rm -rf $TMPDIR
