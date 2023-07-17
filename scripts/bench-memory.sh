#! /usr/bin/env bash
echo "------------------------ Setup nix ---------------------------------------"
wallet_exe="$(which cardano-wallet)"
echo "wallet_exe: $wallet_exe"
node_exe="$(which cardano-node)"
echo "node_exe: $node_exe"
node_db="$(pwd)/lib/wallet-benchmarks/data/membench-snapshot.tgz"
echo "node_db: $node_db"
work_dir="$(pwd)"
echo "work_dir: $work_dir"
bench="memory \
    --snapshot=$node_db \
    --wallet=$wallet_exe \
    --node=$node_exe \
    --work-dir=$work_dir "
echo "bench command: $bench"
echo "------------------------ Setup nix done ---------------------------------"

echo "------------------------ Run --------------------------------------------"
$bench 2>"${error_log:?}" 1>"${log:?}"
echo "------------------------ Run done ---------------------------------------"
