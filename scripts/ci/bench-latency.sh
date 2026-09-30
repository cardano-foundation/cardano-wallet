#! /usr/bin/env bash

set -euo pipefail

echo "+++ Build & run latency benchmark"

if [[ "${CONTEXT_BENCH:-}" == 1 ]]; then
  : "${CONTEXT_BENCH_BASELINE_EXE:?Set CONTEXT_BENCH_BASELINE_EXE to the optimized correctness-fixed baseline latency executable}"
  : "${CONTEXT_CORPUS_DIR:?Set CONTEXT_CORPUS_DIR to an absolute directory for frozen request bodies}"
  if [[ "${LOCAL_CLUSTER_ERA:-conway}" != conway ]]; then
    printf 'Context benchmark requires LOCAL_CLUSTER_ERA=conway\n' >&2
    exit 1
  fi
  export LOCAL_CLUSTER_ERA=conway
  export LOCAL_CLUSTER_CONFIGS="${LOCAL_CLUSTER_CONFIGS:-$PWD/lib/local-cluster/test/data/cluster-configs}"
  export CARDANO_WALLET_TEST_DATA="${CARDANO_WALLET_TEST_DATA:-$PWD/lib/integration/test/data}"
  mkdir -p "$CONTEXT_CORPUS_DIR"
  CONTEXT_BENCH_BASELINE_EXE="$(realpath "$CONTEXT_BENCH_BASELINE_EXE")"
  CONTEXT_CORPUS_DIR="$(realpath "$CONTEXT_CORPUS_DIR")"
  export CONTEXT_BENCH_BASELINE_EXE CONTEXT_CORPUS_DIR
  export CONTEXT_BENCH_COORDINATE=1
  printf 'context_build,git_commit=%s\n' "$(git rev-parse HEAD)"
  baseline_root="$(git -C "$(dirname "$CONTEXT_BENCH_BASELINE_EXE")" rev-parse --show-toplevel)"
  printf 'context_baseline,root=%s,git_commit=%s\n' \
    "$baseline_root" "$(git -C "$baseline_root" rev-parse HEAD)"
  printf 'context_baseline,tracked_delta_sha256='
  git -C "$baseline_root" diff --binary HEAD | sha256sum
  printf 'context_final,tracked_delta_sha256='
  git diff --binary HEAD | sha256sum
  sha256sum cabal.project flake.lock
  printf 'context_machine,os=%s,arch=%s,cpus=%s\n' \
    "$(uname -s)" "$(uname -m)" "$(getconf _NPROCESSORS_ONLN)"
  printf 'context_affinity,parent=%s,worker=%s\n' \
    "$(taskset -pc "$$")" "${CONTEXT_BENCH_WORKER_CPUSET:-inherited}"
  nix develop -c cabal build cardano-wallet-benchmarks:latency -O2
  benchmark="$(nix develop -c cabal list-bin cardano-wallet-benchmarks:latency -O2)"
  sha256sum "$benchmark" "$CONTEXT_BENCH_BASELINE_EXE"
  nix shell --quiet \
    '.#local-cluster' \
    '.#cardano-node' \
    '.#cardano-cli' \
    -c bash -c '
      cardano-node --version
      cardano-cli --version
      exec "$@"
    ' context-bench "$benchmark" \
    "--cluster-configs=$PWD/lib/local-cluster/test/data/cluster-configs" \
    +RTS -T
  exit
fi

nix shell --quiet \
  '.#local-cluster' \
  '.#cardano-node' \
  '.#cardano-wallet' \
  '.#ci.benchmarks.latency' \
  -c latency
