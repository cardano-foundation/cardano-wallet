#!/usr/bin/env bash

set -euo pipefail

job="${1:-}"
if [ -z "$job" ]; then
  echo "usage: $0 ( configure | build )" >& 2
  exit 1
fi

################################################################################
# Cabal config

cabal_args=()
if [ -n "${CABAL_STORE_DIR:-}" ]; then
  cabal_args+=("--store-dir=$CABAL_STORE_DIR")
fi

################################################################################
# Caching

cache_args=()
if [ -n "${CABAL_CACHE_ARCHIVE:-}" ]; then
  cache_args=("--threads=16" "--archive-uri=$CABAL_CACHE_ARCHIVE")
  if [ -n "${CABAL_STORE_DIR:-}" ]; then
    cache_args+=("--store-path=$CABAL_STORE_DIR")
  fi
fi

restore_cache() {
  if [ ${#cache_args[@]} -gt 0 ]; then
    echo "--- Restoring from cache..."
    cabal-cache sync-from-archive "${cache_args[@]}" || true
    echo
    echo "Finished restoring from cache"
    # shellcheck disable=SC2154
    trap 'status=$?; save_cache; exit $status' EXIT
  fi
}

save_cache() {
  echo "--- Saving to cache..."
  cabal-cache sync-to-archive "${cache_args[@]}" || true
  echo
  echo "Finished saving to cache"
}

################################################################################
# Build

echo "--- Updating Hackage index"

( cd "$HOME" && cabal "${cabal_args[@]}" update )

configure_args=(--enable-tests --enable-benchmarks)

if [ "$job" = configure ]; then
  echo "+++ Configuring cardano-wallet"
  cabal "${cabal_args[@]}" configure "${configure_args[@]}"
fi

echo "--- Configuring cardano-wallet (release build)"
cabal "${cabal_args[@]}" configure -frelease "${configure_args[@]}"

if [ "$job" = build ]; then
  restore_cache

  echo "+++ Building cardano-wallet"
  cabal "${cabal_args[@]}" build all

  # TODO: suspend until hls-1.7.0.1 is out
  #
  # echo "+++ haskell-language-server"
  # ln -sf hie-direnv.yaml hie.yaml

  # nix develop --command "$(dirname "$0")/hls-ci.sh"
fi
