#!/usr/bin/env bash

set -euo pipefail

echo "--- Cabal update"
cabal update
echo

echo "+++ Cabal configure -frelease"
cabal configure -frelease --enable-tests --enable-benchmarks
echo

echo "+++ Cabal configure"
cabal configure --enable-tests --enable-benchmarks
echo

# TODO: suspend until hls-1.7.0.1 is out
#
# echo "+++ haskell-language-server"
# ln -sf hie-direnv.yaml hie.yaml
# haskell-language-server lib/wallet/src/Cardano/Wallet.hs
# echo
