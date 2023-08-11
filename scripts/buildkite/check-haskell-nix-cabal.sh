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
