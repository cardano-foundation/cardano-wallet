#!/usr/bin/env bash

set -euo pipefail

echo "--- Cabal update"
# Workaround: cabal 3.16 expects 00-index.tar.gz but haskell.nix CHaP
# local repo has 01-index.tar.gz. Create a writable copy with symlink
# and redirect cabal.project.local to it.
if [ -f cabal.project.local ]; then
  chap_url=$(grep -oP 'url: file://\K.*' cabal.project.local || true)
  if [ -n "$chap_url" ] && [ -f "$chap_url/01-index.tar.gz" ] && [ ! -f "$chap_url/00-index.tar.gz" ]; then
    compat_repo=$(mktemp -d)/chap-repo
    cp -rs "$chap_url" "$compat_repo"
    chmod u+w "$compat_repo"
    ln -s 01-index.tar.gz "$compat_repo/00-index.tar.gz"
    sed -i "s|$chap_url|$compat_repo|" cabal.project.local
  fi
fi
cabal update
echo

echo "+++ Cabal configure -frelease"
cabal configure -frelease --enable-tests --enable-benchmarks
echo

echo "+++ Cabal configure"
cabal configure --enable-tests --enable-benchmarks
echo
