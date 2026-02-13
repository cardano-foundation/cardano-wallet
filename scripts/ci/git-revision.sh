#! /usr/bin/env nix
#! nix shell ../../#cardano-wallet nixpkgs#gnused nixpkgs#git --command bash
# shellcheck shell=bash

set -euo pipefail

# check git is not dirty
if [ -n "$(git status --porcelain)" ]; then
    echo "Git repository is dirty. Please commit your changes."
    exit 1
fi

flake_revision=$(git rev-parse HEAD)

# example
# v2025-03-04 (git revision: 4617e679b2e7b07300a1b32892f157b2207635b9)
exe_revision=$(cardano-wallet version | sed -n 's/.*git revision: \([a-f0-9]\+\).*/\1/p')

if [ "$flake_revision" != "$exe_revision" ]; then
    echo "The flake revision ($flake_revision) does not match the executable revision ($exe_revision)."
    echo "Please commit to update the executable."
    exit 1
fi
