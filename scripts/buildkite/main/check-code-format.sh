#!/usr/bin/env bash

set -euo pipefail

# Force UTF-8 encoding:
LANG=C.UTF-8

echo "+++ Check code format: fourmolu"

# shellcheck disable=SC2046
fourmolu --mode check $(git ls-files -- '*.hs')

echo "+++ Check code format: cabal-fmt"

find lib -name '*.cabal' -exec cabal-fmt -i {} \;

git diff --exit-code
