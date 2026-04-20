#!/usr/bin/env bash

set -euo pipefail

# Force UTF-8 encoding:
LANG=C.UTF-8

echo "+++ Check code format: fourmolu"

# shellcheck disable=SC2046
fourmolu --mode check $(git ls-files -- '*.hs')

echo "+++ Check code format: cabal-fmt"

find lib -name '*.cabal' -exec cabal-fmt -i {} \;

echo "+++ Check code format: nixfmt"

# shellcheck disable=SC2046
nixfmt $(git ls-files -- '*.nix')

git diff --exit-code
