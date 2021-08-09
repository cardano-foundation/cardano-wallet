#!/usr/bin/env bash

set -euo pipefail

echo "+++ stylish-haskell"

stylish-haskell -i `git ls-files -- '*.hs'`

git diff --exit-code
