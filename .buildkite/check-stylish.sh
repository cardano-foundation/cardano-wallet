#!/usr/bin/env bash

set -euo pipefail

echo "+++ stylish-haskell"

stylish-haskell -v --config .stylish-haskell.yaml --inplace `git ls-files -- '*.hs'`

git diff --exit-code
