#!/usr/bin/env bash

set -euo pipefail

echo "+++ Check code format: imports"

stylish-haskell -v --config .stylish-haskell.yaml --inplace `git ls-files -- '*.hs'`

echo "+++ Check code format: blank lines"

find . -type f -name '*.hs' -exec sed -i -r '/^$/N;/^\n$/D' {} +

git diff --exit-code
