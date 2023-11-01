#!/usr/bin/env bash

set -euo pipefail

echo "+++ Check code format: imports"

stylish-haskell -v --config .stylish-haskell.yaml --inplace `git ls-files -- '*.hs'`

echo "+++ Check code format: no multiple consecutive blank lines"

find . -type f -name '*.hs' -exec sed -i -r '/^$/N;/^\n$/D' {} +

echo "+++ Check code format: no blank lines at the ends of files"

find . -type f -name '*.hs' -exec sed -i -r -e :a -e '/^\n*$/{$d;N;ba' -e '}' {} +

git diff --exit-code
