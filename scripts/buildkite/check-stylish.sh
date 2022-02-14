#!/usr/bin/env bash

set -euo pipefail

echo "+++ stylish-haskell"

git ls-files -- '*.hs' | xargs stylish-haskell -i

git diff --exit-code
