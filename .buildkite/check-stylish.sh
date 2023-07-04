#!/usr/bin/env bash

set -euo pipefail

echo "+++ stylish-haskell"
# C.UTF-8 = UTF-8 based locale that is not associated with a natural language.
# This seems necessary so that programs compiled with GHC don't choke,
# see also <https://stackoverflow.com/a/63751678>.
export LANG=C.UTF-8

stylish-haskell -v --config .stylish-haskell.yaml --inplace `git ls-files -- '*.hs'`

git diff --exit-code
