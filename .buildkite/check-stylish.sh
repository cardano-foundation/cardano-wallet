#!/usr/bin/env bash

set -euo pipefail

stylish-haskell -i `git ls-files -- '*.hs'`

git diff --exit-code
