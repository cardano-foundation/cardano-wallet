#!/usr/bin/env bash

set -euo pipefail

echo "+++ fourmolu"

git ls-files -z '*.hs' | xargs -0 fourmolu --mode inplace

git diff --exit-code
