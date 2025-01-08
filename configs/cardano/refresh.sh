#!/usr/bin/env bash

set -euo pipefail

for dir in preprod mainnet preview private sanchonet; do
  (cd "$dir" && ./download.sh)
done