#!/usr/bin/env bash

set -euo pipefail

for dir in preprod; do
  (cd "$dir" && ./download.sh)
done
