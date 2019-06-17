#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git coreutils

set -euo pipefail

nix_dir=$(dirname $0)
json="$nix_dir/iohk-nix-src.json"

nix-prefetch-git --quiet --url https://github.com/input-output-hk/iohk-nix | tee "$json"
echo "Updated $json"
