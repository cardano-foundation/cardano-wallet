#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate stack-to-nix files in ./.stack-nix
rm -f nix/materialized/stack-nix/.stack-to-nix.cache #https://github.com/input-output-hk/haskell.nix/issues/57
nix-build -A private.project.stack-nix.passthru.generateMaterialized
./result nix/materialized/stack-nix/

# Regenerate materialized haskell-build-tools in ./materialized
nix-build -A private.project.pkgs.haskell-build-tools.regenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate materialized iohk-nix-utils in ./materialized
nix-build -A private.project.pkgs.iohk-nix-utils.regenerateMaterialized
./result/bin/regenerate-materialized-nix
