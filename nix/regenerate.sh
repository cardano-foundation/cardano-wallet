#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate the list of the project packages:
nix eval .#pkgs.cardanoWalletLib.projectPackageList | tr '[:blank:]' '\n' > nix/project-package-list.nix.new
mv nix/project-package-list.nix.new nix/project-package-list.nix

# Regenerate sha256map.nix
nix run .#sha256map-regenerate < stack.yaml > nix/sha256map.nix

# Regenerate stack-to-nix files in nix/materialized/stack-nix
rm -f nix/materialized/stack-nix/.stack-to-nix.cache #https://github.com/input-output-hk/haskell.nix/issues/57
nix build .#generateMaterialized
./result nix/materialized/stack-nix/

# Regenerate materialized haskell-build-tools in nix/materialized/*
nix build .#buildToolsGenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate materialized iohk-nix-utils in nix/materialized/*
nix build .#iohkNixGenerateMaterialized
./result/bin/regenerate-materialized-nix
