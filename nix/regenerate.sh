#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate sha256map.nix
# fixme: update for cabal
# nix run .#sha256map-regenerate < stack.yaml > nix/sha256map.nix

# Regenerate plan-to-nix files in ./materialized
nix build .#generateMaterialized
./result nix/materialized/plan-nix/

# Regenerate materialized haskell-build-tools in ./materialized
nix run .#buildToolsGenerateMaterialized

# Regenerate materialized iohk-nix-utils in ./materialized
nix run .#iohkNixGenerateMaterialized

# Regenerate the list of the project packages:
nix eval .#pkgs.cardanoWalletLib.projectPackageList > nix/project-package-list.nix.new
mv nix/project-package-list.nix.new nix/project-package-list.nix
