#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate sha256map.nix
# FIXME: Can we remove this and stack.yaml?
# nix run .#sha256map-regenerate < stack.yaml > nix/sha256map.nix

# Regenerate materialized haskell-build-tools in ./materialized
nix build .#buildToolsGenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate materialized iohk-nix-utils in ./materialized
nix build .#iohkNixGenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate the list of the project packages:
nix eval .#pkgs.cardanoWalletLib.projectPackageList > nix/project-package-list.nix.new
mv nix/project-package-list.nix.new nix/project-package-list.nix
