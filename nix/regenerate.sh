#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate --sha256 hashes in cabal.project
nix build .#checkCabalProject -o check-cabal-project.sh && ./check-cabal-project.sh

# Regenerate materialized haskell-build-tools in ./materialized
nix build .#buildToolsGenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate materialized iohk-nix-utils in ./materialized
nix build .#iohkNixGenerateMaterialized
./result/bin/regenerate-materialized-nix

# Regenerate the list of the project packages:
nix eval .#pkgs.cardanoWalletLib.projectPackageList > nix/project-package-list.nix.new
mv nix/project-package-list.nix.new nix/project-package-list.nix
