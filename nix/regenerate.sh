#!/usr/bin/env bash

set -euo pipefail

cd $(dirname "$0")/..

# Regenerate the list of the project packages:
nix eval .#pkgs.cardanoWalletLib.projectPackageList > nix/project-package-list.nix.new
mv nix/project-package-list.nix.new nix/project-package-list.nix
