#! /bin/bash

set -euox pipefail

attic login adrestia https://attic.cf-app.org/ "$ATTIC_TOKEN"

# shellcheck disable=SC2154
nix build --log-format raw-with-logs ".#devShells.${system}.default.inputDerivation" -o dev-shell

attic push adrestia dev-shell
