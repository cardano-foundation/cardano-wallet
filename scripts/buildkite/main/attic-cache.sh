#! /usr/bin/env  bash

set -euox pipefail

nix shell --log-format raw-with-logs 'github:zhaofengli/attic' --command \
    attic login adrestia https://attic.cf-app.org/ "$ATTIC_TOKEN"

# shellcheck disable=SC2154
nix build --log-format raw-with-logs ".#devShells.${system}.default.inputDerivation" -o dev-shell

nix shell 'github:zhaofengli/attic' --command \
    attic push adrestia dev-shell
