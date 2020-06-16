#!/usr/bin/env bash

# Run this script if you would like to know which version of
# cardano-node will be in the nix-shell (i.e. CI).
#
# Instructions for updating the version are here:
# https://github.com/input-output-hk/cardano-wallet/wiki/Updating-Dependencies

set -euo pipefail

echo "Reading cardano-node branch/tag and revision from nix/sources.json"
jq -r '.["cardano-node"] | .branch, .rev' < `dirname $0`/../nix/sources.json

echo
echo "Your cardano-node is `type -p cardano-node`"
cardano-node version

echo
echo "Your cardano-cli is `type -p cardano-cli`"
cardano-cli version
