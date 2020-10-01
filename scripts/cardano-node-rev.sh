#!/usr/bin/env bash

# Run this script if you would like to know which version of
# cardano-node will be in the nix-shell (i.e. CI).
#
# Instructions for updating the version are here:
# https://github.com/input-output-hk/cardano-wallet/wiki/Updating-Dependencies

set -euo pipefail

echo "Reading cardano-node branch/tag and revision from nix/sources.json"
jq -r '.["cardano-node"] | .branch, .rev' < `dirname $0`/../nix/sources.json

for prog in cardano-node cardano-cli cardano-address; do
  echo
  echo "Your $prog is `type -p $prog`"
  $prog --version
done
