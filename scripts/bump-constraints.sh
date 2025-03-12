#!/usr/bin/env bash

set -euo pipefail

updates=(
    "cardano-addresses-cli"
    "cardano-addresses"
    "cardano-api"
    "cardano-binary"
    "cardano-crypto-class"
    "cardano-crypto-test"
    "cardano-crypto"
    "cardano-data"
    "cardano-ledger-allegra"
    "cardano-ledger-alonzo-test"
    "cardano-ledger-alonzo"
    "cardano-ledger-api"
    "cardano-ledger-babbage-test"
    "cardano-ledger-babbage"
    "cardano-ledger-byron-test"
    "cardano-ledger-byron"
    "cardano-ledger-conway"
    "cardano-ledger-core"
    "cardano-ledger-mary"
    "cardano-ledger-shelley"
    "cardano-slotting"
    "cardano-strict-containers"
    "iohk-monitoring"
    "lobemo-backend-ekg"
    "ouroboros-consensus-cardano"
    "ouroboros-consensus-diffusion"
    "ouroboros-consensus-protocol"
    "ouroboros-consensus"
    "ouroboros-network-api"
    "ouroboros-network-framework"
    "ouroboros-network-protocols"
    "ouroboros-network"
)

freeze_file="$1"

# update all cabal files with one bump
update_cabal_files() {
    local update=$1
    local constraint=$2
    local internal_name="(:[a-zA-Z0-9-]*|:\{[^}]*\})?"
    local match="s/$update$internal_name( .*)?$/$update\1 $constraint/"
    find lib -name '*.cabal' -exec sed -i -E "$match" {} \;
}

# generate version constraint
generate_constraint() {
    local version=$1
    # shellcheck disable=SC2155
    local major_version=$(echo "$version" | cut -d. -f1)
    # shellcheck disable=SC2155
    local minor_version=$(echo "$version" | cut -d. -f2)
    local next_minor_version=$((minor_version + 1))
    echo ">= $version \&\& < $major_version.$next_minor_version"
}

# for every update, get the version from the freeze file and update the cabal files
for update in "${updates[@]}"; do
    version=$(sed -n "s/.*any\.$update ==\([0-9.]*\),.*/\1/p" "$freeze_file")
    constraint=$(generate_constraint "$version")
    echo "$update $constraint"
    update_cabal_files "$update" "$constraint"
done

find lib -name '*.cabal' -exec cabal-fmt -i {} \;
