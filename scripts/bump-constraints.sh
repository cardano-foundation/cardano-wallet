#!/usr/bin/env bash

set -euo pipefail

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
    local major_version
    major_version=$(echo "$version" | cut -d. -f1)
    local minor_version
    minor_version=$(echo "$version" | cut -d. -f2)
    local next_minor_version=$((minor_version + 1))
    echo ">= $version \&\& < $major_version.$next_minor_version"
}

# Single package update mode:
if [ "${1:-}" = "set-package-version" ]; then
    if [ "$#" -ne 3 ]; then
        echo "Usage: $0 set-package-version <package> <version>"
        exit 1
    fi
    package="$2"
    version="$3"
    constraint=$(generate_constraint "$version")
    echo "Updating $package to constraint $constraint"
    update_cabal_files "$package" "$constraint"
    find lib -name '*.cabal' -exec cabal-fmt -i {} \;
    exit 0
fi

# Freeze file mode: update all packages using the freeze file.
freeze_file="$1"

updates=(
    "cardano-addresses-cli"
    "cardano-addresses"
    "cardano-api"
    "cardano-binary"
    "cardano-cli"
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
    "data-default"
    "io-classes"
    "iohk-monitoring"
    "katip"
    "lobemo-backend-ekg"
    "network-mux"
    "ouroboros-consensus-cardano"
    "ouroboros-consensus-diffusion"
    "ouroboros-consensus-protocol"
    "ouroboros-consensus"
    "ouroboros-network-api"
    "ouroboros-network-framework"
    "ouroboros-network-protocols"
    "ouroboros-network"
    "typed-protocols"
)

# for every update, get the version from the freeze file and update the cabal files
for update in "${updates[@]}"; do
    version=$(sed -n "s/.*any\.$update ==\([0-9.]*\),.*/\1/p" "$freeze_file")
    constraint=$(generate_constraint "$version")
    if [ -z "$version" ]; then
        echo "Version for $update not found in $freeze_file"
    else
        echo "$update $constraint"
        update_cabal_files "$update" "$constraint"
    fi
done

find lib -name '*.cabal' -exec cabal-fmt -i {} \;
