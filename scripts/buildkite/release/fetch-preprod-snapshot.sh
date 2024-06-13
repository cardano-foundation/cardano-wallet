#! /usr/bin/env -S nix shell 'nixpkgs#curl' 'nixpkgs#lz4' --command bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p "$NODE_STATE_DIR"

cd "$NODE_STATE_DIR"

export failed=false

curl -s https://downloads.csnapshots.io/testnet/testnet-db-snapshot.json \
    | jq -r .[].file_name > file.new || export failed=true

if [ "$failed" ]; then

    if [ -f file.old ]; then
        if [ "$(cat file.old)" == "$(cat file.new)" ]; then
            echo "Preprod snapshot is up to date with the latest snapshot available."
            exit 0
        else
            echo "New snapshot available, Downloading..."
        fi
    fi

    curl -o - \
        "https://downloads.csnapshots.io/testnet/$(cat file.new)" \
        | lz4 -c -d - | tar -x -C .

    mv file.new file.old

    rm -rf db-new

    mv db db-new
else
    echo "Failed to fetch preprod snapshot. Hope there is an old one available."
fi