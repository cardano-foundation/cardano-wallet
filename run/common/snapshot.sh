#! /usr/bin/env -S nix shell 'nixpkgs#curl' 'nixpkgs#lz4' 'nixpkgs#gnutar' --command bash
# shellcheck shell=bash

set -euo pipefail

# shellcheck disable=SC1091
source .env

# Define a local db if NODE_DB is not set
if [[ -z "${NODE_DB-}" ]]; then
    LOCAL_NODE_DB=./databases/node-db
    mkdir -p $LOCAL_NODE_DB
    NODE_DB=$LOCAL_NODE_DB
fi

# Clean the db directory
rm -rf "${NODE_DB:?}"/*

echo "Network: $NETWORK"

case "$NETWORK" in
    preprod)
        SNAPSHOT_NAME=$(curl -s https://downloads.csnapshots.io/testnet/testnet-db-snapshot.json| jq -r .[].file_name )
        echo "Snapshot name: $SNAPSHOT_NAME"
        SNAPSHOT_URL="https://downloads.csnapshots.io/testnet/$SNAPSHOT_NAME"
        ;;
    *)
        echo "Error: Invalid network $NETWORK"
        exit 1
        ;;
esac

echo "Downloading the snapshot..."

curl -SL "$SNAPSHOT_URL" | lz4 -c -d - | tar -x -C "$NODE_DB"

mv -f "$NODE_DB"/db/* "$NODE_DB"/
rm -rf "$NODE_DB"/db

echo "Snapshot downloaded and extracted to $NODE_DB"