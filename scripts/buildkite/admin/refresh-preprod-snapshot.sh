#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p "$NODE_STATE_DIR"

cd "$NODE_STATE_DIR"

curl -s https://downloads.csnapshots.io/testnet/testnet-db-snapshot.json \
    | jq -r .[].file_name > filename_new


if [ -f filename ]; then
    if [ "$(cat filename)" == "$(cat filename_new)" ]; then
        echo "Preprod snapshot is up to date with the latest snapshot available."
        exit 0
    fi
fi

dir_new="$(sed -e 's/\.tar\.lz4//' filename_new)"

rm -rf "$dir_new" && mkdir -p "$dir_new"

cd "$dir_new"

curl -o snapshot.tar.lz4 \
    "https://downloads.csnapshots.io/testnet/$(cat ../filename_new)"
lz4 -c -d snapshot.tar.lz4 > snapshot.tar
tar -x -f snapshot.tar

cd ..

if [ -f filename ]; then
    dir_old="$(sed -e 's/\.tar\.lz4//' filename)"
    rm -rf "$dir_old"
fi

# Set the `db` directory to a symlink in a way that is supposed to be atomic.
#     See also: https://unix.stackexchange.com/a/6786
ln -s "$dir_new"/db db-new && mv -T db-new db

mv filename_new filename
