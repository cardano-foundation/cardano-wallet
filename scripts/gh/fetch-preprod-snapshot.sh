#!/usr/bin/env bash

set -euo pipefail

mkdir -p state
cd state
mkdir -p node_db
cd node_db
mkdir -p preprod
cd preprod
curl -s https://downloads.csnapshots.io/testnet/testnet-db-snapshot.json \
    | jq -r .[].file_name > snapshot.json
curl -o - \
    "https://downloads.csnapshots.io/testnet/$(cat snapshot.json)" \
    | lz4 -c -d - | tar -x -C .
mv db/* .
