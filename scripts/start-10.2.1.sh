#!/usr/bin/env bash

set -euo pipefail

current_dir=$(pwd)
tmp=$(mktemp -d)

cleanup() {
    echo "Cleaning up..."
    cd "$current_dir" || exit
    rm -rf "$tmp"
}

trap cleanup EXIT
cd "$tmp" || exit

base_url="https://book.play.dev.cardano.org/environments-pre/preprod"

curl "$base_url/config.json" >config.json
curl "$base_url/conway-genesis.json" >conway-genesis.json
curl "$base_url/topology.json" >topology.json
curl "$base_url/byron-genesis.json" >byron-genesis.json
curl "$base_url/shelley-genesis.json" >shelley-genesis.json
curl "$base_url/alonzo-genesis.json" >alonzo-genesis.json

cat config.json

# cp "$current_dir"/configs/cardano/pre-preprod/config.json .
# cp "$current_dir"/lib/local-cluster/test/data/cluster-configs/template/config.json .

# diff "$current_dir"/configs/cardano/pre-preprod/config.json config.json


mkdir -p node-db
nix shell "github:IntersectMBO/cardano-node?ref=10.2.1" -c \
    cardano-node version
nix shell "github:IntersectMBO/cardano-node?ref=10.2.1" -c \
    cardano-node run \
    --config config.json \
    --topology topology.json \
    --database-path node-db \
    --socket-path node-socket
