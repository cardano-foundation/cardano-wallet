#! /usr/bin/env -S nix shell 'nixpkgs#docker-compose' 'nixpkgs#rsync' --command bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p state/node_db

NETWORK=preprod
export NETWORK

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

rsync -a --delete "$NODE_STATE_DIR/db/" "$TESTS_NODE_DB"

WALLET_TAG=$(buildkite-agent meta-data get "release-cabal-version")
export WALLET_TAG

NODE_TAG="8.9.3"
export NODE_TAG

NODE_DB="$TESTS_NODE_DB"
export NODE_DB

WALLET_DB="$(pwd)/state/wallet_db"
export WALLET_DB

WALLET_PORT=$(shuf -i 2000-65000 -n 1)
export WALLET_PORT

docker-compose up -d
sleep 20

curl localhost:$WALLET_PORT/v2/network/information

mkdir -p logs
docker-compose logs > logs/docker-compose.log
docker-compose down
