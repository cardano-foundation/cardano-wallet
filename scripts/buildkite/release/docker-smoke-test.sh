#! /usr/bin/env -S nix shell 'nixpkgs#docker-compose' --command bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p state

export NETWORK=testnet
export WALLET_TAG=$(buildkite-agent meta-data get "release-cabal-version")
export NODE_TAG="8.9.3"
export NODE_DB="$(pwd)/state"
export WALLET_DB="$(pwd)/state"
export WALLET_PORT=8090

docker-compose up -d
mkdir -p logs
docker-compose logs > logs/docker-compose.log
docker-compose down