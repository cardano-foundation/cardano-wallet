#! /usr/bin/env bash

set -euo pipefail

export WALLET_TAG=rc-latest
export NODE_TAG=9.0.0
export WALLET_PORT=8090
export NETWORK=sanchonet
export WALLET_DB=/tmp/cardano-wallet-sanchonet.db
export NODE_DB=/tmp/cardano-node-sanchonet.db

docker-compose pull

docker-compose up
