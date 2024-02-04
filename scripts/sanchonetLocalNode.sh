#! /usr/bin/env -S nix shell '.#cardano-node' --command bash
# shellcheck shell=bash

set -euo pipefail

NETWORK=sanchonet

WALLET_PORT=${WALLET_PORT:=8090}
DATA_DIR=${DATA_DIR:="ignore-me"}/${NETWORK}
NODE_DB=${DATA_DIR}/node.db
CARDANO_NODE_SOCKET_PATH=${DATA_DIR}/node.socket
CONFIGS=${CONFIGS:="configs/cardano/${NETWORK}"}


cardano-node run \
    --topology ${CONFIGS}/topology.json \
    --database-path ${NODE_DB}\
    --socket-path ${CARDANO_NODE_SOCKET_PATH} \
    --config ${CONFIGS}/config.json
