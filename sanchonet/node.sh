#! /usr/bin/env -S nix shell github:IntersectMBO/cardano-node?ref=8.11.0-sancho#cardano-node --command bash
# shellcheck shell=bash

set -euo pipefail

NETWORK=sanchonet

WALLET_PORT=${WALLET_PORT:=8090}
DATA_DIR=${DATA_DIR:="ignore-me"}/${NETWORK}
NODE_DB=${DATA_DIR}/node.db
CARDANO_NODE_SOCKET_PATH=${DATA_DIR}/node.socket
CONFIGS=${CONFIGS:="../configs/cardano/${NETWORK}"}

trap ctrl_c INT

cardano-node run \
    --topology ${CONFIGS}/topology.json \
    --database-path ${NODE_DB}\
    --socket-path ${CARDANO_NODE_SOCKET_PATH} \
    --config ${CONFIGS}/config.json \
    +RTS -N -A16m -qg -qb -RTS &

NODE_ID=$!

function ctrl_c() {
    kill "${NODE_ID}"
    echo "DATA_DIR: ${DATA_DIR}"
    echo "NODE_DB: ${NODE_DB}"
    echo "CARDANO_NODE_SOCKET_PATH: ${CARDANO_NODE_SOCKET_PATH}"
    echo "CONFIGS: ${CONFIGS}"
}

while :; do sleep 2073600; done
