#! /usr/bin/env -S nix shell '.#cardano-wallet' '.#cardano-node' --command bash
# shellcheck shell=bash

set -euo pipefail

NETWORK=sanchonet

WALLET_PORT=${WALLET_PORT:=8090}
DATA_DIR=${DATA_DIR:="ignore-me"}/${NETWORK}
WALLET_DB=${DATA_DIR}/cardano-wallet.db
NODE_DB=${DATA_DIR}/node.db
CARDANO_NODE_SOCKET_PATH=${DATA_DIR}/node.socket
CONFIGS=${CONFIGS:="configs/cardano/${NETWORK}"}

trap ctrl_c INT

cardano-node run \
    --topology ${CONFIGS}/topology.json \
    --database-path ${NODE_DB}\
    --socket-path ${CARDANO_NODE_SOCKET_PATH} \
    --config ${CONFIGS}/config.json &

NODE_ID=$!

cardano-wallet serve \
    --port ${WALLET_PORT} \
    --database ${WALLET_DB} \
    --node-socket ${CARDANO_NODE_SOCKET_PATH} \
    --testnet ${CONFIGS}/byron-genesis.json \
    --listen-address 0.0.0.0 &

WALLET_ID=$!

function ctrl_c() {
    kill "${NODE_ID}"
    kill "${WALLET_ID}"
    kill "${APP_ID}"
    echo "DATA_DIR: ${DATA_DIR}"
    echo "WALLET_DB: ${WALLET_DB}"
    echo "NODE_DB: ${NODE_DB}"
    echo "CARDANO_NODE_SOCKET_PATH: ${CARDANO_NODE_SOCKET_PATH}"
    echo "CONFIGS: ${CONFIGS}"
    echo "WALLET_PORT: ${WALLET_PORT}"
}

while :; do sleep 2073600; done
