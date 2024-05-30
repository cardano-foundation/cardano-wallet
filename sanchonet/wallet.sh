#! /usr/bin/env -S nix shell github:cardano-foundation/cardano-wallet?ref=sanchonet#cardano-wallet --command bash
# shellcheck shell=bash

set -euo pipefail

NETWORK=sanchonet

WALLET_PORT=${WALLET_PORT:=8090}
DATA_DIR=${DATA_DIR:="ignore-me"}/${NETWORK}
WALLET_DB=${DATA_DIR}/cardano-wallet.db
CARDANO_NODE_SOCKET_PATH=${DATA_DIR}/node.socket
CONFIGS=${CONFIGS:="../configs/cardano/${NETWORK}"}

trap ctrl_c INT

cardano-wallet serve \
    --port ${WALLET_PORT} \
    --database ${WALLET_DB} \
    --node-socket ${CARDANO_NODE_SOCKET_PATH} \
    --testnet ${CONFIGS}/byron-genesis.json \
    --listen-address 0.0.0.0 \
    +RTS -N -A16m -qg -qb -RTS &

WALLET_ID=$!

function ctrl_c() {
    kill "${WALLET_ID}"
    echo "DATA_DIR: ${DATA_DIR}"
    echo "WALLET_DB: ${WALLET_DB}"
    echo "CARDANO_NODE_SOCKET_PATH: ${CARDANO_NODE_SOCKET_PATH}"
    echo "WALLET_PORT: ${WALLET_PORT}"
}

while :; do sleep 2073600; done
