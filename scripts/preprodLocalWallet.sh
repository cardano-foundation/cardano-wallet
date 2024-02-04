#! /usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

NETWORK=preprod

WALLET_PORT=${WALLET_PORT:=8090}
DATA_DIR=${DATA_DIR:="/node/preprod"}/${NETWORK}
WALLET_DB=${DATA_DIR}/cardano-wallet.db
CARDANO_NODE_SOCKET_PATH=${DATA_DIR}/node.socket
CONFIGS=${CONFIGS:="configs/cardano/${NETWORK}"}

trap ctrl_c INT

cabal run -O0 cardano-wallet-api:exe:cardano-wallet -- serve \
    --port ${WALLET_PORT} \
    --database ${WALLET_DB} \
    --node-socket ${CARDANO_NODE_SOCKET_PATH} \
    --testnet ${CONFIGS}/byron-genesis.json \
    --listen-address 0.0.0.0 \
    --trace-wallet-engine DEBUG \
    --trace-ntp-client DEBUG
