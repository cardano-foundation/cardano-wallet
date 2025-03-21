#! /usr/bin/env bash
# set -euox pipefail
set -euo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# shellcheck disable=SC1091
source ${SCRIPT_DIR}/.env

mkdir -p ./databases

# Define a local db if WALLET_DB is not set
if [[ -z "${WALLET_DB-}" ]]; then
    LOCAL_WALLET_DB=./databases/wallet-db
    mkdir -p $LOCAL_WALLET_DB
    WALLET_DB=$LOCAL_WALLET_DB
    export WALLET_DB
fi

if [[ -n "${CLEANUP_DB-}" ]]; then
    rm -rf "${WALLET_DB:?}"/*
fi

NETWORK=${NETWORK:=testnet}

# Define and export the node socket name
NODE_SOCKET_NAME=node.socket

# Define and export the local and actual directory for the node socket
LOCAL_NODE_SOCKET_DIR=./databases
NODE_SOCKET_DIR=${NODE_SOCKET_DIR:=$LOCAL_NODE_SOCKET_DIR}

NODE_SOCKET_PATH=${NODE_SOCKET_DIR}/${NODE_SOCKET_NAME}

# Define and export the local and actual configs directory for the node
LOCAL_NODE_CONFIGS=${SCRIPT_DIR}/configs
NODE_CONFIGS=${NODE_CONFIGS:=$LOCAL_NODE_CONFIGS}


# Generate a random port for the wallet service and export it
RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_PORT=${WALLET_PORT:=$RANDOM_PORT}

RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_UI_PORT=${WALLET_UI_PORT:=$RANDOM_PORT}

# Define the wallet logs file
LOCAL_WALLET_LOGS_FILE=./wallet.log
WALLET_LOGS_FILE="${WALLET_LOGS_FILE:=$LOCAL_WALLET_LOGS_FILE}"

echo "Wallet service port: $WALLET_PORT"
echo "Wallet UI port: $WALLET_UI_PORT"

if [[ $NETWORK == "mainnet" ]]; then
    echo "Running wallet in mainnet mode"
    NETWORK_OPTION="--mainnet"
else
    echo "Running wallet in testnet mode"
    NETWORK_OPTION="--testnet ${NODE_CONFIGS}/byron-genesis.json"

fi

# shellcheck disable=SC2086
cabal run --project-dir ${SCRIPT_DIR}/../../.. -O0 cardano-wallet-application:exe:cardano-wallet -- \
    serve \
    --log-level DEBUG \
    --trace-application DEBUG \
    --port "${WALLET_PORT}" \
    --ui-port "${WALLET_UI_PORT}" \
    --database "${WALLET_DB}" \
    --node-socket "${NODE_SOCKET_PATH}" \
    $NETWORK_OPTION \
    --listen-address 0.0.0.0 \
    +RTS -N -A16m -qg -qb -RTS
