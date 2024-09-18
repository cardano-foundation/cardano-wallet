#! /usr/bin/env -S nix shell '.#cardano-node' --command bash
# shellcheck shell=bash

# set -euox pipefail
set -euo pipefail

# shellcheck disable=SC1091
source .env

mkdir -p ./databases

# Define a local db if NODE_DB is not set
if [[ -z "${NODE_DB-}" ]]; then
    LOCAL_NODE_DB=./databases/node-db
    mkdir -p $LOCAL_NODE_DB
    NODE_DB=$LOCAL_NODE_DB
    export NODE_DB
fi

NETWORK=${NETWORK:=testnet}

# Define and export the node socket name
NODE_SOCKET_NAME=node.socket

# Define and export the local and actual directory for the node socket
LOCAL_NODE_SOCKET_DIR=./databases
NODE_SOCKET_DIR=${NODE_SOCKET_DIR:=$LOCAL_NODE_SOCKET_DIR}

NODE_SOCKET_PATH=${NODE_SOCKET_DIR}/${NODE_SOCKET_NAME}

# Define and export the local and actual configs directory for the node
LOCAL_NODE_CONFIGS=./configs
NODE_CONFIGS=${NODE_CONFIGS:=$LOCAL_NODE_CONFIGS}


# Define the node logs file
LOCAL_NODE_LOGS_FILE=./node.log
NODE_LOGS_FILE="${NODE_LOGS_FILE:=$LOCAL_NODE_LOGS_FILE}"

echo "Node socket path: $NODE_SOCKET_PATH"

# Start the node with logs redirected to a file if NODE_LOGS_FILE is set
# shellcheck disable=SC2086
cardano-node run \
    --topology "${NODE_CONFIGS}"/topology.json \
    --database-path "${NODE_DB}"\
    --socket-path "${NODE_SOCKET_PATH}" \
    --config "${NODE_CONFIGS}"/config.json \
    +RTS -N -A16m -qg -qb -RTS
