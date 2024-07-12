#! /usr/bin/env -S nix shell '.#cardano-wallet' '.#cardano-node' --command bash
# shellcheck shell=bash

set -euo pipefail

usage() {
    echo "Usage: $0 [sync]"
    echo "  sync: Sync the service and wait for it to be ready"
}
# Check if no arguments are provided and display usage if true
if [ $# -eq 0 ]; then
    usage
    exit 1
fi

# Generate a random port for the wallet service and export it
RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_PORT=${WALLET_PORT:=$RANDOM_PORT}

# Define a local db if WALLET_DB is not set
if [[ -z "${WALLET_DB-}" ]]; then
    LOCAL_WALLET_DB=./databases/wallet-db
    mkdir -p $LOCAL_WALLET_DB
    WALLET_DB=$LOCAL_WALLET_DB
    export WALLET_DB
fi

# Define a local db if NODE_DB is not set
if [[ -z "${NODE_DB-}" ]]; then
    LOCAL_NODE_DB=./databases/node-db
    mkdir -p $LOCAL_NODE_DB
    NODE_DB=$LOCAL_NODE_DB
    export NODE_DB
fi

# Define and export the node socket name
NODE_SOCKET_NAME=node.socket

# Define and export the local and actual directory for the node socket
LOCAL_NODE_SOCKET_DIR=./.
NODE_SOCKET_DIR=${NODE_SOCKET_DIR:=$LOCAL_NODE_SOCKET_DIR}

NODE_SOCKET_PATH=${NODE_SOCKET_DIR}/${NODE_SOCKET_NAME}

# Define and export the local and actual configs directory for the node
LOCAL_NODE_CONFIGS=./configs
NODE_CONFIGS=${NODE_CONFIGS:=$LOCAL_NODE_CONFIGS}

# Start the node with logs redirected to a file if NODE_LOGS_FILE is set
node() {
    cardano-node run \
        --topology "${NODE_CONFIGS}"/topology.json \
        --database-path "${NODE_DB}"\
        --socket-path "${NODE_SOCKET_PATH}" \
        --config "${NODE_CONFIGS}"/config.json \
        +RTS -N -A16m -qg -qb -RTS
    }

if [[ -z "${NODE_LOGS_FILE-}" ]]; then
    node &
else
    node >"${NODE_LOGS_FILE}" 2>&1 &
fi
NODE_ID=$!

# Start the wallet with logs redirected to a file if WALLET_LOGS_FILE is set
wallet() {
    cardano-wallet serve \
        --port "${WALLET_PORT}" \
        --database "${WALLET_DB}" \
        --node-socket "${NODE_SOCKET_PATH}" \
        --testnet "${NODE_CONFIGS}"/byron-genesis.json \
        --listen-address 0.0.0.0
}

if [[ -z "${WALLET_LOGS_FILE-}" ]]; then
    wallet &
else
    wallet >"${WALLET_LOGS_FILE}" 2>&1 &
fi
WALLET_ID=$!

cleanup() {
    echo "Cleaning up..."
    kill "${NODE_ID}" || true
    kill "${WALLET_ID}" || true
}

# Trap the cleanup function on exit
trap cleanup ERR INT EXIT



# Case statement to handle different command-line arguments
case "$1" in
    sync)
        echo "Wallet service port: $WALLET_PORT"
        echo "Syncing the service..."

        # Initialize timeout and start time for the sync operation
        timeout=10000
        start_time=$(date +%s)

        # Commands to query service status and node tip time
        command="curl -s localhost:$WALLET_PORT/v2/network/information | jq -r"
        query_status="$command .sync_progress.status"
        query_time="$command .node_tip.time"
        query_progress="$command .sync_progress.progress.quantity"
        query_all="$command ."

        # Execute and display the full query result
        bash -c "$query_all"
        while true; do
            # Check the sync status
            status=$(cat <(bash -c "$query_status")) || echo "failed"
            if [[ $(date +%s) -ge $((start_time + timeout)) ]]; then
                result="timeout"
                break
            elif [[ "$status" == "ready" ]]; then
                result="success"
                printf "\n"
                break
            else
                # Display the node tip time as progress
                time=$(cat <(bash -c "$query_time"))
                progress=$(cat <(bash -c "$query_progress"))
                printf "%s%% %s\r" "$progress" "$time"
                sleep 1
            fi
        done

        # Stop the service after syncing
        echo "Result: $result"
        # Exit with 0 on success, 1 on failure or timeout
        if [[ "$result" == "success" ]]; then
            exit 0
        else
            exit 1
        fi
        ;;
    *)
        echo "Error: Invalid option $1"
        usage
        exit 1
        ;;
esac
