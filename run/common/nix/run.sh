#! /usr/bin/env nix
#! nix shell .#cardano-wallet .#cardano-node .#cardano-cli  --command bash
# shellcheck shell=bash

# set -euox pipefail
set -euo pipefail

usage() {
    echo "Usage: $0 [sync]"
    echo "  sync [timeout]: Sync the service and wait for it to be ready"
    echo "  start: Start node and wallet services"
}
# Check if no arguments are provided and display usage if true
if [ $# -eq 0 ]; then
    usage
    exit 1
fi

# shellcheck disable=SC1091
source .env

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

cleanup() {
    exit_status=$?
    echo "Cleaning up..."
    kill "${NODE_ID-}" || echo "Failed to kill node"
    kill "${WALLET_ID-}" || echo "Failed to kill wallet"
    sleep 5
    if [[ -n "${CLEANUP_DB-}" ]]; then
        echo "Cleaning up databases..."
        rm -rf "${NODE_DB:?}"/* || echo "Failed to clean node db"
        rm -rf "${WALLET_DB:?}"/* || echo "Failed to clean wallet db"
    fi
    trap - ERR INT EXIT
    exit $exit_status
}

mithril() {
    # shellcheck disable=SC2048
    # shellcheck disable=SC2086
    nix shell --quiet "github:input-output-hk/mithril?ref=2543.1-hotfix" -c $*
}

# Trap the cleanup function on exit
trap cleanup ERR INT EXIT
if [[ -z ${NO_NODE-} ]]; then

    if [[ -n "${USE_MITHRIL-}" ]]; then
        if [ "$NETWORK" != "mainnet" ]; then
            echo "Error: This option is only available for the mainnet network"
            exit 1
        fi
        echo "Starting the mithril service..."
        rm -rf "${NODE_DB:?}"/*
        export AGGREGATOR_ENDPOINT
        export GENESIS_VERIFICATION_KEY
        mithril echo "mithril is available" || exit 44
        digest=$(mithril mithril-client cdb snapshot list --json | jq -r .[0].digest)
        (cd "${NODE_DB}" && mithril mithril-client cdb download "$digest")
        (cd "${NODE_DB}" && mv db/* . && rmdir db)
    fi

    # Start the node with logs redirected to a file if NODE_LOGS_FILE is set
    # shellcheck disable=SC2086
    cardano-node run \
        --topology "${NODE_CONFIGS}"/topology.json \
        --database-path "${NODE_DB}" \
        --socket-path "${NODE_SOCKET_PATH}" \
        --config "${NODE_CONFIGS}"/config.json \
        +RTS -N -A16m -qg -qb -RTS 1>$NODE_LOGS_FILE 2>$NODE_LOGS_FILE &
    NODE_ID=$!
    echo "Node id: $NODE_ID"
    echo "Node socket path: $NODE_SOCKET_PATH"

    sleep 10

else
    echo "Skipping node service..."
fi

if [[ -z ${NO_WALLET-} ]]; then
    echo "Starting the wallet service..."

    # Generate a random port for the wallet service and export it
    RANDOM_PORT=$(( RANDOM % 63001 + 2000 ))
    WALLET_PORT=${WALLET_PORT:=$RANDOM_PORT}

    RANDOM_PORT=$(( RANDOM % 63001 + 2000 ))
    WALLET_UI_PORT=${WALLET_UI_PORT:=$RANDOM_PORT}

    RANDOM_PORT=$(( RANDOM % 63001 + 2000 ))
    # Define the wallet logs file
    LOCAL_WALLET_LOGS_FILE=./wallet.log
    WALLET_LOGS_FILE="${WALLET_LOGS_FILE:=$LOCAL_WALLET_LOGS_FILE}"

    if [[ "${NETWORK}" == "mainnet" ]]; then
        # shellcheck disable=SC2086
        cardano-wallet serve \
            --port "${WALLET_PORT}" \
            --ui-port "${WALLET_UI_PORT}" \
            --database "${WALLET_DB}" \
            --node-socket "${NODE_SOCKET_PATH}" \
            --mainnet \
            --listen-address 0.0.0.0 >$WALLET_LOGS_FILE 2>&1 &
        WALLET_ID=$!
    else
        # shellcheck disable=SC2086
        cardano-wallet serve \
            --log-level DEBUG \
            --trace-application DEBUG \
            --port "${WALLET_PORT}" \
            --ui-port "${WALLET_UI_PORT}" \
            --database "${WALLET_DB}" \
            --node-socket "${NODE_SOCKET_PATH}" \
            --testnet "${NODE_CONFIGS}"/byron-genesis.json \
            --listen-address 0.0.0.0 >$WALLET_LOGS_FILE 2>&1 &
        WALLET_ID=$!
    fi
    echo "Wallet id: $WALLET_ID"
    echo "Wallet service port: $WALLET_PORT"
    echo "Wallet UI port: $WALLET_UI_PORT"

else
    echo "Skipping wallet service..."
fi

# Case statement to handle different command-line arguments
case "$1" in
    sync)

        echo "Syncing the service..."
        sleep 10

        # Initialize timeout and start time for the sync operation
        timeout=${2:=600}
        start_time=$(date +%s)

        # Commands to query service status and node tip time
        command="curl -s localhost:$WALLET_PORT/v2/network/information | jq -r"
        query_status="$command .sync_progress.status"
        query_time="$command .node_tip.time"
        query_progress="$command .sync_progress.progress.quantity"

        SUCCESS_STATUS=${SUCCESS_STATUS:="ready"}
        while true; do
            # Check the sync status
            status=$(cat <(bash -c "$query_status")) || echo "failed"
            if [[ $(date +%s) -ge $((start_time + timeout)) ]]; then
                result="timeout"
                break
            elif [[ "$status" == "$SUCCESS_STATUS" ]]; then
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
    start)
        echo "Ctrl-C to stop"
        sleep infinity
        ;;
    *)
        echo "Error: Invalid option $1"
        usage
        exit 1
        ;;
esac
