#!/usr/bin/env bash

# Enforce strict script execution modes
set -euox pipefail

# Function to display usage information
usage() {
    echo "Usage: $0 [sync|start|stop|logs|help]"
    echo "  sync [timeout]: Sync the service and wait for it to be ready"
    echo "  start: Start the service"
    echo "  start-with-mithril: Start the service with mithril boostrap"
    echo "  stop: Stop the service"
    echo "  logs: Show the service logs"
    echo "  help: Show this help message"
}

# Check if no arguments are provided and display usage if true
if [ $# -eq 0 ]; then
    usage
    exit 1
fi

# shellcheck disable=SC1091
source .env

# Define and export wallet and node version tags
RELEASE_WALLET_TAG=2025.3.31

WALLET_TAG=${WALLET_TAG:=$RELEASE_WALLET_TAG}
export WALLET_TAG

# Generate a random port for the wallet service and export it
RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_PORT=${WALLET_PORT:=$RANDOM_PORT}
RANDOM_PORT=$(shuf -i 2000-65000 -n 1)
WALLET_UI_PORT=${WALLET_UI_PORT:=$RANDOM_PORT}

export WALLET_UI_PORT

export WALLET_PORT

# Define a local db if WALLET_DB is not set
if [[ -z "${WALLET_DB-}" ]]; then
    LOCAL_WALLET_DB=./databases/wallet-db
    mkdir -p $LOCAL_WALLET_DB
    WALLET_DB=$LOCAL_WALLET_DB
    export WALLET_DB
fi
rm -rf "${WALLET_DB:?}/*"

# Define a local db if NODE_DB is not set
if [[ -z "${NODE_DB-}" ]]; then
    LOCAL_NODE_DB=./databases/node-db
    NODE_DB=$LOCAL_NODE_DB
    export NODE_DB
fi
mkdir -p "${NODE_DB:?}"

# Get the current user's ID and export it
USER_ID=$(id -u)
export USER_ID

# Get the current user's group ID and export it
GROUP_ID=$(id -g)
export GROUP_ID

# Define and export the node socket name
NODE_SOCKET_NAME=node.socket
export NODE_SOCKET_NAME

# Define and export the local and actual directory for the node socket
LOCAL_NODE_SOCKET_DIR=./.
NODE_SOCKET_DIR=${NODE_SOCKET_DIR:=$LOCAL_NODE_SOCKET_DIR}
export NODE_SOCKET_DIR

# Define and export the local and actual configs directory for the node
LOCAL_NODE_CONFIGS=./configs
NODE_CONFIGS=${NODE_CONFIGS:=$LOCAL_NODE_CONFIGS}
export NODE_CONFIGS

COMPOSE_PROJECT_NAME=${COMPOSE_PROJECT_NAME:=cardano}
startup() {
    # Pull the latest images
    if [ -z "${USE_LOCAL_IMAGE-}" ]; then
        docker compose pull -q
    fi
    # Start the service in detached mode
    docker compose -p "$COMPOSE_PROJECT_NAME" up -d
}

# Function to clean up the service
cleanup() {
    echo "Cleaning up..."
    docker compose -p "$COMPOSE_PROJECT_NAME" down 2>/dev/null
    sleep  3
    docker compose -p "$COMPOSE_PROJECT_NAME" kill 2>/dev/null
}

node-db-with-mithril() {
    random_name=$(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 8 ; echo '')
    echo "Starting the mithril service..."
    digest=$(docker compose -p "$random_name" --profile  mithril run --rm mithril cdb  snapshot list --json | jq -r .[0].digest)
    rm -rf "${NODE_DB:?}"/*
    docker compose -p "$random_name" --profile mithril run --rm mithril cdb download "$digest"
}

# Case statement to handle different command-line arguments
case "$1" in
    sync)
        if [[ -n "${USE_MITHRIL-}" ]]; then
            node-db-with-mithril
        fi
        echo "Wallet service port: $WALLET_PORT"
        echo "Wallet service tag: $WALLET_TAG"
        echo "Syncing the service..."
        startup

        # Initialize timeout and start time for the sync operation
        timeout=${2:-600}
        start_time=$(date +%s)

        # Commands to query service status and node tip time
        command=$(printf "docker run --rm --network %s_default alpine/curl curl -s --max-time 5 http://cardano-wallet:8090/v2/network/information | jq -r" "$COMPOSE_PROJECT_NAME")
        query_status="$command  .sync_progress.status"
        query_time="$command .node_tip.time"
        query_progress="$command .sync_progress.progress.quantity"

        # Execute and display the full query result
        trap cleanup ERR INT

        # Define the wanted status and result, can be "syncing" or "ready"
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
        cleanup

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
        echo "Starting the service..."
        echo "Wallet service port: $WALLET_PORT"
        echo "Wallet service ui port: $WALLET_UI_PORT"
        echo "Wallet service tag: $WALLET_TAG"
        startup
        ;;
    stop)
        echo "Stopping the service..."
        cleanup
        ;;
    logs)
        echo "Showing logs..."
        docker compose -p "$COMPOSE_PROJECT_NAME" logs -f  # Follow the service logs
        ;;
    node-db-with-mithril)
        node-db-with-mithril
        ;;
    help)
        usage  # Display usage information
        ;;
    *)
        echo "Error: Invalid option '$1'"
        usage  # Display usage information and exit with error
        exit 1
        ;;
esac
