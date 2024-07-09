#! /bin/bash

set -euo pipefail

# shellcheck disable=SC1091
source .env

# set a directory for the node-db
mkdir -p "$NODE_DB"
# set a directory for the wallet-db
mkdir -p "$WALLET_DB"

# set a node socket dir path
mkdir -p "$NODE_SOCKET_DIR"

# set your user id
USER_ID=$(id -u)
export USER_ID

if [ "$(ls -A "${NODE_DB}")" ]
then
    echo "Node state is present, not downloading the snapshot."
else
        curl -o - \
            https://downloads.csnapshots.io/testnet/"$(curl -s https://downloads.csnapshots.io/testnet/testnet-db-snapshot.json| jq -r .[].file_name )" \
                | lz4 -c -d - \
                | tar -x -C "$NODE_DB"
        mv "$NODE_DB"/db/* "$NODE_DB"/
        rm -rf "$NODE_DB"/db

fi

# start the services
docker compose up