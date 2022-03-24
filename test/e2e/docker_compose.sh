#!/usr/bin/env bash

NETWORK=testnet \
TESTS_E2E_TOKEN_METADATA=https://metadata.cardano-testnet.iohkdev.io/ \
WALLET=dev-master \
NODE=1.34.1 \
NODE_CONFIG_PATH=`pwd`/state/configs/$NETWORK \
DATA=`pwd`/state/node_db/$NETWORK \
WALLET_DATA=`pwd`/state/wallet_db/$NETWORK \
docker-compose -f docker-compose-test.yml $1
