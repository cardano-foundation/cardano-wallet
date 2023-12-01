#!/usr/bin/env bash

NETWORK=preprod \
TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/ \
WALLET=dev-master \
NODE=8.1.1 \
NODE_CONFIG_PATH=`pwd`/state/configs/$NETWORK \
DATA=`pwd`/state/node_db/$NETWORK \
WALLET_DATA=`pwd`/state/wallet_db/$NETWORK \
docker-compose -f docker-compose-test.yml $1
