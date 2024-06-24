#! /usr/bin/env -S nix shell 'nixpkgs#docker-compose' 'nixpkgs#rsync' 'nixpkgs#jq' --command bash
# shellcheck shell=bash

set -euox pipefail


NETWORK=preprod
export NETWORK

TESTS_NODE_DB="$(pwd)/state/node_db"
mkdir -p "$TESTS_NODE_DB"
export TESTS_NODE_DB

rsync -a --delete "$NODE_STATE_DIR/db/" "$TESTS_NODE_DB"

WALLET_TAG=$(buildkite-agent meta-data get "release-cabal-version")
export WALLET_TAG

NODE_TAG="8.9.4"
export NODE_TAG

NODE_DB="$TESTS_NODE_DB"
export NODE_DB

WALLET_DB="$(pwd)/state/wallet_db"
mkdir -p "$WALLET_DB"
export WALLET_DB

WALLET_PORT=$(shuf -i 2000-65000 -n 1)
export WALLET_PORT

USER_ID=$(id -u)
export USER_ID

docker-compose up -d

n=0
while :
do
    result=$(curl --connect-timeout 1 localhost:"$WALLET_PORT"/v2/network/information || echo "wait")
    echo "$result"
    if [ "$result" != "wait" ];
        then
            echo "$result" | jq
            break
        else
            sleep 1
            n=$((n+1))
    fi
    if [ "$n" -ge 20 ]
        then break
    fi
done

mkdir -p logs
docker-compose logs > logs/docker-compose.log
docker-compose down
