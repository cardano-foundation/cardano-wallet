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

NODE_TAG="8.9.3"
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

NODE_SOCKET_DIR=$(mktemp -d /tmp/node-preprod.XXXXXX)
export NODE_SOCKET_DIR

NODE_SOCKET_NAME="node.socket"
export NODE_SOCKET_NAME

COMPOSE_PROJECT_NAME="e2e-tests-$WALLET_PORT"
export COMPOSE_PROJECT_NAME

dc="docker compose -f docker-compose-preprod.yml"

${dc} down || true
${dc} up -d

mkdir -p "$(pwd)/logs"

TESTS_LOGDIR="$(pwd)/logs"
export TESTS_LOGDIR

CARDANO_NODE_CONFIGS="$(pwd)/configs/cardano"
export CARDANO_NODE_CONFIGS

VERSION=$(buildkite-agent meta-data get "release-version")

TESTS_E2E_BINDIR="$(pwd)/cardano-wallet-$VERSION-linux64"
export TESTS_E2E_BINDIR

cd test/e2e

CARDANO_NODE_SOCKET_PATH="$NODE_SOCKET_DIR/$NODE_SOCKET_NAME"
export CARDANO_NODE_SOCKET_PATH

TESTS_E2E_STATEDIR=$(pwd)/state
export TESTS_E2E_STATEDIR

TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/
export TESTS_E2E_TOKEN_METADATA

TESTS_E2E_FIXTURES="$FIXTURE_DECRYPTION_KEY"

export TESTS_E2E_FIXTURES

nix develop -c rake wait_until_node_synced
nix develop -c rake secrets_decode
nix develop -c rake spec

${dc} down

rm -rf "$NODE_SOCKET_DIR"
