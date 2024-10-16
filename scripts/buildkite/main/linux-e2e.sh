#! /usr/bin/env -S nix shell 'nixpkgs#rsync' 'nixpkgs#gnutar' 'nixpkgs#gnupg' --command bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p "$(pwd)/logs"

TESTS_LOGDIR="$(pwd)/logs"
export TESTS_LOGDIR

CARDANO_NODE_CONFIGS="$(pwd)/configs/cardano"
export CARDANO_NODE_CONFIGS

CURRENT_VERSION=v2024-10-16

VERSION=$(buildkite-agent meta-data get "release-version" --default "$CURRENT_VERSION")

echo "VERSION=$VERSION"

buildkite-agent artifact \
    download "result/linux/cardano-wallet-$VERSION-linux64.tar.gz" "."

tar xvzf "result/linux/cardano-wallet-$VERSION-linux64.tar.gz"

TESTS_E2E_BINDIR="$(pwd)/cardano-wallet-$VERSION-linux64"
export TESTS_E2E_BINDIR

cd test/e2e

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"/preprod
rsync -a --delete "$NODE_STATE_DIR/db/" "$TESTS_NODE_DB/preprod"

tmpfile=$(mktemp /tmp/node-preprod.XXXXXX)

CARDANO_NODE_SOCKET_PATH="$tmpfile"
export CARDANO_NODE_SOCKET_PATH

TESTS_E2E_STATEDIR=$(pwd)/state
export TESTS_E2E_STATEDIR

TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/
export TESTS_E2E_TOKEN_METADATA

TESTS_E2E_FIXTURES="$FIXTURE_DECRYPTION_KEY"

export TESTS_E2E_FIXTURES

nix develop -c rake "run_on[preprod,sync,true]" # SPEC_OPTS="-e '<match>'"

rm "$tmpfile"
