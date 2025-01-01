#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p "$(pwd)/logs"

TESTS_LOGDIR="$(pwd)/logs"
export TESTS_LOGDIR

CARDANO_NODE_CONFIGS="$(pwd)/configs/cardano"
export CARDANO_NODE_CONFIGS

CURRENT_VERSION=v2025-01-01

VERSION=$(buildkite-agent meta-data get "release-version" --default $CURRENT_VERSION)
echo "VERSION=$VERSION"

buildkite-agent artifact \
    download "result/macos-silicon/cardano-wallet-$VERSION-macos-silicon.tar.gz" "."

tar xvzf "result/macos-silicon/cardano-wallet-$VERSION-macos-silicon.tar.gz"

TESTS_E2E_BINDIR="$(pwd)/cardano-wallet-$VERSION-macos-silicon"
export TESTS_E2E_BINDIR

cd test/e2e

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"/preprod
rsync -a --delete "$NODE_STATE_DIR/db/" "$TESTS_NODE_DB/preprod"

tmpdir=$(mktemp -d /tmp/node-preprod.XXXXXX)

CARDANO_NODE_SOCKET_PATH="$tmpdir/node.socket"
export CARDANO_NODE_SOCKET_PATH

TESTS_E2E_STATEDIR=$(pwd)/state
export TESTS_E2E_STATEDIR

TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/
export TESTS_E2E_TOKEN_METADATA

TESTS_E2E_FIXTURES="$FIXTURE_DECRYPTION_KEY"

export TESTS_E2E_FIXTURES

# We have to use the `nix develop` shell defined for x86_64-darwin
# But we can still *test* the aarch64-darwin cardano-wallet executable
nix develop --system x86_64-darwin -c rake "run_on[preprod,sync,true]"

rm -Rv "$tmpdir"
