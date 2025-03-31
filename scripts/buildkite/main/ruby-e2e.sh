#! /usr/bin/env bash

set -euox pipefail

# absolutize paths

RUBY_TEST_DIR=$(realpath "$RUBY_TEST_DIR")
export RUBY_TEST_DIR

CARDANO_NODE_CONFIGS=$(realpath "$CARDANO_NODE_CONFIGS")
export CARDANO_NODE_CONFIGS

####################
# Provide the binaries
####################

# needed by rake
TESTS_E2E_BINDIR=$(mktemp -d /tmp/bins-XXXXXX)
export TESTS_E2E_BINDIR

if [ -n "${BUILDKITE:-}" ]; then
    CURRENT_VERSION=v2025-03-31
    VERSION=$(buildkite-agent meta-data get "release-version" --default "$CURRENT_VERSION")
    echo "VERSION=$VERSION"
    if [ "$PLATFORM" = "linux" ]; then
        cardano_wallet_dir="linux"
        cardano_wallet_segment="cardano-wallet-$VERSION-linux64"
    elif [ "$PLATFORM" = "macos-silicon" ]; then
        cardano_wallet_dir="$PLATFORM"
        cardano_wallet_segment="cardano-wallet-$VERSION-$PLATFORM"
    else
        echo "Unsupported platform: $PLATFORM"
        exit 1
    fi
    cardano_wallet_tar="result/$cardano_wallet_dir/$cardano_wallet_segment.tar.gz"

    buildkite-agent artifact download "$cardano_wallet_tar" "."
    tar xvzf "$cardano_wallet_tar"
    cp -R "$cardano_wallet_segment"/* "$TESTS_E2E_BINDIR"
    CARDANO_NODE_CONFIGS="$TESTS_E2E_BINDIR/configs/cardano"
    export CARDANO_NODE_CONFIGS
else
    # link the binaries to the temp dir in a loop
    for binary in cardano-node cardano-wallet cardano-cli mithril-client; do
        if ! command -v "$binary" > /dev/null; then
            echo "$binary not found in PATH, you must provide it manually"
            exit 1
        fi
        ln -s "$(which "$binary")" "$TESTS_E2E_BINDIR/$binary"
    done
fi

####################
# Prefill the node db
####################

mkdir -p "logs"

TESTS_LOGDIR="$(pwd)/logs"
rm -rf "${TESTS_LOGDIR:?}"/*
export TESTS_LOGDIR

cd "$RUBY_TEST_DIR"

TESTS_NODE_DB=$(pwd)/state/node_db
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"
rm -rf "${TESTS_NODE_DB:?}"/*


# check if to use mithril by checking if the env var is set or default to false
if [ -n "${NOT_USE_MITHRIL:-}" ]; then
    echo "NOT_USE_MITHRIL is set, skipping mithril"
else
    # download the latest snapshot
    digest=$(mithril-client cdb snapshot list --json | jq -r .[0].digest)
    (cd "${TESTS_NODE_DB}" && mithril-client cdb download "$digest")
    (cd "${TESTS_NODE_DB}" && mv db preprod)
fi

####################
# Run the tests
####################

if [ "$PLATFORM" = "macos-silicon" ]; then
    node_socket=$(mktemp -d /tmp/node-preprod.XXXXXX)
    CARDANO_NODE_SOCKET_PATH="$node_socket/node.socket"
elif
    [ "$PLATFORM" = "linux" ]; then
    node_socket=$(mktemp /tmp/node-preprod.XXXXXX)
    CARDANO_NODE_SOCKET_PATH="$node_socket"
else
    echo "Unsupported platform: $PLATFORM"
    exit 1
fi
export CARDANO_NODE_SOCKET_PATH

TESTS_E2E_STATEDIR=$(pwd)/state
export TESTS_E2E_STATEDIR

TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/
export TESTS_E2E_TOKEN_METADATA

TESTS_E2E_FIXTURES="$FIXTURE_DECRYPTION_KEY"
export TESTS_E2E_FIXTURES

rake "run_on[preprod,sync,true]" # SPEC_OPTS="-e '<match>'"

####################
# Cleanup
####################

rm -rf "$node_socket"
rm -rf "$TESTS_E2E_BINDIR"
