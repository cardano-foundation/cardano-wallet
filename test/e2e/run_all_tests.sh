#!/usr/bin/env bash

TESTS_E2E_STATEDIR=./state \
TESTS_E2E_BINDIR=./bins \
TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/ \
CARDANO_NODE_SOCKET_PATH=./state/node.socket \
rake stop_node_and_wallet
rake run_on[preprod,sync,skip_bins]
