#!/usr/bin/env bash

TESTS_E2E_STATEDIR=./state \
TESTS_E2E_BINDIR=./bins \
TESTS_E2E_TOKEN_METADATA=https://metadata.world.dev.cardano.org/ \
rake run_on[testnet]
