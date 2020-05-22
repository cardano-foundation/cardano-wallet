#!/usr/bin/env bash

# Run the migration-test executable and launch a self-node.
#
# Environment variables you can set:
#   PATH - You should have all necessary programs on the PATH.
#   stateDir - suitable temporary location for databases.
#   genesisDataDir - where block0.bin and secret.yaml are
#   configFile - path to config.yaml

set -euo pipefail

if [ -z "${1:-}" ]; then
  echo "usage: $0 (run | step1 | step2)"
  exit 1
fi

if [ -z "${stateDir:-}" ]; then
  echo "$0: set the stateDir environment variable before running this script."
  exit 1
fi

if [ -z "${genesisDataDir:-}" ]; then
  echo "$0: set the genesisDataDir environment variable before running this script."
  exit 1
fi

if [ -z "${configFile:-}" ]; then
  echo "$0: set the configFile environment variable before running this script."
  exit 1
fi

# Find an unused TCP port to run the test on.
# Source: https://unix.stackexchange.com/a/132524
find_unused_port() {
  python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()'
}

# Sanity-check versions
cardano-wallet-jormungandr version
jormungandr --version

exec migration-test $1 launch --port "$(find_unused_port)" --state-dir $stateDir --genesis-block $genesisDataDir/block0.bin -- --secret $genesisDataDir/secret.yaml --config $configFile
