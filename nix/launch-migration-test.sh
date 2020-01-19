#!/usr/bin/env bash

# Run the migration-test executable and launch a self-node.
#
# Environment variables you can set:
#   PATH - You should have all necessary programs on the PATH.
#   stateDir - suitable temporary location for databases.
#   src - top of source tree, defaults to parent directory of this script.


set -euo pipefail

if [ -z "${1:-}" ]; then
  echo "usage: $0 (run | step1 | step2)"
  exit 1
fi

if [ -z "${stateDir:-}" ]; then
  echo "$0: set the stateDir environment variable before running this script."
  exit 1
fi

: "${src:=$(cd `dirname $0`/..; pwd)}"

data=$src/lib/jormungandr/test/data/jormungandr
echo "data is $data"

cardano-wallet-jormungandr version
jormungandr --version

exec migration-test $1 launch --state-dir $stateDir --genesis-block $data/block0.bin -- --secret $data/secret.yaml --config $data/config.yaml
