#!/usr/bin/env bash

# Run the migration-test executable and launch a self-node.
# You should have all necessary programs on the PATH.
# Set the "stateDir" environment variable to a suitable temporary location.

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
