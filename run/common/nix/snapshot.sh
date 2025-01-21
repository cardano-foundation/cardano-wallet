#!/usr/bin/env -S nix shell "github:input-output-hk/mithril" --command bash
# shellcheck shell=bash

set -euox pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# shellcheck disable=SC1091
# shellcheck disable=SC2086
source ${SCRIPT_DIR}/.env

export AGGREGATOR_ENDPOINT
export GENESIS_VERIFICATION_KEY

mkdir -p ./databases

NODE_DB=${NODE_DB:=./databases/node-db}
mkdir -p "$NODE_DB"
rm -rf "${NODE_DB:?}"/*

digest=$(mithril-client cdb snapshot list --json | jq -r .[0].digest)
(cd "${NODE_DB}" && mithril-client cdb download "$digest")
(cd "${NODE_DB}" && mv db/* . && rm -rf db)