#! /usr/bin/env bash
# shellcheck shell=bash

function mithril() {
    nix shell 'github:input-output-hk/mithril?ref=2543.1-hotfix' --command mithril-client $@
}

function jq() {
    nix shell 'nixpkgs#jq' --command jq $@
}

set -euox pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# shellcheck disable=SC1091
# shellcheck disable=SC2086
source ${SCRIPT_DIR}/.env

export AGGREGATOR_ENDPOINT
export GENESIS_VERIFICATION_KEY
export ANCILLARY_VERIFICATION_KEY

mkdir -p ./databases

NODE_DB=${NODE_DB:=./databases/node-db}
mkdir -p "$NODE_DB"
rm -rf "${NODE_DB:?}"/*

hash=$(mithril cdb snapshot list --json | jq -r .[0].hash)
(cd "${NODE_DB}" && mithril cdb download --include-ancillary "$hash")
(cd "${NODE_DB}" && mv db/* . && rm -rf db)
