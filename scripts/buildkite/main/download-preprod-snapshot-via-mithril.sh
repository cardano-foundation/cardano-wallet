#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p "$NODE_DB_DIR"
rm -rf "${NODE_DB_DIR:?}"/*

mithril() {
    # shellcheck disable=SC2048
    # shellcheck disable=SC2086
    nix shell "github:input-output-hk/mithril" -c $*
}

mithril echo "mithril is available" || exit 44

digest=$(mithril mithril-client cdb snapshot list --json | jq -r .[0].digest)
(cd "${NODE_DB_DIR}" && mithril mithril-client cdb download "$digest")
(cd "${NODE_DB_DIR}" && mv db/* . && rm -rf db)