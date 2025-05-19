#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

rm -rf "${NODE_STATE_DIR:?}"/*
mkdir -p "$NODE_STATE_DIR"

export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d
mithril() {
    # shellcheck disable=SC2048
    # shellcheck disable=SC2086
    nix shell "github:input-output-hk/mithril?ref=2517.1" -c $*
}

mithril echo "mithril is available" || exit 44

digest=$(mithril mithril-client cdb snapshot list --json | jq -r .[0].digest)
(cd "${NODE_STATE_DIR}" && mithril mithril-client cdb download "$digest")
