#!/usr/bin/env bash
# shellcheck shell=bash

MITHRIL_CLIENT_SOURCE=${MITHRIL_CLIENT_SOURCE:-"github:input-output-hk/mithril?ref=2617.0"}

function mithril() {
    nix shell --quiet "$MITHRIL_CLIENT_SOURCE" --command mithril-client "$@"
}

function jq() {
    nix shell --quiet 'nixpkgs#jq' --command jq "$@"
}

detect_cardano_node_version() {
    if [[ -n "${MITHRIL_CARDANO_NODE_VERSION:-}" ]]; then
        echo "$MITHRIL_CARDANO_NODE_VERSION"
        return
    fi

    if command -v cardano-node >/dev/null 2>&1; then
        local version
        if version=$(cardano-node --version | awk 'NR == 1 { print $2 }') \
            && [[ -n "$version" ]]; then
            echo "$version"
            return
        fi
    fi

    echo "10.7.1"
}

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# shellcheck disable=SC1091
source "${SCRIPT_DIR}/.env"

set_stage() {
    if [[ -n "${MITHRIL_STAGE_FILE:-}" ]]; then
        printf '%s\n' "$1" >"$MITHRIL_STAGE_FILE"
    fi
}

wipe_directory_contents() {
    local dir=$1
    if [[ -z "$dir" || "$dir" == "/" ]]; then
        echo "Refusing to wipe unsafe NODE_DB path: $dir" >&2
        exit 1
    fi
    mkdir -p "$dir"
    (
        shopt -s dotglob nullglob
        rm -rf -- "${dir:?}"/*
    )
}

export AGGREGATOR_ENDPOINT
export GENESIS_VERIFICATION_KEY
export ANCILLARY_VERIFICATION_KEY

mkdir -p ./databases

NODE_DB=${NODE_DB:=./databases/node-db}
wipe_directory_contents "$NODE_DB"

client_version=$(mithril --version 2>/dev/null || echo "unknown")

echo "MITHRIL_CLIENT_SOURCE=$MITHRIL_CLIENT_SOURCE"
echo "MITHRIL_CLIENT_VERSION=$client_version"

set_stage "setup:mithril-list"
snapshots=$(mithril cdb snapshot list --json)
snapshot=$(printf '%s\n' "$snapshots" | jq -c '.[0] // empty')
hash=$(printf '%s\n' "$snapshot" | jq -r '.hash // .digest // empty')
tip=$(printf '%s\n' "$snapshot" | jq -c '.beacon // .tip // null')

if [[ -z "$hash" ]]; then
    echo "FAILED - No Mithril snapshot hash found" >&2
    exit 1
fi

echo "MITHRIL_SNAPSHOT_HASH=$hash"
echo "MITHRIL_SNAPSHOT_TIP=$tip"

if [[ -n "${MITHRIL_SNAPSHOT_INFO_FILE:-}" ]]; then
    jq -n \
        --arg client_source "$MITHRIL_CLIENT_SOURCE" \
        --arg client_version "$client_version" \
        --arg snapshot_hash "$hash" \
        --argjson snapshot_tip "$tip" \
        '{client_source:$client_source, client_version:$client_version, snapshot_hash:$snapshot_hash, snapshot_tip:$snapshot_tip}' \
        >"$MITHRIL_SNAPSHOT_INFO_FILE"
fi

set_stage "setup:mithril-download"
(cd "${NODE_DB}" && mithril cdb download --include-ancillary "$hash")

if [[ -n "${MITHRIL_UTXO_HD_FLAVOR:-}" ]]; then
    case "$MITHRIL_UTXO_HD_FLAVOR" in
        LMDB|Legacy) ;;
        *)
            echo "Unsupported MITHRIL_UTXO_HD_FLAVOR: $MITHRIL_UTXO_HD_FLAVOR" >&2
            exit 1
            ;;
    esac

    cardano_node_version=$(detect_cardano_node_version)
    echo "MITHRIL_UTXO_HD_FLAVOR=$MITHRIL_UTXO_HD_FLAVOR"
    echo "MITHRIL_CARDANO_NODE_VERSION=$cardano_node_version"

    set_stage "setup:mithril-convert"
    (
        cd "${NODE_DB}"
        mithril tools utxo-hd snapshot-converter \
            --db-directory db \
            --cardano-node-version "$cardano_node_version" \
            --utxo-hd-flavor "$MITHRIL_UTXO_HD_FLAVOR" \
            --commit
    )
fi

set_stage "setup:mithril-extract"
(cd "${NODE_DB}" && mv db/* . && rm -rf db)

set_stage "setup:mithril-complete"
