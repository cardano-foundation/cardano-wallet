#! /usr/bin/env -S nix shell 'nixpkgs#rsync' --command bash
# shellcheck shell=bash

set -euox pipefail

cd test/e2e

TESTS_NODE_DB="$(pwd)/state/node_db"
export TESTS_NODE_DB

mkdir -p "$TESTS_NODE_DB"/preprod
rsync -a "$NODE_STATE_DIR/db-new" "$TESTS_NODE_DB/preprod"

git fetch --all
# RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")
RELEASE_CANDIDATE_BRANCH="release-candidate-new/v$(date +%Y-%m-%d)"
echo "RELEASE_CANDIDATE_BRANCH=$RELEASE_CANDIDATE_BRANCH"

git checkout "$RELEASE_CANDIDATE_BRANCH"

CARDANO_NODE_CONFIGS="$(pwd)/../../configs/cardano"
export CARDANO_NODE_CONFIGS

tmpfile=$(mktemp /tmp/node-preprod.XXXXXX)

CARDANO_NODE_SOCKET_PATH="$tmpfile"
export CARDANO_NODE_SOCKET_PATH

nix-shell --run "./run_all_tests.sh"

rm "$tmpfile"