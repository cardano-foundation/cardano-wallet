#!/usr/bin/env bash

set -euox pipefail

git fetch --all

RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")
RELEASE_VERSION=$(buildkite-agent meta-data get "release-version")

git checkout "$RELEASE_CANDIDATE_COMMIT"

mkdir -p result

mkdir -p artifacts

TARGET="artifacts/cardano-wallet-$RELEASE_VERSION-docker-image.tgz"

nix build .#dockerImage -o "$TARGET"

docker load < "$TARGET"
