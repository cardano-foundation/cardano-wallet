#!/usr/bin/env bash

set -euox pipefail

git fetch --all

default_commit=$(git rev-parse HEAD)
RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit" --default "$default_commit")

default_version=$(git describe --tags --abbrev=0)
RELEASE_VERSION=$(buildkite-agent meta-data get "release-version" --default "$default_version")

git checkout "$RELEASE_CANDIDATE_COMMIT"

mkdir -p result

mkdir -p artifacts

TARGET="artifacts/cardano-wallet-$RELEASE_VERSION-docker-image.tgz"

nix build .#dockerImage -o "$TARGET"

load_out=$(docker load < "$TARGET")

image_name=$(sed -n 's/^Loaded image: \(.*\)$/\1/p' <<< "$load_out")

local_image_name="cardanofoundation/cardano-wallet:$BUILDKITE_BUILD_NUMBER"

docker tag "$image_name" "$local_image_name"
