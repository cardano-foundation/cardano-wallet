#!/usr/bin/env bash

set -euox pipefail

git fetch --all

default_commit=$(git rev-parse HEAD)
RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit" --default "$default_commit")

default_version=c$default_commit
RELEASE_VERSION=$(buildkite-agent meta-data get "release-version" --default "$default_version")

RELEASING=$(buildkite-agent meta-data get "release-version" --default "testing")

git checkout "$RELEASE_CANDIDATE_COMMIT"

mkdir -p result

mkdir -p artifacts

TARGET="artifacts/cardano-wallet-$RELEASE_VERSION-docker-image.tgz"

if [ "$RELEASING" = "testing" ]; then
    nix build -L .#dockerTestImage -o "$TARGET"
else
    nix build -L .#dockerImage -o "$TARGET"
fi

output=$(docker load <"$TARGET")

docker_image_tag=$(echo "$output" | sed -n 's/Loaded image: cardanofoundation\/cardano-wallet:\(.*\)/\1/p')

buildkite-agent meta-data set "docker-image-tag" "$docker_image_tag"
