#! /usr/bin/env -S nix shell --command bash
# shellcheck shell=bash

set -euox pipefail

git fetch --all

RELEASE_COMMIT=$(buildkite-agent meta-data get "release-commit")

git checkout "$RELEASE_COMMIT"

mkdir -p result

nix build .#dockerImage -o result/docker-image

docker load < result/docker-image
