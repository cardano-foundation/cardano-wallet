
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit" --default="${BUILDKITE_COMMIT}")

git fetch --all
git checkout "$RELEASE_CANDIDATE_COMMIT"

rm -rf ./result/*
nix build -L -o result/linux .#ci.artifacts.linux64.release
