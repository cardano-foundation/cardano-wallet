
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")

git fetch --all
git checkout "$RELEASE_CANDIDATE_COMMIT"

rm -rf ./result/*
nix build -o result/linux .#ci.artifacts.linux64.release