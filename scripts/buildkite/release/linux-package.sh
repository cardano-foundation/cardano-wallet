#! /usr/bin/env -S nix shell --command bash
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")

git fetch --all
git checkout "$RELEASE_CANDIDATE_BRANCH"

rm -rf ./result/*
nix build -o result/linux .#ci.artifacts.linux64.release