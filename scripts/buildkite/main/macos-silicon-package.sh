#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")

git fetch --all
git checkout "$RELEASE_CANDIDATE_COMMIT"

rm -rf ./result/*
nix build -L -o result/macos-silicon .#packages.aarch64-darwin.ci.artifacts.macos-silicon.release
