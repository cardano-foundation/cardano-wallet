#! /usr/bin/env bash
# shellcheck shell=bash

set -euox pipefail

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")

git fetch --all
git checkout "$RELEASE_CANDIDATE_BRANCH"

rm -rf ./result/*
nix build -o result/macos-silicon .#packages.aarch64-darwin.ci.artifacts.macos-silicon.release
