#! /usr/bin/env bash
# shellcheck shell=bash

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")

git checkout "$RELEASE_CANDIDATE_BRANCH"

rm -Rv result
nix build -o result/macos-silicon .#packages.aarch64-darwin.ci.artifacts.macos-silicon.release
