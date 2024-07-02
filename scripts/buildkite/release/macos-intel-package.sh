#! /usr/bin/env bash
# shellcheck shell=bash

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")

git checkout "$RELEASE_CANDIDATE_BRANCH"

rm -Rv result
nix build -o result/macos-intel .#packages.x86_64-darwin.ci.artifacts.macos-intel.release
