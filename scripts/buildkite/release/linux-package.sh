#! /usr/bin/env -S nix shell --command bash
# shellcheck shell=bash

RELEASE_CANDIDATE_BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")

git checkout "$RELEASE_CANDIDATE_BRANCH"

nix build -o result/linux .#ci.artifacts.linux64.release