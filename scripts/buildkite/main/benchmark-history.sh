#! /usr/bin/env -S nix shell .#benchmark-history nixpkgs#gnutar -c bash
# shellcheck shell=bash

set -euox pipefail

mkdir -p ./benchmark-history

benchmark-history \
    --since 2024-08-25 \
    --charts-dir benchmark-history

# shellcheck disable=SC2295
branch="${BUILDKITE_BRANCH#release-candidate/}"

# Sanitize the branch variable to replace '/' with '-'. Useful when not running
# against a release candidate branch.
branch_sanitized="${branch//\//-}"

mkdir -p artifacts

mv benchmark-history/benchmark-history.csv .

# shellcheck disable=SC2086
tar -czf ./benchmark-history.${branch_sanitized}.tgz benchmark-history
