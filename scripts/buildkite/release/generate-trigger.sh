#!/bin/bash

set -euo pipefail

COMMIT=$(buildkite-agent meta-data get "release-candidate-commit")
VERSION=$(buildkite-agent meta-data get "release-version")
BRANCH=$(buildkite-agent meta-data get "release-candidate-branch")
CABAL=$(buildkite-agent meta-data get "release-cabal-version")
TEST_RC=$(buildkite-agent meta-data get "test-rc")
BASE_BUILD=$(buildkite-agent meta-data get "base-build")

cat << YAML
steps:
  - trigger: cardano-wallet
    depends_on: add-release-commits
    key: main-pipeline-build
    label: Trigger the main pipeline
    build:
        commit: $COMMIT
        branch: $BRANCH
        message: Release Candidate of $VERSION
        env:
            RELEASE_CANDIDATE: "$VERSION"
            TEST_RC: "$TEST_RC"
        meta_data:
            release-version: "$VERSION"
            release-candidate-commit: "$COMMIT"
            release-candidate-branch: "$BRANCH"
            release-cabal-version: "$CABAL"
            triggered-by: "$BASE_BUILD"

YAML
