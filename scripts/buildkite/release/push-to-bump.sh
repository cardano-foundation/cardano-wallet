#! /usr/bin/env bash

set -euox pipefail


if [[ -n "${BUILDKITE-}" ]]; then
    RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get release-candidate-commit --default "")
    if [[ "${RELEASE_CANDIDATE_COMMIT}" != "" ]]; then
        git checkout -b "$RELEASE_CANDIDATE_COMMIT" || true
    fi
else
    RELEASE_CANDIDATE_COMMIT=""
fi


npm install bump-cli@2.8.4

export PATH=$PWD/node_modules/.bin:$PATH

bump --version

mkdir -p artifacts

if [[ "$RELEASE" == "true" ]]; then
    TOKEN="$BUMP_RELEASE_TOKEN"
    REPO=cardano-wallet-backend
else
    TOKEN="$BUMP_DAILY_TOKEN"
    REPO=cardano-wallet-backend-daily
fi

bump diff \
        --doc "$REPO" \
        --token "$TOKEN" \
        specifications/api/swagger.yaml | tee artifacts/api-diffs.md;

if [[ "${RELEASE_CANDIDATE_COMMIT}" != "" ]]; then
    bump deploy \
        --doc "$REPO" \
        --token "$TOKEN" \
        specifications/api/swagger.yaml
fi
