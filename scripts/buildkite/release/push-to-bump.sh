#! /bin/bash

set -euox pipefail

if [[ -n "${BUILDKITE:-}" ]]; then
    RELEASE_CANDIDATE_COMMIT=$(buildkite-agent meta-data get release-candidate-commit)
else
    RELEASE_CANDIDATE_COMMIT=$(git rev-parse HEAD)
fi

git checkout -b "$RELEASE_CANDIDATE_COMMIT" || true

npm install bump-cli@2.8.2

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
        specifications/api/swagger.yaml > artifacts/api-diffs.md;

bump deploy \
        --doc "$REPO" \
        --token "$TOKEN" \
        specifications/api/swagger.yaml
