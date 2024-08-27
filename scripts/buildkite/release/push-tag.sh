#! /usr/bin/env bash

set -euox pipefail

RELEASE_GIT_COMMIT=$(buildkite-agent meta-data get release-candidate-commit)

git tag -l | xargs git tag -d
git fetch --tags

if [ "$RELEASE" == "false" ]; then
    git tag -d nightly || true
    TAG=nightly
else
    TAG=$(buildkite-agent meta-data get release-version)
    exists=$(git tag -l "$TAG")
    if [ -n "$exists" ]; then
        echo "Tag $TAG already exists. Remove it before proceeding."
        exit 1
    fi
fi

git tag -m "$TAG" "$TAG" "$RELEASE_GIT_COMMIT"

git remote set-url origin "https://$GITHUB_TOKEN@github.com/cardano-foundation/cardano-wallet.git"

git push origin -f "$TAG"
