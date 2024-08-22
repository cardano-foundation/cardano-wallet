#! /bin/bash

set -euox pipefail

RELEASE_GIT_COMMIT=$(buildkite-agent meta-data get release-candidate-commit)

git tag -l | xargs git tag -d
git fetch --tags

git tag -d nightly || true

git tag -m nightly nightly "$RELEASE_GIT_COMMIT"

git remote set-url origin "https://$PUSH_ARTIFACTS_TOKEN@github.com/cardano-foundation/cardano-wallet.git"

git push origin -f nightly
