#! /usr/bin/env bash

set -euox pipefail

TAG="release-candidate-tag"
date=$(git show -s --format=%ci $TAG | awk '{print $1}')
DTAG="$TAG-$date"

git remote set-url origin \
  "https://$GITHUB_TOKEN@github.com/cardano-foundation/cardano-wallet.git"
git tag $DTAG $BUILDKITE_COMMIT
git push origin $DTAG