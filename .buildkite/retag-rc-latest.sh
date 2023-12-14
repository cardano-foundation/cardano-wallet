#! /usr/bin/env bash

set -euox pipefail

TAG="rc-latest"
git fetch --tags --force
git remote set-url origin \
  "https://$GITHUB_TOKEN@github.com/cardano-foundation/cardano-wallet.git"
if [ $(git tag -l $TAG) ]; then
  old=$(git rev-list -n 1 $TAG)
  date=$(git show -s --format=%ci $TAG | awk '{print $1}')
  git tag --delete $TAG
  git push origin --delete $TAG || true
  # push a tag with the date of today
  DTAG="$TAG-$date"
  git tag --delete $DTAG || true
  git push origin --delete $DTAG || true
  git tag $DTAG $old
  git push origin $DTAG
else
  echo "RC tagging starts from scratch"
fi
git tag $TAG $BUILDKITE_COMMIT
git push origin $TAG