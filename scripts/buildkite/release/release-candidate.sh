#! /usr/bin/env -S nix shell '.#cardano-node' 'nixpkgs#gnused' --command bash
# shellcheck shell=bash

set -euox pipefail

# date from git tag
# example v2023-04-04 -> 2023-04-04
tag_date() {
  echo "${1##v}"
}
# cabal version from git tag
# example v2023-04-04 -> 2023.4.4
tag_cabal_ver() {
  tag_date "$1" | sed -e s/-0/-/g -e s/-/./g
}

git tag -l | xargs git tag -d
git fetch --tags

BASE_COMMIT=$(git rev-parse HEAD)

git checkout "$BASE_COMMIT"

today=$(date +%Y-%m-%d)

NEW_GIT_TAG=v$today

NEW_CABAL_VERSION=$(tag_cabal_ver "$NEW_GIT_TAG")

OLD_GIT_TAG=$( git tag -l "v2*-*-*" | sort | tail -n1)

OLD_CABAL_VERSION=$(tag_cabal_ver "$OLD_GIT_TAG")

CARDANO_NODE_TAG=$(cardano-node version | head -n1 | awk '{print $2}')

if [ "$BUILDKITE_BRANCH" == "master" ]; then
    RELEASE_CANDIDATE_BRANCH="release-candidate/$NEW_GIT_TAG"
    TEST_RC="FALSE"
else
    RELEASE_CANDIDATE_BRANCH="test-rc/$BUILDKITE_BRANCH"
    TEST_RC="TRUE"
fi

git config --global user.email "gha@cardanofoundation.org"
git config --global user.name "Github Action"

git branch -D "$RELEASE_CANDIDATE_BRANCH" || true
git checkout -b "$RELEASE_CANDIDATE_BRANCH" || true

sed -i "s|version: .*|version: $NEW_GIT_TAG|g" specifications/api/swagger.yaml
git commit -m "Update wallet version in swagger.yaml" specifications/api/swagger.yaml

git ls-files  '*.cabal' | xargs sed -i "s|$OLD_CABAL_VERSION|$NEW_CABAL_VERSION|g"
git commit -am "Update cardano-wallet version in *.cabal files"

sed -i "s|NODE_TAG=.*|NODE_TAG=$CARDANO_NODE_TAG|g" README.md
sed -i "s|WALLET_TAG=.*|WALLET_TAG=$NEW_CABAL_VERSION|g" README.md
sed -i "s|WALLET_VERSION=.*|WALLET_VERSION=$NEW_GIT_TAG|g" README.md
git commit -am "Update cardano-wallet version in README.md"

RELEASE_COMMIT=$(git rev-parse HEAD)

git remote set-url origin "git@github.com:cardano-foundation/cardano-wallet.git"
git remote get-url origin

git push -f origin "$RELEASE_CANDIDATE_BRANCH"

buildkite-agent meta-data set "release-version" "$NEW_GIT_TAG"
buildkite-agent meta-data set "release-candidate-commit" "$RELEASE_COMMIT"
buildkite-agent meta-data set "release-candidate-branch" "$RELEASE_CANDIDATE_BRANCH"
buildkite-agent meta-data set "release-cabal-version" "$NEW_CABAL_VERSION"
buildkite-agent meta-data set "test-rc" "$TEST_RC"
buildkite-agent meta-data set "base-build" "$BUILDKITE_BUILD_ID"
