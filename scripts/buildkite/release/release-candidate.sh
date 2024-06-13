#! /usr/bin/env -S nix shell '.#cardano-node' 'nixpkgs#gnused' --command bash
# shellcheck shell=bash

set -euo pipefail

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

BASE_COMMIT=$(git rev-parse HEAD)
echo "BASE_COMMIT=$BASE_COMMIT"

today=$(date +%Y-%m-%d)

NEW_GIT_TAG=v$today
echo "NEW_GIT_TAG=$NEW_GIT_TAG"

NEW_CABAL_VERSION=$(tag_cabal_ver "$NEW_GIT_TAG")
echo "NEW_CABAL_VERSION=$NEW_CABAL_VERSION"

OLD_GIT_TAG=$( git tag -l "v2*-*-*" | sort | tail -n1)
echo "OLD_GIT_TAG=$OLD_GIT_TAG"

OLD_CABAL_VERSION=$(tag_cabal_ver "$OLD_GIT_TAG")
echo "OLD_CABAL_VERSION=$OLD_CABAL_VERSION"

CARDANO_NODE_TAG=$(cardano-node version | head -n1 | awk '{print $2}')
echo "CARDANO_NODE_TAG=$CARDANO_NODE_TAG"

RELEASE_CANDIDATE_BRANCH="release-candidate-new/$NEW_GIT_TAG"
echo "RELEASE_CANDIDATE_BRANCH=$RELEASE_CANDIDATE_BRANCH"

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
export RELEASE_COMMIT
echo "RELEASE_COMMIT=$RELEASE_COMMIT"

git remote set-url origin "git@github.com:cardano-foundation/cardano-wallet.git"
git remote get-url origin

git push -f origin "$RELEASE_CANDIDATE_BRANCH"

buildkite-agent meta-data set "release-version" "$NEW_GIT_TAG"
buildkite-agent meta-data set "release-commit" "$RELEASE_COMMIT"
buildkite-agent meta-data set "release-candidate-branch" "$RELEASE_CANDIDATE_BRANCH"
