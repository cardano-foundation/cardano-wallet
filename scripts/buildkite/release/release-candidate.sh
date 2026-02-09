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

LOCAL_BRANCH_NAME=$(git branch --show-current)

BASE_COMMIT=$(git rev-parse HEAD)

git checkout "$BASE_COMMIT"

today=$(date +%Y-%m-%d)

NEW_GIT_TAG=v$today

NEW_CABAL_VERSION=$(tag_cabal_ver "$NEW_GIT_TAG")

OLD_GIT_TAG=$(git tag -l "v2*-*-*" | sort | tail -n1)

LAST_RELEASE_DATE=$(tag_date "$OLD_GIT_TAG")

if [ "$OLD_GIT_TAG" == "$NEW_GIT_TAG" ]; then
    echo "Refusing to rewrite last release tag"
    exit 1
fi

OLD_CABAL_VERSION=$(tag_cabal_ver "$OLD_GIT_TAG")

if [ "$OLD_CABAL_VERSION" == "$NEW_CABAL_VERSION" ]; then
    echo "Refusing to rewrite last release cabal version"
    exit 1
fi

CARDANO_NODE_TAG=$(cardano-node version | head -n1 | awk '{print $2}')

if [ -n "${BUILDKITE:-}" ]; then
    BRANCH="$BUILDKITE_BRANCH"
else
    BRANCH="$LOCAL_BRANCH_NAME"
fi

if [ "$BRANCH" == "master" ]; then
    RELEASE_CANDIDATE_BRANCH="release-candidate/$NEW_GIT_TAG"
    TEST_RC="FALSE"
else
    RELEASE_CANDIDATE_BRANCH="test-rc/$BRANCH"
    TEST_RC="TRUE"
fi

git config --global user.email "gha@cardanofoundation.org"
git config --global user.name "Github Action"

git branch -D "$RELEASE_CANDIDATE_BRANCH" || true
git checkout -b "$RELEASE_CANDIDATE_BRANCH" || true

sed -i "s|version: .*|version: $NEW_GIT_TAG|g" specifications/api/swagger.yaml
git commit -m "Update wallet version in swagger.yaml" specifications/api/swagger.yaml

git ls-files '*.cabal' | xargs sed -i "s|$OLD_CABAL_VERSION|$NEW_CABAL_VERSION|g"
git commit -am "Update cardano-wallet version in *.cabal files"

sed -i "s|NODE_TAG=.*|NODE_TAG=$CARDANO_NODE_TAG|g" README.md
sed -i "s|WALLET_TAG=.*|WALLET_TAG=$NEW_CABAL_VERSION|g" README.md
sed -i "s|WALLET_VERSION=.*|WALLET_VERSION=$NEW_GIT_TAG|g" README.md
git commit -am "Update cardano-wallet version in README.md"

sed -i "s|$OLD_GIT_TAG|$NEW_GIT_TAG|g" scripts/buildkite/main/ruby-e2e.sh
git commit -am "Update cardano-wallet version in ruby-e2e.sh"

sed -i "s|RELEASE_WALLET_TAG=.*|RELEASE_WALLET_TAG=$NEW_CABAL_VERSION|g" run/common/docker/run.sh
git commit -am "Update cardano-wallet version in run/common/docker/run.sh"

RELEASE_COMMIT=$(git rev-parse HEAD)

git remote set-url origin "git@github.com:cardano-foundation/cardano-wallet.git"
git remote get-url origin

git push -f origin "$RELEASE_CANDIDATE_BRANCH"

if [ -n "${BUILDKITE:-}" ]; then
    buildkite-agent meta-data set "release-version" "$NEW_GIT_TAG"
    buildkite-agent meta-data set "release-candidate-commit" "$RELEASE_COMMIT"
    buildkite-agent meta-data set "release-candidate-branch" "$RELEASE_CANDIDATE_BRANCH"
    buildkite-agent meta-data set "release-cabal-version" "$NEW_CABAL_VERSION"
    buildkite-agent meta-data set "test-rc" "$TEST_RC"
    buildkite-agent meta-data set "base-build" "$BUILDKITE_BUILD_ID"
    buildkite-agent meta-data set "node-tag" "$CARDANO_NODE_TAG"
    buildkite-agent meta-data set "last-release-date" "$LAST_RELEASE_DATE"
fi

if [ -n "${GITHUB_OUTPUT:-}" ]; then
    {
        echo "release-version=$NEW_GIT_TAG"
        echo "release-candidate-commit=$RELEASE_COMMIT"
        echo "release-candidate-branch=$RELEASE_CANDIDATE_BRANCH"
        echo "release-cabal-version=$NEW_CABAL_VERSION"
        echo "test-rc=$TEST_RC"
        echo "node-tag=$CARDANO_NODE_TAG"
        echo "last-release-date=$LAST_RELEASE_DATE"
    } >> "$GITHUB_OUTPUT"
fi
