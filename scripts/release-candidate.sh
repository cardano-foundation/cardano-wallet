#! /usr/bin/env bash

set -euo pipefail

# date from git tag
tag_date() {
  echo "${1##v}"
}
# cabal version from git tag
tag_cabal_ver() {
  tag_date "$1" | sed -e s/-0/-/g -e s/-/./g
}


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

CARDANO_NODE_TAG="8.1.2"
echo "CARDANO_NODE_TAG=$CARDANO_NODE_TAG"

git checkout "$BASE_COMMIT"
git checkout -b release-candidate/"$NEW_GIT_TAG" || true

sed -i "s|version: .*|version: $NEW_GIT_TAG|" specifications/api/swagger.yaml
git commit -m "Update wallet version in swagger.yaml" specifications/api/swagger.yaml

git ls-files  '*.cabal' | xargs sed -i "s/$OLD_CABAL_VERSION/$NEW_CABAL_VERSION/"
git commit -am "Update cardano-wallet version in *.cabal files"
