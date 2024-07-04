#! /usr/bin/env bash

set -euox pipefail

git config --global user.email "gha@cardanofoundation.org"
git config --global user.name "Github Action"
git remote set-url origin "git@github.com:cardano-foundation/cardano-wallet.git"

TAG="release-candidate-tag"
date=$(git show -s --format=%ci | awk '{print $1}')
DTAG="$TAG/$date"

git tag --delete "$DTAG" || true
git push origin --delete "$DTAG" || true
git tag "$DTAG" "$BUILDKITE_COMMIT"
git push origin "$DTAG"
