#! /usr/bin/env bash

set -euox pipefail

RELEASE_VERSION=$(buildkite-agent meta-data get release-version)
GH_PAGES_BRANCH="gh-pages"

git checkout "${GH_PAGES_BRANCH}"
git pull --rebase origin "${GH_PAGES_BRANCH}"
cd releases
./make_redirects.sh "${RELEASE_VERSION}"
git remote set-url origin "https://$GITHUB_TOKEN@github.com/cardano-foundation/cardano-wallet.git"
git push origin "${GH_PAGES_BRANCH}"
