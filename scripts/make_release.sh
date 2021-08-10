#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gnused jq curl go-jira
# shellcheck shell=bash

set -euo pipefail

# A handy utility for filling in the github RELEASE_TEMPLATE.
# Since we are using nix, this script shoud work whether on linux or mac.
#
# NOTE: This script will target the LATEST wiki version. Make sure the
# wiki hasn't changed undesirably since the release tag.
#
# Usage:
# cd cardano-wallet # (only works from here!)
# export GITHUB_API_TOKEN="..."
# ./scripts/make_release.sh
#

################################################################################
# Release-specific parameters (Change when you bump the version)
# Release tags must follow format vYYYY-MM-DD.
GIT_TAG="v2021-08-11"
OLD_GIT_TAG="v2021-07-30"
CARDANO_NODE_TAG="alonzo-purple-1.0.1"

################################################################################
# Tag munging functions

# date from git tag
tag_date() {
  echo "${1##v}"
}
# cabal version from git tag
tag_cabal_ver() {
  tag_date "$1" | sed -e s/-0/-/g -e s/-/./g
}
# cabal version regular expression from git tag (escaped .)
tag_cabal_ver_re() {
  tag_cabal_ver "$1" | sed -e 's/\./\\./g'
}

################################################################################
# Update versions

OLD_DATE=$(tag_date $OLD_GIT_TAG)
OLD_CABAL_VERSION=$(tag_cabal_ver $OLD_GIT_TAG)
OLD_CABAL_VERSION_RE=$(tag_cabal_ver_re $OLD_GIT_TAG)
CABAL_VERSION=$(tag_cabal_ver $GIT_TAG)

echo ""
echo "Replacing $OLD_CABAL_VERSION with $CABAL_VERSION"
git ls-files '*.nix' '*.cabal' '*swagger.yaml' docker-compose.yml | xargs sed -i "s/$OLD_CABAL_VERSION_RE/$CABAL_VERSION/"
echo ""

echo "Updating docker-compose.yml with $CARDANO_NODE_TAG cardano-node tag"
sed -i "s|inputoutput/cardano-node:.*|inputoutput/cardano-node:$CARDANO_NODE_TAG|" docker-compose.yml
echo ""

echo "Looking for remaining references to old version $OLD_CABAL_VERSION:"
git grep "$OLD_CABAL_VERSION_RE" || echo "Nothing - good."
echo ""

################################################################################
# ChangeLog

OUT=GENERATED_RELEASE_NOTES-$GIT_TAG.md
CHANGELOG=GENERATED_CHANGELOG.md
KNOWN_ISSUES=GENERATED_KNOWN_ISSUES.md
REPO="input-output-hk/cardano-wallet"
WIKI_COMMIT=$(git ls-remote https://github.com/$REPO.wiki.git HEAD | cut -f1)

echo "Generating changelog into $CHANGELOG..."
./scripts/make_changelog.sh "$OLD_DATE" > $CHANGELOG
echo ""

echo "Generating unresolved issues list into $KNOWN_ISSUES..."
( jira release-notes-bugs || echo "TBD" ) > $KNOWN_ISSUES
echo ""

echo "Filling in template into $OUT..."
sed -e "s/{{GIT_TAG}}/$GIT_TAG/g"                   \
    -e "s/{{CARDANO_NODE_TAG}}/$CARDANO_NODE_TAG/g" \
    -e "s/{{CABAL_VERSION}}/$CABAL_VERSION/g"       \
    -e "s/{{WIKI_COMMIT}}/$WIKI_COMMIT/g"           \
    -e "/{{CHANGELOG}}/r $CHANGELOG"                \
    -e "/{{CHANGELOG}}/d"                           \
    -e "/{{KNOWN_ISSUES}}/r $KNOWN_ISSUES"          \
    -e "/{{KNOWN_ISSUES}}/d"                        \
    .github/RELEASE_TEMPLATE.md > $OUT

################################################################################
# Commit and tag

read -p "Do you want to create a commit and release-tag? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
  msg="Bump version from $OLD_CABAL_VERSION to $CABAL_VERSION"
  git diff --quiet || git commit -am "$msg"
  git tag -s -m "$GIT_TAG" "$GIT_TAG"
fi
