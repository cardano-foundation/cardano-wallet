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
# Release-specific parameters. Can be changed interactively by running the script.
# Release tags must follow format vYYYY-MM-DD.
GIT_TAG="v2021-08-27"
OLD_GIT_TAG="v2021-08-11"
CARDANO_NODE_TAG="1.29.0"

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
# Interactively change the release-specific parameter by promting the caller, and
# mutating the script itself.

echo "Previous release: $GIT_TAG"
new_tag=$(date +v%Y-%m-%d)
read -r -e -p "New release tag: " -i "$new_tag" new_tag

SCRIPT=$(realpath "$0")
sed -i -e "s/^OLD_GIT_TAG=\"$OLD_GIT_TAG\"/OLD_GIT_TAG=\"$GIT_TAG\"/g" "$SCRIPT"
sed -i -e "s/^GIT_TAG=\"$GIT_TAG\"/GIT_TAG=\"$new_tag\"/g" "$SCRIPT"

OLD_GIT_TAG=$GIT_TAG
GIT_TAG="$new_tag"

OLD_CARDANO_NODE_TAG=$CARDANO_NODE_TAG
read -r -e -p "Cardano node tag: " -i "$CARDANO_NODE_TAG" CARDANO_NODE_TAG
sed -i -e "s/^CARDANO_NODE_TAG=\"$OLD_CARDANO_NODE_TAG\"/CARDANO_NODE_TAG=\"$CARDANO_NODE_TAG\"/g" "$SCRIPT"

################################################################################
# Update releases in README.md

# We assuming a specific structure and want to insert a tweaked copy of the
# master version, and delete the oldest release.
ln=$(awk '$0 ~ "`master` branch" {print NR}' README.md)
master_line=$(sed -n "$ln"p README.md)
line_to_insert=$(echo "$master_line" | sed -e "s/\`master\` branch/\[$GIT_TAG\](https:\/\/github.com\/input-output-hk\/cardano-wallet\/releases\/tag\/$GIT_TAG)/")
sed -i -e "s/^GIT_TAG=\"$GIT_TAG\"/GIT_TAG=\"$new_tag\"/g" "$SCRIPT"

# Edit from the bottom and up, not to affect the line-numbers.
sed -i -e $((ln+3))d README.md
sed -i -e $((ln+1))i"$line_to_insert" README.md

echo "Automatically updated the list of releases in README.md. Please review the resulting changes."

################################################################################
# Update versions

OLD_DATE=$(tag_date $OLD_GIT_TAG)
OLD_CABAL_VERSION=$(tag_cabal_ver $OLD_GIT_TAG)
OLD_CABAL_VERSION_RE=$(tag_cabal_ver_re $OLD_GIT_TAG)
CABAL_VERSION=$(tag_cabal_ver "$GIT_TAG")

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
    .github/RELEASE_TEMPLATE.md > "$OUT"

################################################################################
# Commit and tag

read -p "Do you want to create a commit (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
  msg="Bump version from $OLD_CABAL_VERSION to $CABAL_VERSION"
  git diff --quiet || git commit -am "$msg"
fi
