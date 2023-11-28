#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gnused jq curl
# shellcheck shell=bash

set -euo pipefail

# A handy utility for filling in the github RELEASE_TEMPLATE.
# Since we are using nix, this script should work whether on linux or mac.
#
# NOTE: This script will target the LATEST wiki version. Make sure the
# wiki hasn't changed undesirably since the release tag.
#
# Usage:
#   export GITHUB_API_TOKEN="..."
#   ./scripts/make_release.sh [vYYYY-MM-DD] [CARDANO_NODE_TAG]
#

cd "$(dirname "$0" || exit 1)"/..
SCRIPT=$(realpath "$0")

################################################################################
# Release-specific parameters. Can be changed interactively by running the script.
# Release tags must follow format vYYYY-MM-DD.
GIT_TAG="v2023-07-18"
OLD_GIT_TAG="v2023-04-14"
CARDANO_NODE_TAG="8.1.1"

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
# Interactively change the release-specific parameter by prompting the caller, and
# mutating the script itself.

echo "Previous release: $GIT_TAG"
new_tag="${1:-}"
if [ -z "$new_tag" ]; then
  today=$(date +%Y-%m-%d)
  read -r -e -p "New release tag: " -i "v$today" new_tag
fi

if [ "$new_tag" != "$GIT_TAG" ]; then
  sed -i -e "s/^OLD_GIT_TAG=\"$OLD_GIT_TAG\"/OLD_GIT_TAG=\"$GIT_TAG\"/g" "$SCRIPT"
  sed -i -e "s/^GIT_TAG=\"$GIT_TAG\"/GIT_TAG=\"$new_tag\"/g" "$SCRIPT"

  OLD_GIT_TAG=$GIT_TAG
  GIT_TAG="$new_tag"
else
  echo "This release:     $GIT_TAG"
  echo "Previous release: $OLD_GIT_TAG"
fi

new_cardano_node_tag="${2:-}"
if [ -z "$new_cardano_node_tag" ]; then
  read -r -e -p "Cardano node tag: " -i "$CARDANO_NODE_TAG" new_cardano_node_tag
fi

if [ "$new_cardano_node_tag" != "$CARDANO_NODE_TAG" ]; then
  OLD_CARDANO_NODE_TAG=$CARDANO_NODE_TAG
  CARDANO_NODE_TAG="$new_cardano_node_tag"
  sed -i -e "s/^CARDANO_NODE_TAG=\"$OLD_CARDANO_NODE_TAG\"/CARDANO_NODE_TAG=\"$CARDANO_NODE_TAG\"/g" "$SCRIPT"
fi

################################################################################
# Update releases in README.md

if [ "$OLD_GIT_TAG" != "$GIT_TAG" ]; then
  # We assuming a specific structure and want to insert a tweaked copy of the
  # master version, and delete the oldest release.
  ln=$(awk '$0 ~ "`master` branch" {print NR}' README.md)
  master_line=$(sed -n "$ln"p README.md)
  line_to_insert=$(echo "$master_line" | sed -e "s/\`master\` branch/\[$GIT_TAG\](https:\/\/github.com\/input-output-hk\/cardano-wallet\/releases\/tag\/$GIT_TAG)/")
  sed -i -e "s/^GIT_TAG=\"$GIT_TAG\"/GIT_TAG=\"$new_tag\"/g" "$SCRIPT"

  # Edit from the bottom and up, not to affect the line-numbers.
  sed -i -e $((ln + 3))d README.md
  sed -i -e $((ln + 1))i"$line_to_insert" README.md

  echo "Automatically updated the list of releases in README.md. Please review the resulting changes."
fi

################################################################################
# Update versions

OLD_DATE=$(tag_date $OLD_GIT_TAG)
OLD_CABAL_VERSION=$(tag_cabal_ver $OLD_GIT_TAG)
OLD_CABAL_VERSION_RE=$(tag_cabal_ver_re $OLD_GIT_TAG)
CABAL_VERSION=$(tag_cabal_ver "$GIT_TAG")

echo ""
echo "Replacing $OLD_CABAL_VERSION with $CABAL_VERSION"
git ls-files '*.nix' '*.cabal' docker-compose.yml | xargs sed -i "s/$OLD_CABAL_VERSION_RE/$CABAL_VERSION/"
echo ""

echo "Updating swagger.yml with $GIT_TAG tag"
sed -i "s|version: .*|version: $GIT_TAG|" specifications/api/swagger.yaml
echo ""

echo "Updating docker-compose.yml with $CARDANO_NODE_TAG cardano-node tag"
sed -i "s|inputoutput/cardano-node:.*|inputoutput/cardano-node:$CARDANO_NODE_TAG|" docker-compose.yml
echo ""

echo "Looking for remaining references to old version $OLD_CABAL_VERSION:"
git grep "$OLD_CABAL_VERSION_RE" || echo "Nothing - good."
echo ""

################################################################################
# ChangeLog

release_docs="docs/releases/$GIT_TAG"
release_notes="${release_docs}/GENERATED_RELEASE_NOTES.md"
CHANGELOG="$release_docs/GENERATED_CHANGELOG.md"
KNOWN_ISSUES="$release_docs/GENERATED_KNOWN_ISSUES.md"

mkdir -p "$release_docs"

echo "Generating changelog into $CHANGELOG..."
./scripts/make_changelog.sh "$OLD_DATE" >"$CHANGELOG"
echo ""

echo "Generating unresolved issues list into $KNOWN_ISSUES..."
if ! jira release-notes-bugs >"$KNOWN_ISSUES"; then
  echo "The \"jira release-notes-bugs\" command didn't work."
  echo TBD >"$KNOWN_ISSUES"
fi
echo ""

echo "Filling in template into ${release_notes}..."
sed -e "s/{{GIT_TAG}}/$GIT_TAG/g" \
  -e "s/{{CARDANO_NODE_TAG}}/$CARDANO_NODE_TAG/g" \
  -e "s/{{CABAL_VERSION}}/$CABAL_VERSION/g" \
  -e "/{{CHANGELOG}}/r $CHANGELOG" \
  -e "/{{CHANGELOG}}/d" \
  -e "/{{KNOWN_ISSUES}}/r $KNOWN_ISSUES" \
  -e "/{{KNOWN_ISSUES}}/d" \
  .github/RELEASE_TEMPLATE.md >"$release_notes"

################################################################################
# Commit and tag

if git diff --quiet; then
  echo "No git sources changed"
else
  read -p "Do you want to create a commit (y/n) " -n 1 -r
  echo
  if [[ $REPLY =~ ^[Yy]$ ]]; then
    msg="Bump version from $OLD_CABAL_VERSION to $CABAL_VERSION"
    git commit -am "$msg"
  fi
fi
