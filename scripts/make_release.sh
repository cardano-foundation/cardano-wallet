#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep

# A handy utility for filling in the github RELEASE_TEMPLATE.
# Since we are using nix, this script shoud work whether on linux or mac.
#
# NOTE: This script will target the LATEST wiki version. Make sure the wiki hasn't changed undesirably since
# the release tag.
#
# Usage:
# cd cardano-wallet # (only works from here!)
# export GITHUB_API_TOKEN="..."
# ./scripts/make_release.sh
#

###############################################################
# Release-specific parameters (Change when you bump the version)
GIT_TAG="v2020-04-01"
CABAL_VERSION="2020.4.1"

OLD_GIT_TAG="v2020-03-16"
OLD_CABAL_VERSION="2020.3.16"

JORM_TAG="v0.8.15"
CARDANO_NODE_TAG="1.9.3"
###############################################################
TMP_CWD=$(pwd)
OLD_DATE="${OLD_GIT_TAG//v}"
CHANGELOG=GENERATED_CHANGELOG.md
OUT=GENERATED_RELEASE_NOTES-$GIT_TAG.md
REPO="input-output-hk/cardano-wallet"
TMP_WIKI_DIR=/tmp/cardano-wallet-script
rm -rf $TMP_WIKI_DIR
mkdir $TMP_WIKI_DIR
echo $TMP_WIKI_DIR
cd $TMP_WIKI_DIR
git clone https://github.com/$REPO.wiki.git
cd cardano-wallet.wiki

WIKI_COMMIT=$(git log -n 1 --pretty=format:%H)

cd $TMP_CWD
echo "cd $TMP_CWD"

echo ""
echo "Replacing $OLD_CABAL_VERSION with $CABAL_VERSION"
find nix -name "*.nix" -type f | xargs sed -i "s/$OLD_CABAL_VERSION/$CABAL_VERSION/"
find lib -name "*.cabal" -type f | xargs sed -i "s/$OLD_CABAL_VERSION/$CABAL_VERSION/"
echo "Looking for remaining references to old version:"
echo "$(git grep $OLD_CABAL_VERSION)"
echo ""

echo "Generating changelog..."
./scripts/make_changelog $OLD_DATE > $CHANGELOG
echo ""
echo "Filling in template..."
sed -e "
s/{{GIT_TAG}}/$GIT_TAG/g
s/{{JORM_TAG}}/$JORM_TAG/g
s/{{CARDANO_NODE_TAG}}/$CARDANO_NODE_TAG/g
s/{{CABAL_VERSION}}/$CABAL_VERSION/g
s/{{DOCKER_WIKI_COMMIT}}/$WIKI_COMMIT/g
s/{{JORM_CLI_WIKI_COMMIT}}/$WIKI_COMMIT/g
s/{{BYRON_CLI_WIKI_COMMIT}}/$WIKI_COMMIT/g
" .github/RELEASE_TEMPLATE.md | sed -e "/{{CHANGELOG}}/r $CHANGELOG" > $OUT

read -p "Do you want to create a commit? (y/n)" -n 1 -r
echo    # (optional) move to a new line
if [[ $REPLY =~ ^[Yy]$ ]]
then
	git commit -am "Bump version from $OLD_CABAL_VERSION to $CABAL_VERSION"
fi

