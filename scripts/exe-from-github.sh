#!/bin/sh

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.

# exe-from-github.sh
#
#   Runs an executable fetched from a github repository, on a given release & platform.
#   This only works for executables which follow a similar naming convention, like hlint
#   and stylish-haskell.
#
#   Adapted from https://raw.githubusercontent.com/ndmitchell/neil/master/misc/travis.sh
#
# Usage: ./exe-from-github.sh
#
# Available ENV vars:
#   REPOSITORY           Name of the repository
#   PACKAGE              Name of the release package
#   PLATFORM             Target platform of the executable
#   VERSION              Executable version number
#   RELEASE              Release git tag

set -e

if [ -z "$REPOSITORY" ]; then
  echo "REPOSITORY must be provided as ENV var."
  exit 1
fi

if [ -z "$PACKAGE" ]; then
  echo "PACKAGE must be provided as ENV var."
  exit 1
fi

if [ -z "$PLATFORM" ]; then
  echo "PLATFORM must be provided as ENV var."
  exit 1
fi

if [ -z "$VERSION" ]; then
  echo "VERSION must be provided as ENV var."
  exit 1
fi

if [ -z "$RELEASE" ]; then
  echo "RELEASE must be provided as ENV var."
  exit 1
fi

URL=https://github.com/$REPOSITORY/releases/download/$RELEASE/$PACKAGE-$VERSION-$PLATFORM.tar.gz
TEMP=$(mktemp --directory ."$PACKAGE-XXXXX")

echo "Downloading and running $PACKAGE-$VERSION FROM $URL"

cleanup(){
    rm -r "$TEMP"
}
trap cleanup EXIT

curl --progress-bar --location -o"$TEMP/$PACKAGE.tar.gz" "$URL"
tar -xzf "$TEMP/$PACKAGE.tar.gz" -C"$TEMP"

"$TEMP/$PACKAGE-$VERSION*/$PACKAGE $*"
