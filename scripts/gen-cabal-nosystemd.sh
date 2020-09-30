#!/bin/sh

# force systemd off until there's a solution to
# https://github.com/haskell/cabal/issues/5444

set -eux

root="$(dirname "$(dirname "$(realpath "$0")")")"

cd "${root}"

perl -pe 's/\r\n/\n/' cabal.project | perl -00 -n -e "print if not /.*scribe-systemd.*/" > cabal.nosystemd.project
echo "" >> cabal.nosystemd.project
echo "flags: -systemd" >> cabal.nosystemd.project

if [ -e cabal.project.freeze ] ; then
	cp cabal.project.freeze cabal.nosystemd.project.freeze
fi
