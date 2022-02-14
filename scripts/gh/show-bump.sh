#!/usr/bin/env -S nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#gnugrep nixpkgs#gnused nixpkgs#html-xml-utils --inputs-from . --command bash
# shellcheck shell=bash disable=SC2016

# Needs gnused and W3C html-xml-utils

set -euo pipefail

hxnormalize -x 'https://bump.sh/doc/cardano-wallet-diff/changes' \
    | hxselect "ul.changelog-event__diff" \
    | hxselect -s '\n' "ul:first-child" \
    | sed -z 's/\n[ ]\+/ /g' \
    | hxselect -c -s '\n' 'li' \
    | sed 's/^\([ ]*\)\([A-Z][^:]\+: \)\(.*\)$/\1- \2`\3`/g'
