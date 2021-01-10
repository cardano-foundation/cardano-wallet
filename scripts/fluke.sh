#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils expect moreutils
# shellcheck shell=bash

######################################################################
#
# Finds flaky tests by running them repeatedly until failure.
#
######################################################################

if [ -z "$1" ]; then
  echo "usage: $0 PROGRAM [ARGS...]"
  exit 1
fi

set -euo pipefail

x=0

time ( while true; do
  echo "*** fluke run $((++x))"
  unbuffer "$@" 2>&1 | ts "[$x %Y-%m-%d %H:%M:%S]"
  # reset colours and show cursor
  echo -en '\033[0m\033[?12l\033[?25h'
done )
