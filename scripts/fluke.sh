#! /usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils expect moreutils

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

x=1

time ( while true; do
  echo "*** fluke run $((x++))"
  unbuffer "$@" 2>&1 | ts '[%Y-%m-%d %H:%M:%S]'
  echo -e "\033[0m"
done )
