#!/usr/bin/env bash

set -euo pipefail

source "${BASH_SOURCE%/*}/common.sh"

dest_branch=${1:-}
badge_url=${2:-}

if [ -z "$badge_url" ]; then
  echo "usage: $0 DEST_BRANCH BADGE_URL"
  exit 1
fi

check_branch "$dest_branch"

out="hydra-badge.svg"

echo "GET $badge_url"
curl --fail "$badge_url" -o "$out"
git add "$out"

commit_and_push "badge: hydra"
