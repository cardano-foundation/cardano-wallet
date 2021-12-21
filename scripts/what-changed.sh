#!/usr/bin/env bash

# This will print the top-level files/directories modified in a
# branch, compared to its base branch.

set -euo pipefail

if [ -z "${2:-}" ]; then
  echo "usage: $0 BASE_REF BRANCH_REF"
  exit 1
fi

base="$1"
branch="$2"

git log --name-only --pretty=format: "${base}..${branch}" | grep -v '^$' | sed -e 's|^"||' -e 's|"$"||' -e 's|^\([^/]*\).*$|\1|' | sort -u
