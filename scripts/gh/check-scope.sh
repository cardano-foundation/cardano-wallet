#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")/../.." || exit 8

output="${1:-result}"
target="${2:-$GITHUB_BASE_REF}"
branch="${3:-$GITHUB_HEAD_REF}"

printf "Fetching branches\n"
git fetch origin --force "$target:tmp/a" "$branch:tmp/b"

base="$(git merge-base tmp/a tmp/b || true)"

if [ -n "$base" ]; then
  printf '\nComparing %s branch base %s..%s\n' "$target" "$base" "$branch"
  dirs=$(scripts/what-changed.sh tmp/a tmp/b)
  printf 'The following top-level paths were changed:\n%s\n' "$dirs"

  printf '\nSet output variable "%s"\n' "$output"
  echo "::set-output name=$output::$(xargs <<< "$dirs")"
  echo
else
  printf '\nNo common base found between %s and %s\n' "$target" "$branch"
fi

git branch -D tmp/a tmp/b
