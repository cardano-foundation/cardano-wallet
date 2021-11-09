#!/usr/bin/env bash

set -euo pipefail

cd "$(dirname "$0")/../.." || exit 8

base="$GITHUB_BASE_REF"
branch="$GITHUB_HEAD_REF"
output="${1:-result}"

printf "Fetching branches\n"
git fetch origin "$base:tmp/a" "$branch:tmp/b"

printf '\nComparing %s..%s\n' "$base" "$branch"
dirs=$(scripts/what-changed.sh tmp/a tmp/b)
printf 'The following top-level paths were changed:\n%s\n' "$dirs"

printf '\nSet output variable "%s"\n' "$output"
echo "::set-output name=$output::$(xargs <<< "$dirs")"
echo

git branch -D tmp/a tmp/b
