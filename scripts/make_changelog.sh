#!/usr/bin/env bash

# Constructs a CHANGELOG using every PR merged since a given date. It
# groups PRs by milestones.
#
# /!\ depends on 'jq' and 'curl' /!\
#
# Usage:
#     ./make_changelog.sh YYYY-MM-DD
#
# Example:
#     ./make_changelog.sh 2019-08-15

# NOTE: You can optionally set $GITHUB_API_TOKEN before running this
# script to avoid GitHub rate limit errors.

set -euo pipefail

repo="cardano-foundation/cardano-wallet"
since_date="${1:-}"

if [ -z "$since_date" ]; then
  echo "usage: $0 YYYY-MM-DD"
  exit 1
fi

fetch_prs() {
  if [ -z "${GITHUB_API_TOKEN:-}" ]; then
    echo "warning: GITHUB_API_TOKEN is not set. You may get a rate limit error when fetching pull requests." > /dev/stderr
    auth_header=()
  else
    auth_header=("-H" "Authorization: token $GITHUB_API_TOKEN")
  fi

  url="https://api.github.com/search/issues?per_page=500&q=repo:$1+is:pr+is:merged+merged:%3E$2"
  echo "Fetching $url" > /dev/stderr
  curl --silent "${auth_header[@]}" \
    -H "Accept: application/vnd.github.v3+json" \
    "$url"
}

pull_requests=$(fetch_prs "$repo" "$since_date" | jq '.items | map({number:.number,title:.title,label:.labels[0].name}) | group_by(.label)')

items=$(jq 'map(map("\(.title) #\(.number)"))' <<< "$pull_requests")
labels=$(jq 'map(map("\(.label)") | unique) | flatten' <<< "$pull_requests")

labels_max=$(jq 'length - 1' <<< "$labels")

for i in $(seq 0 "$labels_max"); do
  label=$(jq -r ".[$i]" <<< "$labels")
  case $label in
  "null")
    echo "## Unclassified (Move user-facing items to correct section. Delete the rest)"
      ;;
  "ADDING FEATURE")
      echo "## New Features"
      ;;
  "IMPROVEMENT")
      echo "## Improvements"
      ;;
  "RESOLVING ISSUE")
      echo "## Resolved Issues"
      ;;
  esac
  echo ""
  jq -r '.['"$i"'] | .[] | "- \(.)"' <<< "$items"
  echo ""
done
