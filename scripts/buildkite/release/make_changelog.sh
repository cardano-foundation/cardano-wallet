#!/usr/bin/env bash

set -euo pipefail

since_date="$(buildkite-agent meta-data get last-release-date)"

fetch_prs() {
    curl --silent \
    -H "Accept: application/vnd.github+json" \
    -H "Authorization: Bearer $GITHUB_TOKEN" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "https://api.github.com/search/issues?q=repo:cardano-foundation/cardano-wallet+is:pr+is:merged+merged:%3E$since_date"
    }

prs=$(fetch_prs | jq '.items | map({date:.pull_request.merged_at | split("T")[0], number:.number,title:.title,labels:.labels| map(.name),author:.user.login })')

items=$(jq 'map("\(.date), #\(.number), \(.author), \(.labels | join(", ")), \(.title)")' <<< "$prs")
item_max=$(jq 'length - 1' <<< "$items")

mkdir -p artifacts

for i in $(seq 0 "$item_max"); do
    item=$(jq -r ".[$i]" <<< "$items")
    echo "$item" \
        | sed -E 's,\[(ADP\-[0-9]+)\],[\1](https://cardanofoundation.atlassian.net/browse/\1),g' \
        | sed -E 's,\[ADP-x+\],,g' \
        | sed -E 's,#([0-9]+),[#\1](https://github.com/cardano-foundation/cardano-wallet/pull/\1),g' \
        >> artifacts/changes.md
done
