#!/usr/bin/env bash

set -euo pipefail

if [ -n "${BUILDKITE:-}" ]; then
    since_date="$(buildkite-agent meta-data get last-release-date)"
fi

per_page=30
fetch_prs_at_page() {
    results=$(curl --silent \
        -H "Accept: application/vnd.github+json" \
        -H "Authorization: Bearer $GITHUB_TOKEN" \
        -H "X-GitHub-Api-Version: 2022-11-28" \
        "https://api.github.com/search/issues?q=repo:cardano-foundation/cardano-wallet+is:pr+is:merged+merged:%3E$since_date&per_page=$per_page&page=$1")
    echo "$results"
}

fetch_prs() {
    local page=1
    local all_items=""
    local retrieved_count=0
    while true; do
        results=$(fetch_prs_at_page "$page")
        new_items=$(jq '.items | map({date:.pull_request.merged_at | split("T")[0], number:.number,title:.title,labels:.labels| map(.name),author:.user.login })' <<<"$results")
        all_items=$(jq -s '.[0] + .[1]' <(echo "$all_items") <(echo "$new_items"))
        total_count=$(jq '.total_count' <<<"$results")
        items_count=$(jq 'length' <<<"$new_items")
        retrieved_count=$((retrieved_count + items_count))
        if [ "$retrieved_count" -lt "$total_count" ]; then
            page=$((page + 1))
        else
            break
        fi
    done
    echo "$all_items"
}

items=$(fetch_prs)
sorted_items=$(jq 'sort_by(.date)' <<<"$items")
string_items=$(jq 'map("\(.date), #\(.number), \(.author), \(.labels | join(", ")), \(.title)")' <<<"$sorted_items")
item_max=$(jq 'length - 1' <<<"$items")

mkdir -p artifacts

rm -f artifacts/changes.md

for i in $(seq 0 "$item_max"); do
    item=$(jq -r ".[$i]" <<<"$string_items")
    echo "$item" |
        sed -E 's,\[(ADP\-[0-9]+)\],[\1](https://cardanofoundation.atlassian.net/browse/\1),g' |
        sed -E 's,\[ADP-x+\],,g' |
        sed -E 's,#([0-9]+),[#\1](https://github.com/cardano-foundation/cardano-wallet/pull/\1),g' \
            >>artifacts/changes.md
done
