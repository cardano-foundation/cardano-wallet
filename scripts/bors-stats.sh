#!/usr/bin/env bash

# Script for analysing recent CI failures.

# TODO: Add option to output raw JSON.
# TODO: Add option to specify how many PRs to fetch
# TODO: Parse attributes from comments to allow categorization
# TODO: Link to create an issue from an uncategorized failure with pre-filled information

set -euo pipefail

# You need to set $GITHUB_API_TOKEN before running this script
: ${GITHUB_API_TOKEN?"Please provide a Github Api Token for fetching pull requests"}

# See https://developer.github.com/v4/explorer/
# to experiment with queries.

# Note: the latest bors comments may not nececarily be in the latest PRs. Fetching more than we need,
# and later sorting by comment date should work decently though.
QUERY=$(cat <<-END
query {
repository(name: "cardano-wallet", owner: "input-output-hk") {
	pullRequests(last: 40) { edges { node {
		comments(first: 100) { edges { node {
			bodyText,
      createdAt,
      url,
      author {
          login
      }
		}}}
	}}}
}
}
END)

DATA=$(echo $QUERY \
  | tr -d ' ' \
  | tr -d '\n' \
  | tr -d '\t' \
  | jq -aRs '{query: .}' \
  | curl -s https://api.github.com/graphql -X POST -H "Authorization: bearer $GITHUB_API_TOKEN" --data-binary @- \
  | jq '
      .data.repository.pullRequests.edges
      | map (.node.comments.edges)
      | flatten
      | map (.node)
      | sort_by (.createdAt)
      | map (
          select(.author.login == "iohk-bors")
          | select(.bodyText | contains("try") | not)
          | select(.bodyText | contains("Canceled") | not)
          | select(.bodyText | contains("Merge conflict") | not)
          | select(.bodyText | contains("Already running a review") | not)
          | . + {succeded: (.bodyText | contains("Build succeeded"))}
        )
      ')

echo $DATA | jq -r \ '
      def colors: # https://stackoverflow.com/a/57298714
       {
       "black": "\u001b[30m",
       "red": "\u001b[31m",
       "green": "\u001b[32m",
       "yellow": "\u001b[33m",
       "blue": "\u001b[34m",
       "magenta": "\u001b[35m",
       "cyan": "\u001b[36m",
       "white": "\u001b[37m",
       "reset": "\u001b[0m",
      };
      .[] | (if .succeded then colors.green else colors.red end) + (.createdAt | fromdate | strftime("%d %b %H:%m")) + " "
      + colors.blue + .url + colors.reset+"\n"
      + (if .succeded then "" else .bodyText+"\n\n" end)'

AGGREGATED_DATA=$(echo $DATA | jq '. | reduce .[] as $x ( {runs: [], succeded: 0, total: 0, failed: 0};
      .runs += [$x] | .total += 1 | if $x.succeded then .succeded += 1 else .failed += 1 end
      )')
echo $AGGREGATED_DATA | jq -r '
  def round: . + 0.5 | floor;
  "succeded: " + (.succeded | tostring) +
  ", failed: " + (.failed | tostring) + " (" + (100 * .failed / .total | round | tostring) + "%)" +
  ", total: " + (.total | tostring)'
