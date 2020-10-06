#!/usr/bin/env bash

# Script for analysing recent CI failures.

# TODO: Add option to output raw JSON.
# TODO: Add option to specify how many PRs to fetch
# TODO: Compute range of occurance rate for each tag, with one value including unclassified failures, and the other excluding.
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
          | select(.bodyText | contains("This PR was included in a batch that successfully built, but then failed to merge into master (it was a non-fast-forward update). It will be automatically retried.") | not)
          | select(.bodyText | contains("Already running a review") | not)
          | . + {succeded: (.bodyText | contains("Build succeeded"))}

          # Extract lines starting with # as tags. Mostly for linking to issues.
          | . + {tags: (.bodyText | split("\n") | map (select(startswith("#"))) )}
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
      + colors.yellow + (.tags | join(", ")) + " "
      + colors.blue + .url + colors.reset+"\n"
      + (if (.succeded | not) and (.tags | length) == 0 then .bodyText+"\n\n" else "" end)' # only show full text of unclassified failures

AGGREGATED_DATA=$(echo $DATA | jq 'def filter_expected: map(select(any(.tags[]; . == "#expected") | not));
      . | filter_expected | reduce .[] as $x ( {runs: [], succeded: 0, total: 0, failed: 0};
      .runs += [$x] | .total += 1 | if $x.succeded then .succeded += 1 else .failed += 1 end
      )')
echo $AGGREGATED_DATA | jq -r '
  def round: . + 0.5 | floor;
  "succeded: " + (.succeded | tostring) +
  ", failed: " + (.failed | tostring) + " (" + (100 * .failed / .total | round | tostring) + "%)" +
  ", total: " + (.total | tostring) + "\nexcluding #expected failures"'

echo ""
echo "Broken down:"

echo $DATA | jq -r 'def yellow: "\u001b[33m"; def reset: "\u001b[0m"; def filter_expected: map(select(any(.tags[]; . == "#expected") | not)); def blue: "\u001b[34m";
  . | filter_expected | map(select(.succeded == false)) |  group_by(.tags)
  | map({count: length, tags: .[0].tags, example_url: .[0].url})
  | .[] | yellow + (.tags | join(", ")) + reset + " => " + (.count | tostring) + " times (e.g. " + blue + .example_url + reset + ")"'

