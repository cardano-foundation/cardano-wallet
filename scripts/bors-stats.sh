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
          | select(.bodyText | contains("try\nBuild") | not)
          | select(.bodyText | contains("Canceled") | not)
          | select(.bodyText | contains("Merge conflict") | not)
          | select(.bodyText | contains("Rejected by too few approved reviews") | not)
          | select(.bodyText | contains("This PR was included in a batch that") | not)
          | select(.bodyText | contains("Already running a review") | not)
          | . + {succeded: (.bodyText | contains("Build succeeded"))}

          # Extract lines starting with # as tags. Mostly for linking to issues.
          | . + {tags: (.bodyText | split("\n") | map (select(startswith("#"))) | map(split(" ") | .[0]) )}
        )
      ')

TITLEQUERY=$(cat <<-END
query {
repository(name: "cardano-wallet", owner: "input-output-hk") {
	issues(labels: ["Test failure"], last: 100) { edges { node {
    number,
    url,
    title
	}}}
}
}
END)

# Get a map from issue number to title
TITLEMAP=$(echo $TITLEQUERY \
  | jq -aRs '{query: .}' \
  | curl -s https://api.github.com/graphql -X POST -H "Authorization: bearer $GITHUB_API_TOKEN" --data-binary @- \
  | jq '.data.repository.issues.edges | map (.node) | INDEX(.number) | with_entries({key: ("#" + .key), value: .value})')


# Show the data in a nice way, and with some added summaries.
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
      def bold: "\u001b[1m";
      def reset: colors.reset;
      def title_map_data: '"$TITLEMAP"';
      def round: . + 0.5 | floor;
      def url: colors.blue + . + reset;
      def show_comment: (if .succeded then colors.green else colors.red end) + (.createdAt | fromdate | strftime("%d %b %H:%M")) + " "
        + colors.yellow + (.tags | join(", ")) + " "
        + (.url | url) +"\n"
        + (if (.succeded | not) and (.tags | length) == 0 then .bodyText+"\n\n" else "" end); # only show full text of unclassified failures
      def exclude_expected: map(select(any(.tags[]; . == "#expected" or (. == "#duplicate")) | not));
      def aggregate_summary: .
        | exclude_expected
        | reduce .[] as $x ( {runs: [], succeded: 0, total: 0, failed: 0};
             .runs += [$x] | .total += 1 | if $x.succeded then .succeded += 1 else .failed += 1 end
          );

      def lookup_issue: . as $number | if (title_map_data | has($number)) then title_map_data[$number] else null end;
      def show_breakdown_by_tag: .
        | exclude_expected | map(select(.succeded == false)) |  group_by(.tags)
        | map({count: length, tags: .[0].tags, example_url: .[0].url})
        | sort_by(.count)
        | reverse
        | .[]
        | bold + (.count | tostring) + reset + " times "
          + colors.yellow + (.tags | join(", ")) + colors.reset
          + " " + (.tags | .[0] | if . == null then "" else (lookup_issue | bold + .title + reset + " | " + (.url | url)) end);
      def show_summary: "succeded: " + (.succeded | tostring) +
          ", failed: " + (.failed | tostring) + " (" + (100 * .failed / .total | round | tostring) + "%)" +
          ", total: " + (.total | tostring) + "\nexcluding #expected failures";
      . | map(show_comment)
          + [(. | aggregate_summary | show_summary)]
          + [""]
          + ["Broken down by tags/issues:"]
          + [. | show_breakdown_by_tag]
        | join ("\n")'
