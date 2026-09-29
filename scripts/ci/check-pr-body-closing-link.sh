#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

event_name=${GITHUB_EVENT_NAME:-}

if [[ "$event_name" != pull_request ]]; then
    echo "Skipping PR body closing-link validation for event: ${event_name:-unknown}."
    exit 0
fi

event_path=${GITHUB_EVENT_PATH:-}
if [[ -z "$event_path" || ! -f "$event_path" ]]; then
    echo "Cannot validate the PR body: GITHUB_EVENT_PATH is missing or unreadable." >&2
    exit 1
fi

if ! body=$(jq -r '
    (.pull_request.body // "")
    | if type == "string" then . else "" end
' "$event_path"); then
    echo "Cannot validate the PR body: the GitHub event payload is invalid." >&2
    exit 1
fi

visible_body=$(
    printf '%s\n' "$body" | awk '
        {
            remainder = $0
            visible = ""
            while (length(remainder) > 0) {
                if (in_comment) {
                    comment_end = index(remainder, "-->")
                    if (comment_end == 0) {
                        remainder = ""
                    } else {
                        remainder = substr(remainder, comment_end + 3)
                        in_comment = 0
                    }
                } else {
                    comment_start = index(remainder, "<!--")
                    if (comment_start == 0) {
                        visible = visible remainder
                        remainder = ""
                    } else {
                        visible = visible substr(remainder, 1, comment_start - 1) " _ "
                        remainder = substr(remainder, comment_start + 4)
                        in_comment = 1
                    }
                }
            }
            print visible
        }
    '
)

closing_pattern='(^|[^[:alnum:]_])(close|closes|closed|fix|fixes|fixed|resolve|resolves|resolved):?[[:space:]]+#[1-9][0-9]*([^[:alnum:]_]|$)'
partial_pattern='(^|[^[:alnum:]_])(part[[:space:]]+of|towards):?[[:space:]]+#[1-9][0-9]*([^[:alnum:]_]|$)'

if printf '%s\n' "$visible_body" | grep -Eiq "$closing_pattern"; then
    echo "PR body contains a valid closing reference."
    exit 0
fi

# A PR that is only one step of a larger issue must not close it. The reference
# is still required, so the PR stays traceable, but GitHub will not close the
# issue on merge; the final PR of the issue carries the closing keyword.
if printf '%s\n' "$visible_body" | grep -Eiq "$partial_pattern"; then
    echo "PR body contains a valid partial reference; the issue will not close on merge."
    exit 0
fi

echo "PR body must contain a visible GitHub issue reference: Closes #123 (or Fixes #123) to close it, or Part of #123 (or Towards #123) when the PR is only part of it." >&2
exit 1
