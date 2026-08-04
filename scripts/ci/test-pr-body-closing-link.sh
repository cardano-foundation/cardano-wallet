#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)
fixture_dir="$script_dir/fixtures/pr-body-closing-link"
validator=${PR_BODY_VALIDATOR:-"$script_dir/check-pr-body-closing-link.sh"}
cases_exercised=0

fail() {
    echo "PR body closing-link fixture failure: $*" >&2
    exit 1
}

run_case() {
    local event_name=$1
    local expected=$2
    local event_path=$3
    local case_name=${4:-$(basename -- "$event_path")}
    local output
    local status

    jq -e . "$event_path" >/dev/null ||
        fail "[$case_name] fixture is not valid JSON"

    set +e
    output=$(
        GITHUB_EVENT_NAME="$event_name" \
            GITHUB_EVENT_PATH="$event_path" \
            "$validator" 2>&1
    )
    status=$?
    set -e

    case "$expected" in
        accept)
            [ "$status" -eq 0 ] ||
                fail "[$case_name] expected acceptance, exit=$status; output: $output"
            printf '%s\n' "$output" | grep -Fqi 'valid closing' ||
                fail "[$case_name] acceptance omitted the valid-closing signal"
            ;;
        reject)
            [ "$status" -ne 0 ] ||
                fail "[$case_name] expected rejection, but validator accepted it"
            printf '%s\n' "$output" | grep -Fq 'Closes #123' ||
                fail "[$case_name] rejection omitted Closes #123 remediation; output: $output"
            ;;
        skip)
            [ "$status" -eq 0 ] ||
                fail "[$case_name] expected a successful skip, exit=$status; output: $output"
            printf '%s\n' "$output" | grep -Fqi 'skip' ||
                fail "[$case_name] skip omitted the observable skip signal"
            if printf '%s\n' "$output" | grep -Fqi 'valid closing'; then
                fail "[$case_name] skip was indistinguishable from PR validation"
            fi
            ;;
        *)
            fail "[$case_name] unknown expected verdict: $expected"
            ;;
    esac

    cases_exercised=$((cases_exercised + 1))
    echo "PASS [$case_name] expected=$expected"
}

accepted_fixtures=(
    valid-close.json
    valid-closes.json
    valid-closed.json
    valid-fix.json
    valid-fixes.json
    valid-fixed.json
    valid-resolve.json
    valid-resolves.json
    valid-resolved.json
    valid-uppercase-colon.json
    valid-multiline.json
    valid-visible-outside-comment.json
)

for fixture in "${accepted_fixtures[@]}"; do
    run_case pull_request accept "$fixture_dir/$fixture"
done

rejected_fixtures=(
    invalid-missing-body.json
    invalid-empty-body.json
    invalid-relation-only.json
    invalid-bare-reference.json
    invalid-zero-reference.json
    invalid-negative-reference.json
    invalid-nonnumeric-reference.json
    invalid-mixed-reference.json
    invalid-keyword-substring.json
    invalid-comment-only.json
    invalid-multiline-comment-only.json
    invalid-comment-token-join.json
    invalid-comment-boundary-separator.json
)

for fixture in "${rejected_fixtures[@]}"; do
    run_case pull_request reject "$fixture_dir/$fixture"
done

run_case push skip "$fixture_dir/skip-push.json"

tmp_root=$(mktemp -d)
trap 'rm -rf -- "$tmp_root"' EXIT
template_event="$tmp_root/template-event.json"
jq -n --rawfile body "$repo_root/.github/PULL_REQUEST_TEMPLATE.md" \
    '{pull_request: {body: $body}}' >"$template_event"
run_case pull_request reject "$template_event" template-comment-only

[ "$cases_exercised" -gt 0 ] ||
    fail "zero cases were exercised"
echo "PR body closing-link fixture suite passed: $cases_exercised cases."
