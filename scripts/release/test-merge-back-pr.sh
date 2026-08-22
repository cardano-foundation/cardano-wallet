#!/usr/bin/env bash
# shellcheck shell=bash
#
# Focused proof for merge-back PR selection, identity, and
# idempotency. Substitutes git/gh; never mutates GitHub.

set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
repo_root=$(cd -- "$script_dir/../.." && pwd)
helper=$script_dir/merge-back-pr.sh
workflow=$repo_root/.github/workflows/publish-release.yml
closing_validator=$repo_root/scripts/ci/check-pr-body-closing-link.sh
cases_exercised=0
failures=0
created_issue=6000
forbidden_issues='5385 5387'

tag=v2020-01-01
head_branch=release-candidate/$tag
tag_sha=49e06790051ea64abb2029dd29a0bdaa7befb19a
other_sha=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
repo=cardano-foundation/cardano-wallet
today=$(date +%Y-%m-%d)

fail_case() {
    echo "FAIL [$1] $2" >&2
    failures=$((failures + 1))
}

pass_case() {
    echo "PASS [$1]"
    cases_exercised=$((cases_exercised + 1))
}

write_fakes() {
    local bin=$1
    mkdir -p -- "$bin"
    cat >"$bin/git" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
: "${FAKE_DIR:?}"
printf 'git %s\n' "$*" >>"$FAKE_DIR/git.log"
if [[ "${1:-}" == fetch ]]; then
    exit 0
fi
if [[ "${1:-}" == rev-parse && "${2:-}" == --verify ]]; then
    ref=${3:-}
    if [[ "$ref" == "${FAKE_TAG}^{commit}" ]]; then
        if [[ -z "${FAKE_TAG_SHA:-}" ]]; then
            echo "fatal: Needed a single revision" >&2
            exit 128
        fi
        printf '%s\n' "$FAKE_TAG_SHA"
        exit 0
    fi
    if [[ "$ref" == "origin/release-candidate/${FAKE_TAG}" ]]; then
        if [[ -z "${FAKE_BRANCH_SHA:-}" ]]; then
            echo "fatal: Needed a single revision" >&2
            exit 128
        fi
        printf '%s\n' "$FAKE_BRANCH_SHA"
        exit 0
    fi
    echo "fatal: Needed a single revision" >&2
    exit 128
fi
echo "unexpected git $*" >&2
exit 1
EOF
    cat >"$bin/gh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
: "${FAKE_DIR:?}"
printf 'gh %s\n' "$*" >>"$FAKE_DIR/gh.log"
if [[ "${1:-}" == pr && "${2:-}" == list ]]; then
    if [[ -f "$FAKE_DIR/pr-list.tsv" ]]; then
        cat -- "$FAKE_DIR/pr-list.tsv"
    fi
    exit 0
fi
if [[ "${1:-}" == pr && "${2:-}" == create ]]; then
    : >"$FAKE_DIR/create.args"
    prev=
    for arg in "$@"; do
        printf '%s\n' "$arg" >>"$FAKE_DIR/create.args"
        if [[ "$prev" == --body-file ]]; then
            cp -- "$arg" "$FAKE_DIR/create.body"
        fi
        prev=$arg
    done
    printf '%s\n' "https://github.com/cardano-foundation/cardano-wallet/pull/9999"
    exit 0
fi
if [[ "${1:-}" == issue && "${2:-}" == list ]]; then
    if [[ -f "$FAKE_DIR/issue-list.tsv" ]]; then
        cat -- "$FAKE_DIR/issue-list.tsv"
    fi
    exit 0
fi
if [[ "${1:-}" == issue && "${2:-}" == create ]]; then
    : >"$FAKE_DIR/issue-create.args"
    prev=
    for arg in "$@"; do
        printf '%s\n' "$arg" >>"$FAKE_DIR/issue-create.args"
        if [[ "$prev" == --body-file ]]; then
            cp -- "$arg" "$FAKE_DIR/issue-create.body"
        fi
        prev=$arg
    done
    printf '%s\n' "https://github.com/cardano-foundation/cardano-wallet/issues/6000"
    exit 0
fi
if [[ "${1:-}" == pr && "${2:-}" =~ ^(merge|review|ready|lock|close)$ ]]; then
    printf '%s\n' "$*" >>"$FAKE_DIR/forbidden.log"
    echo "forbidden gh $*" >&2
    exit 1
fi
if [[ "${1:-}" == issue && "${2:-}" =~ ^(close|reopen|delete|edit)$ ]]; then
    printf '%s\n' "$*" >>"$FAKE_DIR/forbidden.log"
    echo "forbidden gh $*" >&2
    exit 1
fi
echo "unexpected gh $*" >&2
exit 1
EOF
    cat >"$bin/date" <<'EOF'
#!/usr/bin/env bash
echo "date must not select the merge-back tag: $*" >&2
exit 99
EOF
    chmod +x -- "$bin/git" "$bin/gh" "$bin/date"
}

has_arg_pair() {
    local file=$1
    local flag=$2
    local value=$3
    local prev=
    local line
    while IFS= read -r line; do
        if [[ "$prev" == "$flag" && "$line" == "$value" ]]; then
            return 0
        fi
        prev=$line
    done <"$file"
    return 1
}

validate_closing_body() {
    local body_file=$1
    local event_file=$2
    jq -n --rawfile body "$body_file" '{pull_request: {body: $body}}' >"$event_file"
    GITHUB_EVENT_NAME=pull_request GITHUB_EVENT_PATH=$event_file \
        "$closing_validator" >/dev/null 2>&1
}

assert_closes_issue() {
    local body_file=$1
    local issue=$2
    local event_file=$3
    local stripped=$4
    local n
    grep -Eq "(^|[^[:alnum:]_])[Cc]loses[[:space:]]+#${issue}([^[:alnum:]_]|$)" \
        "$body_file" || return 1
    for n in $forbidden_issues; do
        if [[ "$n" != "$issue" ]] \
            && grep -Eq "(^|[^[:alnum:]_])(close|closes|closed|fix|fixes|fixed|resolve|resolves|resolved):?[[:space:]]+#${n}([^[:alnum:]_]|$)" \
                "$body_file"; then
            return 1
        fi
    done
    validate_closing_body "$body_file" "$event_file" || return 1
    grep -vE "(^|[^[:alnum:]_])[Cc]loses[[:space:]]+#[1-9][0-9]*" \
        "$body_file" >"$stripped" || true
    if validate_closing_body "$stripped" "$event_file"; then
        return 1
    fi
    return 0
}

run_helper() {
    local work=$1
    local event_name=$2
    local prerelease=$3
    local output_file=$4
    local status_file=$5
    local status

    write_fakes "$work/bin"
    : >"$work/git.log"
    : >"$work/gh.log"

    set +e
    (
        cd -- "$repo_root"
        export FAKE_DIR=$work
        export FAKE_TAG=$tag
        export FAKE_TAG_SHA="${FAKE_TAG_SHA:-}"
        export FAKE_BRANCH_SHA="${FAKE_BRANCH_SHA:-}"
        export PATH="$work/bin:$PATH"
        TAG=$tag \
            GITHUB_EVENT_NAME=$event_name \
            MERGE_BACK_PRERELEASE=$prerelease \
            GITHUB_REPOSITORY=$repo \
            MERGE_BACK_GIT="$work/bin/git" \
            MERGE_BACK_GH="$work/bin/gh" \
            "$helper"
    ) >"$output_file" 2>&1
    status=$?
    set -e
    printf '%s\n' "$status" >"$status_file"
}

expect_create() {
    local name=$1
    local event_name=$2
    local work output status
    work=$(mktemp -d)
    output=$work/output
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" "$event_name" false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -ne 0 ]]; then
        fail_case "$name" "expected create, exit=$status; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fq "identity: tag=$tag sha=$tag_sha head=$head_branch" "$output"; then
        fail_case "$name" "missing identity line; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fq 'created: https://github.com/cardano-foundation/cardano-wallet/pull/9999' "$output"; then
        fail_case "$name" "missing created URL; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/create.args" ]]; then
        fail_case "$name" "gh pr create was not invoked"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$tag" == "v$today" ]]; then
        fail_case "$name" "fixture tag unexpectedly matches calendar date"
        rm -rf -- "$work"
        return 0
    fi
    if ! has_arg_pair "$work/create.args" --repo "$repo" \
        || ! has_arg_pair "$work/create.args" --base master \
        || ! has_arg_pair "$work/create.args" --head "$head_branch" \
        || ! has_arg_pair "$work/create.args" --title "chore(release): merge back $head_branch version bumps" \
        || ! has_arg_pair "$work/create.args" --label Release \
        || ! has_arg_pair "$work/create.args" --label CI/CD \
        || ! has_arg_pair "$work/create.args" --assignee paolino; then
        fail_case "$name" "create args missing required metadata; $(cat -- "$work/create.args")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/create.body" ]] \
        || ! grep -Fq "$tag" "$work/create.body" \
        || ! grep -Fqi drift "$work/create.body"; then
        fail_case "$name" "body must identify the tag and version-drift invariant"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/issue-create.args" ]]; then
        fail_case "$name" "tracking issue was not created"
        rm -rf -- "$work"
        return 0
    fi
    if ! has_arg_pair "$work/issue-create.args" --title "Merge $tag version bumps back to master" \
        || [[ ! -f "$work/issue-create.body" ]] \
        || ! grep -Fq "merge-back-tracking/${tag}" "$work/issue-create.body"; then
        fail_case "$name" "tracking issue missing tag identity; $(cat -- "$work/issue-create.args" 2>/dev/null || true)"
        rm -rf -- "$work"
        return 0
    fi
    if ! assert_closes_issue "$work/create.body" "$created_issue" \
        "$work/event.json" "$work/stripped.body"; then
        fail_case "$name" "PR body must visibly Closes #$created_issue and fail the closing-link check without it"
        rm -rf -- "$work"
        return 0
    fi
    if [[ -f "$work/forbidden.log" ]]; then
        fail_case "$name" "forbidden gh mutation: $(cat -- "$work/forbidden.log")"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_skip_prerelease() {
    local name=prerelease-exclusion
    local work output status
    work=$(mktemp -d)
    output=$work/output
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" release true "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -ne 0 ]]; then
        fail_case "$name" "prerelease must skip successfully, exit=$status; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fqi skip "$output"; then
        fail_case "$name" "prerelease skip omitted skip signal; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if grep -Fqi created "$output" || [[ -f "$work/create.args" ]]; then
        fail_case "$name" "prerelease publication created a merge-back PR"
        rm -rf -- "$work"
        return 0
    fi
    if grep -Eq 'git (fetch|rev-parse)' "$work/git.log"; then
        fail_case "$name" "prerelease must not fetch release identity"
        rm -rf -- "$work"
        return 0
    fi
    if grep -Eq 'gh (pr|issue) ' "$work/gh.log"; then
        fail_case "$name" "prerelease must not query or create PRs or issues"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_mismatch() {
    local name=tag-head-mismatch
    local work output status
    work=$(mktemp -d)
    output=$work/output
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$other_sha \
        run_helper "$work" release false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -eq 0 ]]; then
        fail_case "$name" "mismatch was accepted; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fqi mismatch "$output" && ! grep -Fq "$tag_sha" "$output"; then
        fail_case "$name" "mismatch did not name the identity failure; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ -f "$work/create.args" || -f "$work/issue-create.args" ]]; then
        fail_case "$name" "mismatch created a PR or tracking issue"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_existing() {
    local name=$1
    local state=$2
    local merged_at=$3
    local work output status
    work=$(mktemp -d)
    output=$work/output
    printf '%s\t%s\t%s\t%s\n' \
        5384 "$state" \
        "https://github.com/cardano-foundation/cardano-wallet/pull/5384" \
        "$merged_at" >"$work/pr-list.tsv"
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" release false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -ne 0 ]]; then
        fail_case "$name" "existing $state PR must be idempotent, exit=$status; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fq 'existing: PR #5384' "$output"; then
        fail_case "$name" "missing existing PR signal; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ -f "$work/create.args" || -f "$work/issue-create.args" ]]; then
        fail_case "$name" "existing $state PR was duplicated or grew a tracking issue"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_closed_unmerged() {
    local name=closed-unmerged
    local work output status
    work=$(mktemp -d)
    output=$work/output
    printf '%s\t%s\t%s\t%s\n' \
        5370 CLOSED \
        "https://github.com/cardano-foundation/cardano-wallet/pull/5370" \
        "" >"$work/pr-list.tsv"
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" release false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -eq 0 ]]; then
        fail_case "$name" "closed unmerged PR was accepted; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if ! grep -Fqi closed "$output"; then
        fail_case "$name" "closed unmerged failure was unclear; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ -f "$work/create.args" || -f "$work/issue-create.args" ]]; then
        fail_case "$name" "closed unmerged PR was duplicated or grew a tracking issue"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_issue_reuse() {
    local name=$1
    local state=$2
    local work output status
    work=$(mktemp -d)
    output=$work/output
    printf '%s\t%s\t%s\t%s\n' \
        5400 "$state" \
        "Merge $tag version bumps back to master" \
        "https://github.com/cardano-foundation/cardano-wallet/issues/5400" \
        >"$work/issue-list.tsv"
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" release false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -ne 0 ]]; then
        fail_case "$name" "expected PR create with reused issue, exit=$status; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ -f "$work/issue-create.args" ]]; then
        fail_case "$name" "existing $state tracking issue was duplicated"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/create.body" ]] \
        || ! assert_closes_issue "$work/create.body" 5400 \
            "$work/event.json" "$work/stripped.body"; then
        fail_case "$name" "reused issue was not closed by the PR body"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_ignore_unrelated_issues() {
    local name=ignore-unrelated-issues
    local work output status
    work=$(mktemp -d)
    output=$work/output
    {
        printf '%s\t%s\t%s\t%s\n' \
            5385 OPEN \
            "Create release merge-back PR automatically after publication" \
            "https://github.com/cardano-foundation/cardano-wallet/issues/5385"
        printf '%s\t%s\t%s\t%s\n' \
            5387 OPEN \
            "Merge v2026-08-21 version bumps back to master" \
            "https://github.com/cardano-foundation/cardano-wallet/issues/5387"
    } >"$work/issue-list.tsv"
    FAKE_TAG_SHA=$tag_sha FAKE_BRANCH_SHA=$tag_sha \
        run_helper "$work" release false "$output" "$work/status"
    status=$(cat -- "$work/status")

    if [[ ! -x "$helper" ]]; then
        fail_case "$name" "merge-back helper is absent; expected $helper"
        rm -rf -- "$work"
        return 0
    fi
    if [[ "$status" -ne 0 ]]; then
        fail_case "$name" "expected a tag-specific issue, exit=$status; $(cat -- "$output")"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/issue-create.args" ]]; then
        fail_case "$name" "unrelated issues were reused instead of creating a tag-specific tracker"
        rm -rf -- "$work"
        return 0
    fi
    if [[ ! -f "$work/create.body" ]] \
        || ! assert_closes_issue "$work/create.body" "$created_issue" \
            "$work/event.json" "$work/stripped.body"; then
        fail_case "$name" "PR reused #5385/#5387 or omitted Closes #$created_issue"
        rm -rf -- "$work"
        return 0
    fi
    pass_case "$name"
    rm -rf -- "$work"
}

expect_workflow_wiring() {
    local name=workflow-wiring
    local missing=()

    if [[ ! -f "$workflow" ]]; then
        fail_case "$name" "publish-release workflow is absent"
        return 0
    fi
    grep -Eq '^  merge-back:' "$workflow" || missing+=('merge-back job')
    grep -Fq 'scripts/release/merge-back-pr.sh' "$workflow" \
        || missing+=('helper invocation')
    grep -Fq 'pull-requests: write' "$workflow" \
        || missing+=('pull-requests: write')
    grep -Fq 'issues: write' "$workflow" || missing+=('issues: write')
    grep -Fq 'contents: read' "$workflow" || missing+=('contents: read')
    grep -Fq 'release-candidate/' "$workflow" \
        || missing+=('release-candidate/ head')
    grep -Fq 'github.event.inputs.tag || github.event.release.tag_name' \
        "$workflow" || missing+=('published tag selection')
    if grep -Fq 'date +%Y-%m-%d' "$workflow"; then
        missing+=('workflow uses calendar date')
    fi
    if grep -Eq 'gh pr (merge|review)' "$workflow"; then
        missing+=('workflow requests merge or review')
    fi
    if ((${#missing[@]} > 0)); then
        fail_case "$name" "missing behavior: ${missing[*]}"
        return 0
    fi
    pass_case "$name"
}

expect_create stable-publication release
expect_create manual-recovery workflow_dispatch
expect_skip_prerelease
expect_mismatch
expect_existing existing-open OPEN ""
expect_existing existing-merged MERGED "2026-08-21T00:00:00Z"
expect_closed_unmerged
expect_issue_reuse issue-reuse-open OPEN
expect_issue_reuse issue-reuse-closed CLOSED
expect_ignore_unrelated_issues
expect_workflow_wiring

if [[ "$cases_exercised" -eq 0 || "$failures" -ne 0 ]]; then
    echo "merge-back proof failed: passed=$cases_exercised failed=$failures" >&2
    exit 1
fi
echo "merge-back proof passed: $cases_exercised cases."
