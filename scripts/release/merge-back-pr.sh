#!/usr/bin/env bash
# shellcheck shell=bash
#
# Validate published-tag identity, ensure one tag-specific tracking
# issue, and ensure one merge-back PR exists. GitHub mutation is
# gh issue create and gh pr create only when those records are absent.

set -euo pipefail

GIT=${MERGE_BACK_GIT:-git}
GH=${MERGE_BACK_GH:-gh}
EVENT_NAME=${GITHUB_EVENT_NAME:-}
PRERELEASE=${MERGE_BACK_PRERELEASE:-false}
REPO=${GITHUB_REPOSITORY:-}
TAG=${TAG:-}
BASE_BRANCH=${MERGE_BACK_BASE_BRANCH:-master}
HEAD_BRANCH=
EXPECTED_ISSUE_TITLE=
ISSUE_NUMBER=

skip_prerelease_if_needed() {
    if [[ "$EVENT_NAME" == release ]]; then
        case "$PRERELEASE" in
            true | True | TRUE | yes | 1)
                echo "skip: prerelease publication does not create a merge-back PR"
                exit 0
                ;;
        esac
    elif [[ "$EVENT_NAME" != workflow_dispatch ]]; then
        echo "error: unsupported event: ${EVENT_NAME:-<empty>}" >&2
        exit 1
    fi
}

require_identity_inputs() {
    if [[ -z "$TAG" ]]; then
        echo "error: TAG is required; use the published tag, not the calendar date" >&2
        exit 1
    fi
    if [[ -z "$REPO" ]]; then
        echo "error: GITHUB_REPOSITORY is required" >&2
        exit 1
    fi
    HEAD_BRANCH=${MERGE_BACK_HEAD_BRANCH:-release-candidate/${TAG}}
    if [[ "$HEAD_BRANCH" != "release-candidate/${TAG}" ]]; then
        echo "error: head branch ${HEAD_BRANCH} is not release-candidate/${TAG}" >&2
        exit 1
    fi
    EXPECTED_ISSUE_TITLE="Merge ${TAG} version bumps back to master"
}

fetch_and_validate_identity() {
    local tag_sha branch_sha
    "$GIT" fetch --quiet origin "+refs/tags/${TAG}:refs/tags/${TAG}"
    "$GIT" fetch --quiet origin \
        "+refs/heads/${HEAD_BRANCH}:refs/remotes/origin/${HEAD_BRANCH}"
    tag_sha=$("$GIT" rev-parse --verify "${TAG}^{commit}")
    branch_sha=$("$GIT" rev-parse --verify "origin/${HEAD_BRANCH}")
    if [[ "$tag_sha" != "$branch_sha" ]]; then
        echo "error: tag ${TAG} peeled to ${tag_sha} but ${HEAD_BRANCH} is at ${branch_sha} (mismatch)" >&2
        exit 1
    fi
    echo "identity: tag=${TAG} sha=${tag_sha} head=${HEAD_BRANCH}"
}

is_merged_timestamp() {
    local merged_at=$1
    [[ -n "$merged_at" && "$merged_at" != null && "$merged_at" != NULL ]]
}

ensure_pr() {
    local list existing_number existing_state existing_url closed_number
    local number state url merged_at state_u
    list=$("$GH" pr list \
        --repo "$REPO" \
        --base "$BASE_BRANCH" \
        --head "$HEAD_BRANCH" \
        --state all \
        --json number,state,url,mergedAt \
        --jq '.[] | [.number, .state, .url, (.mergedAt // "")] | @tsv')
    existing_number=
    existing_state=
    existing_url=
    closed_number=
    if [[ -n "$list" ]]; then
        while IFS=$'\t' read -r number state url merged_at; do
            state_u=$(printf '%s' "$state" | tr '[:lower:]' '[:upper:]')
            case "$state_u" in
                OPEN | MERGED)
                    existing_number=$number
                    existing_state=$state_u
                    existing_url=$url
                    break
                    ;;
                CLOSED)
                    if is_merged_timestamp "${merged_at:-}"; then
                        existing_number=$number
                        existing_state=MERGED
                        existing_url=$url
                        break
                    fi
                    closed_number=$number
                    ;;
                *)
                    echo "error: unexpected PR state '${state}' for #${number}" >&2
                    exit 1
                    ;;
            esac
        done <<<"$list"
    fi
    if [[ -n "$existing_number" ]]; then
        echo "existing: PR #${existing_number} (${existing_state}) ${existing_url}"
        return 0
    fi
    if [[ -n "$closed_number" ]]; then
        echo "error: closed unmerged merge-back PR #${closed_number} for ${HEAD_BRANCH}; not creating a duplicate" >&2
        exit 1
    fi
    ensure_tracking_issue
    create_pr
}

list_issues() {
    local query=$1
    "$GH" issue list \
        --repo "$REPO" \
        --state all \
        --limit 50 \
        --search "$query" \
        --json number,state,title,url \
        --jq '.[] | [.number, .state, .title, .url] | @tsv'
}

select_matching_issue() {
    local list=$1
    local number state title url
    [[ -n "$list" ]] || return 1
    while IFS=$'\t' read -r number state title url; do
        if [[ "$title" == "$EXPECTED_ISSUE_TITLE" ]]; then
            ISSUE_NUMBER=$number
            echo "existing-issue: #${ISSUE_NUMBER} (${state}) ${url}"
            return 0
        fi
    done <<<"$list"
    return 1
}

find_tracking_issue() {
    local list
    ISSUE_NUMBER=
    list=$(list_issues "$EXPECTED_ISSUE_TITLE")
    select_matching_issue "$list" && return 0
    list=$(list_issues "merge-back-tracking/${TAG}")
    select_matching_issue "$list" && return 0
    return 0
}

create_tracking_issue() {
    local body_file url
    body_file=$(mktemp)
    cat >"$body_file" <<EOF
## Goal

Merge the already-published \`${TAG}\` release-candidate version
metadata back into \`${BASE_BRANCH}\` so the next release run does
not fail its version-drift guard.

merge-back-tracking/${TAG}
EOF
    url=$("$GH" issue create \
        --repo "$REPO" \
        --title "$EXPECTED_ISSUE_TITLE" \
        --body-file "$body_file" \
        --label Release \
        --label CI/CD)
    rm -f -- "$body_file"
    ISSUE_NUMBER=${url##*/}
    ISSUE_NUMBER=${ISSUE_NUMBER//$'\r'/}
    ISSUE_NUMBER=${ISSUE_NUMBER//$'\n'/}
    if [[ ! "$ISSUE_NUMBER" =~ ^[1-9][0-9]*$ ]]; then
        echo "error: issue create did not return a number: ${url}" >&2
        exit 1
    fi
    echo "created-issue: #${ISSUE_NUMBER} ${url}"
}

ensure_tracking_issue() {
    find_tracking_issue
    if [[ -n "$ISSUE_NUMBER" ]]; then
        return 0
    fi
    create_tracking_issue
}

create_pr() {
    local body_file title url
    if [[ ! "$ISSUE_NUMBER" =~ ^[1-9][0-9]*$ ]]; then
        echo "error: tracking issue number is missing" >&2
        exit 1
    fi
    body_file=$(mktemp)
    title="chore(release): merge back ${HEAD_BRANCH} version bumps"
    cat >"$body_file" <<EOF
## Summary

Merge the release automation's version updates from
\`${HEAD_BRANCH}\` back into \`${BASE_BRANCH}\` after publishing
\`${TAG}\`.

This keeps \`${BASE_BRANCH}\` aligned with the latest stable
cardano-wallet release and prevents the next scheduled Release
workflow from stopping on its cabal-version drift guard.

Closes #${ISSUE_NUMBER}
EOF
    url=$("$GH" pr create \
        --repo "$REPO" \
        --base "$BASE_BRANCH" \
        --head "$HEAD_BRANCH" \
        --title "$title" \
        --body-file "$body_file" \
        --label Release \
        --label CI/CD \
        --assignee paolino)
    rm -f -- "$body_file"
    echo "created: ${url}"
}

skip_prerelease_if_needed
require_identity_inputs
fetch_and_validate_identity
ensure_pr
