#!/usr/bin/env bash
# Mechanical gate for PR: fix macOS CI LOCAL_CLUSTER_CONFIGS resolution (#5328).
#
# This PR changes CI workflow YAML only. There is no Haskell to build, and the
# macOS self-hosted runner cannot be exercised locally, so the gate is static:
# YAML well-formedness, the ticket's specific structural invariants, and a
# parity check against ci.yml (Linux), which is the known-green baseline.
set -euo pipefail

cd "$(dirname "$0")"

MAC=.github/workflows/macos-unit-tests.yml
LINUX=.github/workflows/ci.yml

fail() { printf 'GATE FAIL: %s\n' "$*" >&2; exit 1; }

echo "== whitespace/conflict markers =="
git diff --check

echo "== workflow YAML well-formedness =="
for wf in .github/workflows/*.yml; do
    yq -e 'has("jobs")' "$wf" >/dev/null || fail "$wf does not parse as a workflow"
done
echo "all workflow files parse"

echo "== #5328 invariant: no cluster-config env leaks into the unit-tests job =="
# Env.hs (localClusterConfigsFromEnv) resolves LOCAL_CLUSTER_CONFIGS relative to
# the process CWD when the variable is set. The wallet-unit shards run with
# working-directory: lib/unit, so a repo-root-relative value doubles the prefix
# to lib/unit/lib/local-cluster/... and the faucet-addrs lookup fails. The
# unset-fallback ("../local-cluster/test/data/cluster-configs") is already
# correct for CWD=lib/unit, so the job must not set the variable at all.
[ "$(yq -r '.jobs["unit-tests"].env.LOCAL_CLUSTER_CONFIGS // "null"' "$MAC")" = null ] \
    || fail "$MAC still sets a job-level LOCAL_CLUSTER_CONFIGS on unit-tests"

[ "$(yq -r '[.jobs["unit-tests"].steps[].env.LOCAL_CLUSTER_CONFIGS | select(. != null)] | length' "$MAC")" = 0 ] \
    || fail "$MAC sets LOCAL_CLUSTER_CONFIGS on a unit-tests step"

echo "== #5328 invariant: wallet-unit shards still run from lib/unit =="
# The CWD-relative default is only correct from lib/unit; if a wallet-unit shard
# loses its working-directory, dropping the env var would break it instead.
bad=$(yq -r '
  .jobs["unit-tests"].strategy.matrix.include[]
  | select(.name | test("^wallet-unit / "))
  | select(.["working-directory"] != "lib/unit")
  | .name' "$MAC")
[ -z "$bad" ] || fail "wallet-unit shard(s) not running from lib/unit: $bad"

echo "== parity with the green Linux baseline (ci.yml) =="
[ "$(yq -r '.jobs["unit-tests"].env.LOCAL_CLUSTER_CONFIGS // "null"' "$LINUX")" = null ] \
    || fail "$LINUX unit-tests grew a job-level LOCAL_CLUSTER_CONFIGS; baseline assumption broken"

echo "== commit message gate =="
commit_gate() {
    local sha="${1:?usage: commit_gate <sha>}"
    local subject body
    subject=$(git show -s --format=%s "$sha")
    body=$(git show -s --format=%b "$sha" | sed '/^[[:space:]]*$/d')

    case "$subject" in
        [Ww][Ii][Pp]*|draft*|Draft*|tmp*|Tmp*|temp*|Temp*|fixup!*|squash!*)
            echo "bad subject: $subject"; return 1 ;;
    esac

    printf '%s\n' "$subject" \
        | grep -Eq '^(feat|fix|docs|test|refactor|perf|build|ci|chore|style|revert)(\([^)]+\))?!?: .+' \
        || { echo "subject is not an approved Conventional Commit"; return 1; }

    [ -n "$body" ] || { echo "commit body is empty"; return 1; }

    case "$subject" in
        chore*|docs*|build*|ci*|style*|revert*) ;;
        *)
            printf '%s\n' "$body" \
                | grep -Eq '^Tasks:[[:space:]]*T[0-9]+([[:space:]]*,[[:space:]]*T[0-9]+)*[[:space:]]*$' \
                || { echo "commit body missing 'Tasks: T###[, T###]' trailer"; return 1; }
            ;;
    esac
}

base=$(git merge-base origin/master HEAD)
rc=0
while read -r sha; do
    if ! commit_gate "$sha"; then
        printf '  ^ %s %s\n' "${sha:0:7}" "$(git show -s --format=%s "$sha")"
        rc=1
    fi
done < <(git rev-list --reverse "$base..HEAD")
[ "$rc" -eq 0 ] || fail "one or more commits fail the message gate"

echo "GATE PASS"
