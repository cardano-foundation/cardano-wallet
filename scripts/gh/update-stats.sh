#!/usr/bin/env bash

set -euo pipefail

source "${BASH_SOURCE%/*}/common.sh"

dest_branch="${1:-}"
eval="${2:-}"
builds="${3:-}"
timings="${4:-}"

if [ -z "$timings" ]; then
  echo "usage: $0 DEST_BRANCH EVAL.json BUILDS.json TIMINGS.json"
  exit 1
fi

check_branch "$dest_branch"

eval_id=$(jq .id "$eval")

# shellcheck disable=SC2154
out="$dir/hydra/eval-$eval_id.json"

mkdir -p "$(dirname "$out")"

jq --slurpfile evals "$eval" --slurpfile builds "$builds" '$evals[0] as $eval | { eval: $eval.id, builds: { required: $builds[0].required|{id,finished,buildstatus,buildmetrics} }, rev: $eval.jobsetevalinputs["cardano-wallet"].revision, timings: . }' "$timings" > "$out"

git add "$out"
commit_and_push "stats: hydra"
