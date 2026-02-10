#!/usr/bin/env bash
# Collect benchmark CSV artifacts from recent GHA runs and
# produce aggregated history (CSV + SVG charts).
#
# Usage: scripts/gha/benchmark-history.sh
#
# Environment:
#   GH_TOKEN        - GitHub token (set by GHA)
#   SINCE_DATE      - Start date (default: 6 months ago)
#   DATA_DIR        - Directory for downloaded data (default: benchmark-data)
#   CHARTS_DIR      - Output directory for charts + CSV (default: benchmark-history)
#   CURRENT_ARTIFACTS - Path to current run's artifacts (default: current-run-artifacts)

set -euo pipefail

REPO="cardano-foundation/cardano-wallet"
WORKFLOW="Linux Benchmarks"
SINCE_DATE="${SINCE_DATE:-$(date -d '6 months ago' +%Y-%m-%d)}"
DATA_DIR="${DATA_DIR:-benchmark-data}"
CHARTS_DIR="${CHARTS_DIR:-benchmark-history}"
CURRENT_ARTIFACTS="${CURRENT_ARTIFACTS:-current-run-artifacts}"

mkdir -p "$DATA_DIR" "$CHARTS_DIR"

# --- Step 1: Download checkpoint from most recent "Benchmark History" artifact ---

checkpoint_flag=""
echo "Looking for previous Benchmark History artifact..."
latest_history_run=$(
    gh run list \
        --repo "$REPO" \
        --workflow "$WORKFLOW" \
        --branch master \
        --status completed \
        --json databaseId,conclusion \
        --jq '[.[] | select(.conclusion == "success")] | .[0].databaseId // empty' \
    || true
)

if [[ -n "${latest_history_run:-}" ]]; then
    echo "Downloading checkpoint from run $latest_history_run..."
    if gh run download "$latest_history_run" \
        --repo "$REPO" \
        --name "Benchmark History" \
        --dir checkpoint-tmp 2>/dev/null; then
        if [[ -f checkpoint-tmp/benchmark-history.csv ]]; then
            checkpoint_flag="--checkpoint checkpoint-tmp/benchmark-history.csv"
            echo "Checkpoint loaded."
        fi
    else
        echo "No previous Benchmark History artifact found, starting fresh."
    fi
fi

# --- Step 2: List recent successful Linux Benchmarks runs ---

echo "Listing recent benchmark runs since $SINCE_DATE..."
run_ids=$(
    gh run list \
        --repo "$REPO" \
        --workflow "$WORKFLOW" \
        --branch master \
        --status completed \
        --json databaseId,conclusion,createdAt \
        --jq "[.[] | select(.conclusion == \"success\" and .createdAt >= \"${SINCE_DATE}\") | .databaseId] | .[]"
)

# --- Step 3: Download CSV artifacts from each run ---

for run_id in $run_ids; do
    echo "Processing run $run_id..."
    # Get run date
    run_date=$(
        gh run view "$run_id" \
            --repo "$REPO" \
            --json createdAt \
            --jq '.createdAt[:10]'
    )
    day_dir="$DATA_DIR/$run_date"
    if [[ -d "$day_dir" ]]; then
        echo "  Already have data for $run_date, skipping."
        continue
    fi
    mkdir -p "$day_dir"

    # Download all benchmark artifacts for this run
    for artifact_name in "API Benchmark" "Latency Benchmark" "DB Benchmark" "Read-blocks Benchmark" "Memory Benchmark"; do
        if gh run download "$run_id" \
            --repo "$REPO" \
            --name "$artifact_name" \
            --dir "$day_dir" 2>/dev/null; then
            echo "  Downloaded: $artifact_name"
        fi
    done
done

# --- Step 4: Copy current run's artifacts into today's dir ---

today=$(date +%Y-%m-%d)
today_dir="$DATA_DIR/$today"
mkdir -p "$today_dir"

if [[ -d "$CURRENT_ARTIFACTS" ]]; then
    echo "Copying current run artifacts to $today_dir..."
    find "$CURRENT_ARTIFACTS" -name '*-bench-results.csv' \
        -exec cp {} "$today_dir/" \;
fi

# --- Step 5: Run benchmark-history ---

echo "Running benchmark-history..."
# shellcheck disable=SC2086
benchmark-history \
    --since "$SINCE_DATE" \
    --data-dir "$DATA_DIR" \
    --charts-dir "$CHARTS_DIR" \
    $checkpoint_flag

echo "Done. Output in $CHARTS_DIR/"
