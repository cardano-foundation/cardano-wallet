#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

if [ "$#" -lt 3 ]; then
  echo "usage: $0 NETWORK BENCH NODE_DB"
  exit 1
fi

network=$1
bench=$2
node_db=$3

if [ "$network" != "mainnet" ]; then
  echo "FAILED - Mithril-provisioned restoration benchmarks require mainnet" >&2
  exit 64
fi

repo_root=$(pwd)
artifact_name=restore-$network
log=restore.log
results=restore-$network.txt
total_time=restore-time.txt
provenance_file=restore-provenance-"$bench".json
node_log_artifact=node-"$bench".log

# Workflow-provided phase budgets. Defaults keep manual runs aligned with the
# scheduled restoration benchmark workflow.
setup_timeout_seconds=${SETUP_TIMEOUT_SECONDS:-7200}
benchmark_timeout_seconds=${BENCHMARK_TIMEOUT_SECONDS:-43200}
sync_poll_interval_seconds=${SYNC_POLL_INTERVAL_SECONDS:-15}
sync_stale_seconds=${SYNC_STALE_SECONDS:-1200}

tmp_base=${TMPDIR:-/tmp}
work_dir="$tmp_base/bench/restore/$bench"
node_socket="$work_dir/node.socket"
node_log="$work_dir/node.log"
tip_file="$work_dir/node-tip.json"
tip_error_file="$work_dir/node-tip.err"
mithril_info_file="$work_dir/mithril-snapshot.json"
mithril_stage_file="$work_dir/mithril-stage"

node_pid=""
failure_stage=""
setup_started_at=""
setup_finished_at=""
benchmark_started_at=""
node_sync_completed_at=""
node_sync_wait_seconds=""
node_sync_progress=""
final_tip_json="null"
mithril_snapshot_hash=""
mithril_snapshot_tip="null"
mithril_client_source="github:input-output-hk/mithril?ref=2543.1-hotfix"
mithril_client_version=""

iso_now() {
    date -u +%Y-%m-%dT%H:%M:%SZ
}

epoch_now() {
    date +%s
}

wipe_directory_contents() {
    local dir=$1
    if [[ -z "$dir" || "$dir" == "/" ]]; then
        echo "Refusing to wipe unsafe node DB path: $dir" >&2
        exit 1
    fi
    mkdir -p "$dir"
    (
        shopt -s dotglob nullglob
        rm -rf -- "${dir:?}"/*
    )
}

write_provenance() {
    if ! command -v jq >/dev/null 2>&1; then
        return 0
    fi

    jq -n \
        --arg workflow_run_id "${GITHUB_RUN_ID:-}" \
        --arg bench_name "$bench" \
        --arg network "$network" \
        --arg node_db_path "$node_db" \
        --arg node_socket_path "$node_socket" \
        --arg node_log_path "$node_log" \
        --arg mithril_snapshot_hash "$mithril_snapshot_hash" \
        --arg mithril_client_source "$mithril_client_source" \
        --arg mithril_client_version "$mithril_client_version" \
        --arg setup_started_at "$setup_started_at" \
        --arg setup_finished_at "$setup_finished_at" \
        --arg node_sync_completed_at "$node_sync_completed_at" \
        --arg node_sync_wait_seconds "$node_sync_wait_seconds" \
        --arg benchmark_started_at "$benchmark_started_at" \
        --arg failure_stage "$failure_stage" \
        --argjson final_tip "$final_tip_json" \
        --argjson mithril_snapshot_tip "$mithril_snapshot_tip" \
        '{
          workflow_run_id: $workflow_run_id,
          bench_name: $bench_name,
          network: $network,
          node_db_path: $node_db_path,
          node_socket_path: $node_socket_path,
          node_log_path: $node_log_path,
          mithril_snapshot_hash: $mithril_snapshot_hash,
          mithril_snapshot_tip: $mithril_snapshot_tip,
          mithril_client_source: $mithril_client_source,
          mithril_client_version: $mithril_client_version,
          setup_started_at: $setup_started_at,
          setup_finished_at: $setup_finished_at,
          node_sync_completed_at: $node_sync_completed_at,
          node_sync_wait_seconds: $node_sync_wait_seconds,
          benchmark_started_at: $benchmark_started_at,
          final_tip: $final_tip,
          failure_stage: $failure_stage
        }' >"$provenance_file" || true
}

cleanup() {
    status=$?

    if [[ -n "$node_pid" ]] && kill -0 "$node_pid" 2>/dev/null; then
        kill "$node_pid" 2>/dev/null || true
        wait "$node_pid" 2>/dev/null || true
    fi

    if [[ -f "$node_log" ]]; then
        cp "$node_log" "$node_log_artifact" 2>/dev/null || true
    fi

    if [[ $status -ne 0 && -n "$failure_stage" ]]; then
        echo "FAILURE_STAGE=$failure_stage"
    fi

    write_provenance
}
trap cleanup EXIT

fail_stage() {
    failure_stage=$1
    exit 1
}

remaining_setup_seconds() {
    local now
    now=$(epoch_now)
    echo $((setup_deadline - now))
}

require_setup_time() {
    local remaining
    remaining=$(remaining_setup_seconds)
    if (( remaining <= 0 )); then
        fail_stage "$1"
    fi
    echo "$remaining"
}

echo "--- Build"
nix build --quiet .#ci.benchmarks.restore -o bench-restore

mkdir -p "$work_dir"
mkdir -p "$node_db"

setup_started_at=$(iso_now)
setup_start_epoch=$(epoch_now)
setup_deadline=$((setup_start_epoch + setup_timeout_seconds))

echo "--- Setup restore benchmark node - $network/$bench"
echo "BENCH_NAME=$bench"
echo "NETWORK=$network"
echo "NODE_DB=$node_db"
echo "NODE_SOCKET_PATH=$node_socket"
echo "NODE_LOG_PATH=$node_log"
echo "SETUP_STARTED_AT=$setup_started_at"

echo "--- Provision Mithril snapshot"
wipe_directory_contents "$node_db"

mithril_remaining=$(require_setup_time "setup:mithril-list")
set +e
timeout --foreground --kill-after=60s "$mithril_remaining" \
    env \
      NODE_DB="$node_db" \
      MITHRIL_SNAPSHOT_INFO_FILE="$mithril_info_file" \
      MITHRIL_STAGE_FILE="$mithril_stage_file" \
      "$repo_root/run/mainnet/nix/snapshot.sh"
mithril_status=$?
set -e

if (( mithril_status != 0 )); then
    if [[ -s "$mithril_stage_file" ]]; then
        failure_stage=$(<"$mithril_stage_file")
    else
        failure_stage="setup:mithril-list"
    fi
    fail_stage "$failure_stage"
fi

if [[ -s "$mithril_info_file" ]]; then
    mithril_snapshot_hash=$(jq -r '.snapshot_hash // empty' "$mithril_info_file")
    mithril_snapshot_tip=$(jq -c '.snapshot_tip // null' "$mithril_info_file")
    mithril_client_source=$(jq -r '.client_source // empty' "$mithril_info_file")
    mithril_client_version=$(jq -r '.client_version // empty' "$mithril_info_file")
fi

echo "MITHRIL_CLIENT_SOURCE=$mithril_client_source"
echo "MITHRIL_CLIENT_VERSION=$mithril_client_version"
echo "MITHRIL_SNAPSHOT_HASH=$mithril_snapshot_hash"
echo "MITHRIL_SNAPSHOT_TIP=$mithril_snapshot_tip"

echo "--- Start cardano-node"
CARDANO_NODE_CONFIGS="$repo_root/configs/cardano"
node_config_dir="$CARDANO_NODE_CONFIGS/$network"

cardano-node run \
    --topology "$node_config_dir/topology.json" \
    --database-path "$node_db" \
    --socket-path "$node_socket" \
    --config "$node_config_dir/config.json" \
    +RTS -N -A16m -qg -qb -RTS >"$node_log" 2>&1 &
node_pid=$!
echo "NODE_PID=$node_pid"

echo "--- Wait for node sync"
last_progress=""
last_slot=""
last_change_epoch=0
first_valid_tip=false

while true; do
    require_setup_time "setup:node-sync" >/dev/null

    set +e
    cardano-cli query tip --mainnet --socket-path "$node_socket" \
        >"$tip_file" 2>"$tip_error_file"
    query_status=$?
    set -e

    if (( query_status != 0 )); then
        if ! kill -0 "$node_pid" 2>/dev/null; then
            if [[ "$first_valid_tip" == true ]]; then
                fail_stage "setup:node-sync"
            else
                fail_stage "setup:node-start"
            fi
        fi
        sleep "$sync_poll_interval_seconds"
        continue
    fi

    first_valid_tip=true
    progress=$(jq -r '.syncProgress // empty' "$tip_file" | tr -d '%')
    slot=$(jq -r '.slot // empty' "$tip_file")
    final_tip_json=$(jq -c . "$tip_file")

    if [[ -z "$progress" ]]; then
        fail_stage "setup:node-sync"
    fi

    node_sync_progress=$progress
    echo "NODE_SYNC_PROGRESS=$node_sync_progress"

    if awk -v progress="$progress" 'BEGIN { exit !(progress + 0 >= 99.9) }'; then
        node_sync_completed_at=$(iso_now)
        node_sync_wait_seconds=$(( $(epoch_now) - setup_start_epoch ))
        setup_finished_at=$node_sync_completed_at
        echo "NODE_SYNC_COMPLETED_AT=$node_sync_completed_at"
        echo "NODE_SYNC_WAIT_SECONDS=$node_sync_wait_seconds"
        break
    fi

    now=$(epoch_now)
    if [[ "$progress" != "$last_progress" || "$slot" != "$last_slot" ]]; then
        last_progress=$progress
        last_slot=$slot
        last_change_epoch=$now
    elif (( last_change_epoch > 0 && now - last_change_epoch >= sync_stale_seconds )); then
        fail_stage "setup:node-sync"
    fi

    sleep "$sync_poll_interval_seconds"
done

echo "--- Run benchmarks - $network"
benchmark_started_at=$(iso_now)
echo "BENCHMARK_STARTED_AT=$benchmark_started_at"

BENCH_CMD=(
    ./bench-restore/bin/restore
    "$network"
    --running-node "$node_socket"
    --bench-name "$bench"
    --cardano-node-configs "$CARDANO_NODE_CONFIGS"
    --to-tip-timeout 1
    +RTS -N -qg -A1m -I0 -T -M16G -hT -RTS
)

printf '%q ' "${BENCH_CMD[@]}"
printf '\n'

set +e
command time -o "$total_time" -v \
    timeout --foreground --kill-after=60s "$benchmark_timeout_seconds" \
    "${BENCH_CMD[@]}" 2>&1 | tee "$log"
bench_status=${PIPESTATUS[0]}
set -e

grep -v INFO "$log" 2>/dev/null | awk '/All results/,EOF { print $0 }' >"$results" || true

echo "+++ Results - $network"
cat "$results"

echo "+++ Memory Summary - $network"

# time -v reports Maximum resident set size (kB) — this includes
# all child processes (notably cardano-node which uses ~11 GB).
time_v_rss=$(grep "Maximum resident set size" "$total_time" 2>/dev/null \
    | awk '{print $NF}' || true)
if [ -n "$time_v_rss" ]; then
    time_v_rss_mb=$(awk "BEGIN {printf \"%.1f\", $time_v_rss / 1024}")
    echo "  Total peak RSS (incl. cardano-node): ${time_v_rss_mb} MB  [from time -v]"
fi

# The wallet binary now self-reports its own RSS via /proc/self/status.
wallet_rss=$(grep "Wallet peak RSS:" "$log" 2>/dev/null | head -1 || true)
if [ -n "$wallet_rss" ]; then
    echo "  $wallet_rss"
fi

# GHC RTS stats from the benchmark (requires -T RTS flag).
rts_section=$(sed -n '/=== GHC RTS Memory Stats ===/,/^$/p' "$log" 2>/dev/null || true)
if [ -n "$rts_section" ]; then
    echo "$rts_section"
fi

if [[ -f restore.hp ]]; then
    mv restore.hp "$artifact_name".hp
    hp2pretty "$artifact_name".hp
fi

if (( bench_status != 0 )); then
    fail_stage "benchmark:restore"
fi

if [ -z "$(cat "$results")" ]; then
  echo "+++ Bad news"
  echo "FAILED - Missing results" >/dev/stderr
  fail_stage "benchmark:restore"
fi
