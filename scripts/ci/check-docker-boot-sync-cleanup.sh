#!/usr/bin/env bash
# shellcheck shell=bash
# shellcheck disable=SC2016

set -euo pipefail

compose_file="run/common/docker/docker-compose.yml"
run_script="run/common/docker/run.sh"
workflow_file=".github/workflows/ci.yml"

failed=0

require_literal() {
    local file=$1
    local literal=$2
    local description=$3

    if ! grep -Fq -- "$literal" "$file"; then
        echo "Missing ${description}: ${literal}"
        failed=1
    fi
}

reject_literal() {
    local file=$1
    local literal=$2
    local description=$3

    if grep -Fq -- "$literal" "$file"; then
        echo "Forbidden ${description}: ${literal}"
        failed=1
    fi
}

restart_count=$(
    grep -F -c 'restart: "${DOCKER_RESTART_POLICY:-on-failure}"' \
        "$compose_file" || true
)

if [[ "$restart_count" -ne 2 ]]; then
    echo "Expected two configurable restart policies in ${compose_file}; found ${restart_count}."
    failed=1
fi

require_literal "$run_script" \
    'docker compose -p "$COMPOSE_PROJECT_NAME" down --remove-orphans --timeout 10' \
    "compose orphan cleanup"
require_literal "$run_script" 'trap cleanup EXIT' "EXIT cleanup trap"
require_literal "$run_script" "trap 'exit 130' INT" "INT cleanup trap"
require_literal "$run_script" "trap 'exit 143' TERM" "TERM cleanup trap"

require_literal "$workflow_file" 'DOCKER_RESTART_POLICY: "no"' \
    "CI restart-policy disable"
require_literal "$workflow_file" 'BOOT_SYNC_DIR: ${{ runner.temp }}/cardano-wallet/${{ github.run_id }}-${{ strategy.job-index }}' \
    "runner-temp boot-sync directory"
require_literal "$workflow_file" 'NODE_DB: ${{ runner.temp }}/cardano-wallet/${{ github.run_id }}-${{ strategy.job-index }}/node-db' \
    "runner-temp node database"
require_literal "$workflow_file" 'WALLET_DB: ${{ runner.temp }}/cardano-wallet/${{ github.run_id }}-${{ strategy.job-index }}/wallet-db' \
    "runner-temp wallet database"
require_literal "$workflow_file" 'NODE_SOCKET_DIR: ${{ runner.temp }}/cardano-wallet/${{ github.run_id }}-${{ strategy.job-index }}/ipc' \
    "runner-temp IPC directory"
require_literal "$workflow_file" 'if: always()' "always-run cleanup step"
require_literal "$workflow_file" \
    'docker compose -p "$COMPOSE_PROJECT_NAME" down --remove-orphans --timeout 10 || true' \
    "workflow compose cleanup"
reject_literal "$workflow_file" 'rm -rf databases' \
    "checkout-local boot-sync database cleanup"

if [[ "$failed" -ne 0 ]]; then
    exit 1
fi

echo "Docker boot-sync cleanup contract passed."
