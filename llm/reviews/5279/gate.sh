#!/usr/bin/env bash
# shellcheck shell=bash

set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

bash -n scripts/ci/bench-restore.sh
bash -n run/common/nix/snapshot.sh
./scripts/shellcheck.sh scripts/ci/bench-restore.sh run/common/nix/snapshot.sh
nix build --quiet .#ci.benchmarks.restore
