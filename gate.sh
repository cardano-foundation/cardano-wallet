#!/usr/bin/env bash
# Mechanical gate for issue #5309 (cardano-wallet-unit memory leak).
#
# Every implementation worker must pass this before returning a commit; the
# orchestrator re-runs it before accepting a reviewed slice. This file is
# removed in a final `chore: drop gate.sh` commit before the PR is marked ready.
set -euo pipefail

git diff --check

# Whitespace / formatting check (matches CI).
nix develop --quiet -c scripts/ci/check-code-format.sh

# Keep the affected suite buildable without running the known OOM reproducer.
# A bounded residency regression proof will be added after the leak is
# localized and its safe invocation is specified.
nix develop --quiet -c cabal build cardano-wallet-unit:unit -O0 -v0
