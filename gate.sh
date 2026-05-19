#!/usr/bin/env bash
# Mechanical gate for PR #5285 (unsigned Shelley tx ledger builder).
#
# Every worker must pass this before returning a commit; the orchestrator
# re-runs it before accepting any reviewed slice. This file is removed in a
# final `chore: drop gate.sh` commit before the PR is marked ready.
set -euo pipefail

git diff --check

base="$(git merge-base origin/master HEAD)"
if git diff --name-only "$base"...HEAD | rg -q '^lib/integration/'; then
    echo "FAIL: #5285 must not modify lib/integration/**"
    git diff --name-only "$base"...HEAD | rg '^lib/integration/'
    exit 1
fi

# Whitespace / formatting check (matches CI script).
nix develop --quiet -c scripts/ci/check-code-format.sh

# Build the wallet library tree we are touching, in fast-dev mode.
nix develop --quiet -c cabal build \
    cardano-wallet:lib:cardano-wallet \
    cardano-wallet-unit:unit \
    -O0 -v0

# Run the Shelley transaction unit specs covering the migrated unsigned path.
nix develop --quiet -c cabal test cardano-wallet-unit:unit \
    -O0 -v0 \
    --test-options '--match="Cardano.Wallet.Shelley.Transaction"'

# hlint over the wallet library tree and touched Shelley unit tests.
nix develop --quiet -c hlint lib/wallet lib/unit/test/unit/Cardano/Wallet/Shelley
