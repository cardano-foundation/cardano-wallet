#!/usr/bin/env bash
# Mechanical gate for PR #5288 (script-witness parity in Transaction.Ledger).
#
# Every subagent must pass this before returning a commit; the
# orchestrator re-runs it before accepting any reviewed slice.
# This file is removed in a final `chore: drop gate.sh` commit
# before the PR is marked ready.
set -euo pipefail

git diff --check

# Whitespace / formatting check (matches CI script).
nix develop --quiet -c scripts/ci/check-code-format.sh

# Build the wallet library tree we are touching, in fast-dev mode.
nix develop --quiet -c cabal build \
    cardano-wallet:lib:cardano-wallet \
    cardano-wallet-unit:unit \
    -O0 -v0

# Run the Shelley transaction unit specs that prove parity.
# Narrow to the modules this PR owns; the full suite is broader than
# the slice and would just slow the gate down for subagents.
nix develop --quiet -c cabal test cardano-wallet-unit:unit \
    -O0 -v0 \
    --test-options '--match="Cardano.Wallet.Shelley.TransactionLedger"'

# hlint over the wallet library tree.
nix develop --quiet -c hlint lib/wallet lib/unit/test/unit/Cardano/Wallet/Shelley
