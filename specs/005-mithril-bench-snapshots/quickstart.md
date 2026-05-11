# Quickstart: Mithril-Provisioned Restoration Benchmark Runs

**Feature Branch**: `005-mithril-bench-snapshots`
**Date**: 2026-05-11

## Local Static Checks

Run from `/code/cardano-wallet-issue-5278`:

```bash
bash -n scripts/ci/bench-restore.sh
```

```bash
nix shell --quiet nixpkgs#shellcheck -c shellcheck scripts/ci/bench-restore.sh run/common/nix/snapshot.sh
```

```bash
nix build --quiet .#ci.benchmarks.restore
```

## Local Dry Run

Use this only to validate argument handling and early setup logging without a
full mainnet run:

```bash
TMPDIR="${TMPDIR:-/tmp}" \
SETUP_TIMEOUT_SECONDS=60 \
BENCHMARK_TIMEOUT_SECONDS=60 \
SYNC_STALE_SECONDS=30 \
bash scripts/ci/bench-restore.sh mainnet base "$TMPDIR/cardano-wallet-restore-dry-run-node-db"
```

Expected dry-run outcome on a normal developer machine is a setup failure if
network, disk, or runner resources are not available. The useful signal is that
the log classifies the failure as `setup:<stage>` and preserves the node DB
path rather than silently falling through to an existing database.

## Failure-Stage Smokes

The setup-timeout smoke can run on a normal developer machine. It proves that a
stale per-leg node database is wiped before Mithril provisioning and that setup
failure is classified:

```bash
dry_db=$(mktemp -d)
dry_tmp=$(mktemp -d)
touch "$dry_db/stale-marker"
TMPDIR="$dry_tmp" \
SETUP_TIMEOUT_SECONDS=1 \
BENCHMARK_TIMEOUT_SECONDS=1 \
SYNC_STALE_SECONDS=1 \
bash scripts/ci/bench-restore.sh mainnet base "$dry_db"
test ! -e "$dry_db/stale-marker"
rm -rf "$dry_db" "$dry_tmp"
```

Expected log fields:

```text
BENCH_NAME=base
NODE_DB=<temporary path>
SETUP_STARTED_AT=<timestamp>
FAILURE_STAGE=setup:mithril-list
```

The node-sync and benchmark-timeout smokes require a benchmark runner because
they need a downloaded Mithril snapshot and a started mainnet node:

```bash
TMPDIR="${TMPDIR:-/tmp}" \
SETUP_TIMEOUT_SECONDS=7200 \
BENCHMARK_TIMEOUT_SECONDS=60 \
SYNC_STALE_SECONDS=30 \
bash scripts/ci/bench-restore.sh mainnet base "$HOME/databases/node/mainnet-smoke"
```

For node-sync validation, temporarily lower `SYNC_STALE_SECONDS` on the runner
or interrupt node progress after startup; expected classification is
`FAILURE_STAGE=setup:node-sync`. For benchmark-timeout validation, use a very
small `BENCHMARK_TIMEOUT_SECONDS` after setup has completed; expected
classification is `FAILURE_STAGE=benchmark:restore`.

## End-to-End Workflow Check

After implementation is pushed to the draft PR branch, trigger the workflow:

```bash
gh workflow run "Restoration Benchmarks" --ref 005-mithril-bench-snapshots
```

Watch the run:

```bash
gh run list --workflow "Restoration Benchmarks" --branch 005-mithril-bench-snapshots --limit 5
```

For the successful run, confirm each matrix leg log contains:

```text
MITHRIL_SNAPSHOT_HASH=
MITHRIL_CLIENT_VERSION=
NODE_DB=
NODE_SYNC_COMPLETED_AT=
NODE_SYNC_WAIT_SECONDS=
BENCHMARK_STARTED_AT=
```

Confirm each matrix leg artifact contains the existing benchmark outputs:

```text
restore-mainnet.txt
restore-time.txt
restore-mainnet.hp
restore-mainnet.svg
```

and the new provenance JSON artifact.

## PR Evidence

Before marking the PR ready for review, add a PR comment or description update
with:

```text
Restoration Benchmarks workflow: <workflow-run-url>
Evidence: all four variants started after Mithril-provisioned node sync and uploaded normal artifacts.
```

Also rerun any unrelated flaky CI job once the full workflow has finished, then
wait for the PR checks to settle before requesting review.
