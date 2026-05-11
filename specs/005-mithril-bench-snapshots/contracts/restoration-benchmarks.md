# Contract: Restoration Benchmark Workflow

**Feature Branch**: `005-mithril-bench-snapshots`
**Date**: 2026-05-11

## Workflow Interface

**File**: `/code/cardano-wallet-issue-5278/.github/workflows/restoration-benchmarks.yml`

The workflow remains available through:

```yaml
on:
  schedule:
    - cron: "0 0 * * 1"
  workflow_dispatch:
```

The restore job matrix remains:

```yaml
strategy:
  fail-fast: false
  matrix:
    include:
      - bench: base
        node-db: mainnet-1
      - bench: seq0
        node-db: mainnet-2
      - bench: seq1
        node-db: mainnet-3
      - bench: rnd5
        node-db: mainnet-4
```

Each matrix leg must call:

```bash
bash scripts/ci/bench-restore.sh mainnet "$bench" "$HOME/databases/node/$node_db"
```

with these environment values available to the script:

```text
SETUP_TIMEOUT_SECONDS=7200
BENCHMARK_TIMEOUT_SECONDS=43200
SYNC_POLL_INTERVAL_SECONDS=15
SYNC_STALE_SECONDS=1200
TMPDIR=<runner temp directory>
```

## Script Interface

**File**: `/code/cardano-wallet-issue-5278/scripts/ci/bench-restore.sh`

### Arguments

```text
bench-restore.sh NETWORK BENCH NODE_DB
```

- `NETWORK`: must be `mainnet` for Mithril-provisioned restoration benchmarks.
- `BENCH`: one of `base`, `seq0`, `seq1`, `rnd5`.
- `NODE_DB`: absolute or runner-home-relative path dedicated to that matrix leg.

### Exit Classes

The script exits non-zero on failure and prints one of:

```text
FAILURE_STAGE=setup:mithril-list
FAILURE_STAGE=setup:mithril-download
FAILURE_STAGE=setup:mithril-convert
FAILURE_STAGE=setup:mithril-extract
FAILURE_STAGE=setup:node-start
FAILURE_STAGE=setup:node-sync
FAILURE_STAGE=benchmark:restore
```

The script exits zero only after:

1. The node database has been wiped at setup start.
2. A Mithril snapshot has been downloaded and extracted into that database.
3. A per-leg node has reported `syncProgress >= 99.9` through
   `cardano-cli query tip`.
4. The restore benchmark has completed and produced the normal result artifact.

## Required Workflow Log Fields

Each run must print these fields before the benchmark starts:

```text
BENCH_NAME=<base|seq0|seq1|rnd5>
NETWORK=mainnet
NODE_DB=<path>
MITHRIL_CLIENT_SOURCE=github:input-output-hk/mithril?ref=2617.0
MITHRIL_CLIENT_VERSION=<version>
MITHRIL_SNAPSHOT_HASH=<hash>
MITHRIL_UTXO_HD_FLAVOR=LMDB
MITHRIL_CARDANO_NODE_VERSION=<version>
SETUP_STARTED_AT=<ISO-8601 timestamp>
NODE_SYNC_PROGRESS=<numeric percentage>
NODE_SYNC_COMPLETED_AT=<ISO-8601 timestamp>
NODE_SYNC_WAIT_SECONDS=<integer seconds>
BENCHMARK_STARTED_AT=<ISO-8601 timestamp>
```

On setup failure, the log must still include every field known at the time of
failure plus `FAILURE_STAGE=setup:<stage>`.

On benchmark failure, the log must include all setup fields plus
`FAILURE_STAGE=benchmark:restore`.

## Required Artifacts

The workflow upload step must continue accepting the current benchmark artifacts:

```text
*.txt
*.log
*.svg
*.hp
```

It must also accept:

```text
*.json
```

for the per-leg provenance record.

## Readiness Poll Contract

The setup phase polls:

```bash
cardano-cli query tip --mainnet --socket-path "$NODE_SOCKET_PATH"
```

The response is valid for readiness when:

```text
syncProgress_without_percent >= 99.9
```

Polling stops with setup failure when:

- the setup timeout reaches 7200 seconds, or
- valid tip JSON has been observed, readiness is not reached, and both slot and
  sync progress remain unchanged for 1200 seconds.

## Database Lifecycle Contract

For each matrix leg:

```text
previous DB at NODE_DB
  -> wiped at setup start
  -> populated from Mithril
  -> written only by this leg's cardano-node
  -> preserved after success or failure
  -> wiped by the next run for the same leg
```

No end-of-run cleanup may remove `NODE_DB`.
