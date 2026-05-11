# Data Model: Mithril-Provisioned Restoration Benchmark Runs

**Feature Branch**: `005-mithril-bench-snapshots`
**Date**: 2026-05-11

## Entities

### RestorationBenchmarkLeg

**Location**: `/code/cardano-wallet-issue-5278/.github/workflows/restoration-benchmarks.yml`

**Fields**:
- `bench`: one of `base`, `seq0`, `seq1`, `rnd5`
- `network`: `mainnet`
- `node_db_name`: stable per-leg database suffix, currently `mainnet-1`
  through `mainnet-4`
- `node_db_path`: absolute runner path, currently
  `$HOME/databases/node/<node_db_name>`

**Validation rules**:
- `bench` must match a benchmark name accepted by `restore --bench-name`.
- `node_db_path` must be unique across all concurrently scheduled matrix legs.
- The path is wiped only at setup start.

**State transitions**:
- `scheduled` -> `setup-running` -> `benchmark-running` -> `succeeded`
- `scheduled` -> `setup-running` -> `setup-failed`
- `scheduled` -> `setup-running` -> `benchmark-running` -> `benchmark-failed`

### MithrilSnapshot

**Location**: selected by `/code/cardano-wallet-issue-5278/run/mainnet/nix/snapshot.sh`

**Fields**:
- `network`: `mainnet`
- `hash`: snapshot hash selected from `mithril-client cdb snapshot list --json`
- `client_source`: pinned Nix source identifier,
  `github:input-output-hk/mithril?ref=2543.1-hotfix`
- `client_version`: `mithril-client --version`
- `tip`: best-effort snapshot tip metadata if exposed by the client JSON

**Validation rules**:
- `hash` must be non-empty before download starts.
- Snapshot download and extraction must complete inside the setup timeout.
- Failure to list, download, or extract a snapshot is a setup failure.

**Relationships**:
- Populates exactly one `PerLegNodeDatabase` for a benchmark leg.
- Appears in the leg's provenance record and workflow logs.

### PerLegNodeDatabase

**Location**: `$HOME/databases/node/<matrix-node-db>` on the self-hosted runner

**Fields**:
- `path`: absolute filesystem path
- `owner_leg`: benchmark leg that may write this database
- `snapshot_hash`: hash of the Mithril snapshot used to populate it
- `created_at`: setup timestamp for the current run

**Validation rules**:
- Only the owning leg may run `cardano-node` against this path.
- The path is removed and recreated before each Mithril provisioning attempt.
- No end-of-run cleanup removes this path.

**State transitions**:
- `previous-run-db` -> `wiped-at-setup-start`
- `wiped-at-setup-start` -> `mithril-populated`
- `mithril-populated` -> `node-writing`
- `node-writing` -> `preserved-for-triage`

### SetupPhase

**Location**: `/code/cardano-wallet-issue-5278/scripts/ci/bench-restore.sh`

**Fields**:
- `started_at`
- `finished_at`
- `timeout_seconds`: `7200`
- `node_socket_path`
- `node_log_path`
- `sync_poll_interval_seconds`: `15`
- `sync_stale_seconds`: `1200`
- `final_sync_progress`
- `final_tip`
- `failure_stage`: one of `mithril-list`, `mithril-download`,
  `mithril-extract`, `node-start`, `node-sync`

**Validation rules**:
- The benchmark phase may not start before `final_sync_progress >= 99.9`.
- The phase must fail non-zero if the setup timeout or stale heuristic fires.
- The phase must log elapsed sync wait duration on success and failure.

### BenchmarkPhase

**Location**: `/code/cardano-wallet-issue-5278/scripts/ci/bench-restore.sh` invoking the `restore` executable

**Fields**:
- `started_at`
- `finished_at`
- `timeout_seconds`: `43200`
- `bench_name`
- `running_node_socket`
- `result_file`: `restore-mainnet.txt`
- `time_file`: `restore-time.txt`
- `heap_profile`: `restore-mainnet.hp`

**Validation rules**:
- Uses `--running-node` with the socket prepared by `SetupPhase`.
- Does not perform Mithril download or node replay as part of the measured
  restoration work.
- Timeout is classified as `benchmark:restore`.

### RunProvenanceRecord

**Location**: workflow logs and a per-leg JSON artifact in the workflow working directory

**Fields**:
- `workflow_run_id`
- `bench_name`
- `network`
- `node_db_path`
- `mithril_snapshot_hash`
- `mithril_client_source`
- `mithril_client_version`
- `setup_started_at`
- `node_sync_completed_at`
- `node_sync_wait_seconds`
- `benchmark_started_at`
- `final_tip`
- `failure_stage`

**Validation rules**:
- Required fields are printed to the workflow log before the script exits.
- JSON artifact is best effort on failure, but logs must remain sufficient.
- `benchmark_started_at` is set only after node readiness succeeds.

## Relationship Map

```text
RestorationBenchmarkLeg owns one PerLegNodeDatabase
MithrilSnapshot populates one PerLegNodeDatabase per run
SetupPhase selects MithrilSnapshot and starts cardano-node
SetupPhase emits RunProvenanceRecord
BenchmarkPhase consumes the SetupPhase node socket
BenchmarkPhase appends benchmark result artifacts
RunProvenanceRecord links setup timing to benchmark timing
```

## Migration Impact

No persistent database schema changes. The only persistent runner state is the
existing per-leg node database directory, whose lifecycle changes from
"reuse if present" to "wipe at setup start, repopulate from Mithril, preserve
until next setup start".
