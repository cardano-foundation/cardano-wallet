# Tasks: Mithril-Provisioned Restoration Benchmark Runs

**Input**: Design documents from `/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/`
**Issue**: #5278
**PR**: #5279

## Commit Discipline

Every behavior-changing commit must be a vertical, bisect-safe slice. For this
feature, each story phase below is a reviewable slice: it includes its
regression proof, implementation, and verification path in the same slice.

The repository has no dedicated shell test harness for these CI scripts, so the
proof strategy is:

- RED-style baseline checks against the current workflow/script behavior.
- Static gates: `bash -n`, `scripts/shellcheck.sh`, and `nix build --quiet .#ci.benchmarks.restore`.
- Mandatory live boundary proof from a `workflow_dispatch` restoration
  benchmark run before the PR is marked ready.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel because it touches separate files or is a
  non-mutating check.
- **[Story]**: Maps to a user story from `spec.md`.
- Every task names the file path or review artifact it changes or validates.

---

## Phase 1: Setup (Shared PR Infrastructure)

**Purpose**: Establish the local gate and review proof surface before changing
workflow behavior.

- [x] T001 Create PR gate script in `llm/reviews/5279/gate.sh` covering `bash -n scripts/ci/bench-restore.sh`, `scripts/shellcheck.sh`, and `nix build --quiet .#ci.benchmarks.restore`
- [x] T002 [P] Record the current branch/task baseline in `/code/cardano-wallet-issue-5278/WIP.md`
- [x] T003 [P] Decide whether generated agent context should be committed by reviewing `/code/cardano-wallet-issue-5278/AGENTS.md`

**Checkpoint**: The PR has a repeatable local gate and the local handoff notes
identify any generated non-code files that should be included or dropped.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Prepare shared helpers used by all user stories.

- [x] T004 Extend `run/common/nix/snapshot.sh` to print the Mithril client source, client version, selected snapshot hash, and best-effort snapshot tip without breaking `.github/workflows/linux-mithril-sync.yml`
- [x] T005 [P] Add comments or usage text for setup and benchmark timeout environment variables in `scripts/ci/bench-restore.sh`
- [x] T006 Run the PR gate from `llm/reviews/5279/gate.sh` after foundational helper changes

**Checkpoint**: Existing Mithril sync workflow behavior is preserved while the
snapshot helper exposes the provenance needed by restoration benchmarks.

---

## Phase 3: User Story 1 - Repeatable restoration benchmarks unaffected by stale runner state (Priority: P1) MVP

**Goal**: Every restoration benchmark matrix leg wipes its own database at run
start, provisions a fresh Mithril snapshot, waits for node readiness, and runs
the benchmark against an already-running synced node.

**Independent Test**: Trigger the restoration benchmark workflow on a runner
whose persistent disk is empty, stale, or contains an older-node database. All
four variants complete and produce normal benchmark artifacts.

### Tests and Proof for User Story 1

- [x] T007 [P] [US1] Capture RED baseline that `.github/workflows/restoration-benchmarks.yml` passes an existing `$HOME/databases/node/<matrix-node-db>` directly to `scripts/ci/bench-restore.sh` without a Mithril setup phase
- [x] T008 [P] [US1] Capture RED baseline that `scripts/ci/bench-restore.sh` does not call `run/mainnet/nix/snapshot.sh`, does not poll `cardano-cli query tip`, and starts the benchmark with `--node-db` instead of `--running-node`

### Implementation for User Story 1

- [x] T009 [US1] Update `.github/workflows/restoration-benchmarks.yml` to remove or ignore `to-tip-timeout`, set `SETUP_TIMEOUT_SECONDS=7200`, `BENCHMARK_TIMEOUT_SECONDS=43200`, `SYNC_POLL_INTERVAL_SECONDS=15`, and `SYNC_STALE_SECONDS=1200`
- [x] T010 [US1] Update `.github/workflows/restoration-benchmarks.yml` so the restore job Nix shell provides `coreutils`, `gnugrep`, `gawk`, `jq`, `time`, `cardano-node`, `cardano-cli`, and `haskellPackages.hp2pretty`
- [x] T011 [US1] Implement start-of-run per-leg database cleanup in `scripts/ci/bench-restore.sh` by wiping `${node_db:?}` immediately before Mithril provisioning and never at script exit
- [x] T012 [US1] Implement Mithril provisioning in `scripts/ci/bench-restore.sh` by sourcing `run/mainnet/nix/.env` and invoking `run/mainnet/nix/snapshot.sh` with `NODE_DB=$node_db`
- [x] T013 [US1] Implement per-leg node startup in `scripts/ci/bench-restore.sh` using `configs/cardano/mainnet/config.json`, `configs/cardano/mainnet/topology.json`, the isolated `node_db`, and a socket under `$TMPDIR/bench/restore/$bench/`
- [x] T014 [US1] Implement node readiness polling in `scripts/ci/bench-restore.sh` with `cardano-cli query tip --mainnet --socket-path "$socket"`, 15 second cadence, `syncProgress >= 99.9`, 2 hour setup timeout, and 20 minute stale slot/progress failure
- [x] T015 [US1] Update `scripts/ci/bench-restore.sh` to run `bench-restore/bin/restore` with `--running-node "$socket"` and a separate GNU `timeout --foreground --kill-after=60s "$BENCHMARK_TIMEOUT_SECONDS"` benchmark budget
- [x] T016 [US1] Preserve existing result extraction, memory summary, heap profile conversion, and artifact filenames in `scripts/ci/bench-restore.sh`

### Verification for User Story 1

- [x] T017 [US1] Run `llm/reviews/5279/gate.sh` and record the green output in `/code/cardano-wallet-issue-5278/WIP.md`
- [x] T018 [US1] Run the local dry-run command from `specs/005-mithril-bench-snapshots/quickstart.md` and confirm the script does not silently reuse an existing node database

**Checkpoint**: User Story 1 is complete when the script has a separate setup
phase, the benchmark phase starts only after a synced already-running node is
available, and local static gates are green.

---

## Phase 4: User Story 2 - Failed runs are diagnosable from workflow logs alone (Priority: P2)

**Goal**: Workflow logs and artifacts identify the Mithril snapshot, client
version, database path, sync duration, benchmark start time, and failure stage.

**Independent Test**: Force failures at Mithril list/download/extraction, node
startup, node sync, and benchmark execution; confirm logs contain the available
provenance and a specific `FAILURE_STAGE`.

### Tests and Proof for User Story 2

- [x] T019 [P] [US2] Capture RED baseline that `scripts/ci/bench-restore.sh` does not print `MITHRIL_SNAPSHOT_HASH`, `MITHRIL_CLIENT_VERSION`, `NODE_SYNC_WAIT_SECONDS`, `BENCHMARK_STARTED_AT`, or `FAILURE_STAGE`
- [x] T020 [P] [US2] Add failure-stage smoke commands to `specs/005-mithril-bench-snapshots/quickstart.md` for setup timeout, node-sync timeout, and benchmark timeout validation

### Implementation for User Story 2

- [x] T021 [US2] Add line-oriented provenance logging to `scripts/ci/bench-restore.sh` for `BENCH_NAME`, `NETWORK`, `NODE_DB`, `MITHRIL_CLIENT_SOURCE`, `MITHRIL_CLIENT_VERSION`, `MITHRIL_SNAPSHOT_HASH`, setup timestamps, sync progress, sync wait seconds, and benchmark start time
- [x] T022 [US2] Add per-leg JSON provenance artifact writing to `scripts/ci/bench-restore.sh` with fields from `specs/005-mithril-bench-snapshots/contracts/restoration-benchmarks.md`
- [x] T023 [US2] Add explicit `FAILURE_STAGE=setup:mithril-list`, `setup:mithril-download`, `setup:mithril-extract`, `setup:node-start`, `setup:node-sync`, and `benchmark:restore` classification paths in `scripts/ci/bench-restore.sh`
- [x] T024 [US2] Update `.github/workflows/restoration-benchmarks.yml` artifact upload patterns to include `*.json` alongside the existing `*.txt`, `*.log`, `*.svg`, and `*.hp` artifacts
- [x] T025 [US2] Ensure `scripts/ci/bench-restore.sh` writes best-effort provenance on exit even when setup fails before the benchmark starts

### Verification for User Story 2

- [x] T026 [US2] Run `llm/reviews/5279/gate.sh` and record the green output in `/code/cardano-wallet-issue-5278/WIP.md`
- [ ] T027 [US2] Run the failure-stage smoke commands from `specs/005-mithril-bench-snapshots/quickstart.md` and confirm the expected log fields appear

**Checkpoint**: User Story 2 is complete when an engineer can diagnose setup
or benchmark failure stage and provenance from logs without shell access to the
runner.

---

## Phase 5: User Story 3 - Matrix legs run in parallel without sharing mutable node state (Priority: P2)

**Goal**: All four matrix legs can run concurrently without sharing node
database, socket, or log paths.

**Independent Test**: Trigger the workflow and verify each matrix leg logs a
different node database path and isolated socket/log paths.

### Tests and Proof for User Story 3

- [x] T028 [P] [US3] Capture RED baseline that `scripts/ci/bench-restore.sh` currently uses only the passed node DB for isolation and does not log per-leg socket or node log paths
- [x] T029 [P] [US3] Capture RED baseline that `.github/workflows/restoration-benchmarks.yml` has unique `matrix.node-db` values but no logged uniqueness check

### Implementation for User Story 3

- [x] T030 [US3] Ensure `.github/workflows/restoration-benchmarks.yml` keeps unique stable `matrix.node-db` values for `base`, `seq0`, `seq1`, and `rnd5`
- [x] T031 [US3] Ensure `scripts/ci/bench-restore.sh` derives socket, node log, and temporary work paths from `$TMPDIR/bench/restore/$bench/` so concurrent legs cannot collide
- [x] T032 [US3] Ensure `scripts/ci/bench-restore.sh` kills only the node process it started and never removes another leg's socket, log, or database path
- [x] T033 [US3] Add per-leg `NODE_SOCKET_PATH` and `NODE_LOG_PATH` log fields to `scripts/ci/bench-restore.sh`

### Verification for User Story 3

- [x] T034 [US3] Run `llm/reviews/5279/gate.sh` and record the green output in `/code/cardano-wallet-issue-5278/WIP.md`
- [ ] T035 [US3] Trigger the restoration benchmark workflow with `gh workflow run "Restoration Benchmarks" --ref 005-mithril-bench-snapshots` and verify all four matrix legs log distinct `NODE_DB`, `NODE_SOCKET_PATH`, and `NODE_LOG_PATH` values

**Checkpoint**: User Story 3 is complete when parallel matrix legs have unique
mutable paths and no cleanup path can affect another leg.

---

## Phase 6: Polish and Cross-Cutting Concerns

**Purpose**: Final validation, documentation, and PR readiness.

- [x] T036 [P] Update `specs/005-mithril-bench-snapshots/quickstart.md` with the final local and workflow verification commands if implementation changes them
- [ ] T037 [P] Update `/code/cardano-wallet-issue-5278/WIP.md` with the final task status, workflow run URL, and CI status
- [ ] T038 Run `llm/reviews/5279/gate.sh` after all feature slices are complete
- [ ] T039 Run mandatory workflow-dispatch proof with `gh workflow run "Restoration Benchmarks" --ref 005-mithril-bench-snapshots` and record the successful run URL in `/code/cardano-wallet-issue-5278/WIP.md`
- [ ] T040 Verify PR #5279 records the successful workflow-dispatch URL showing all four variants start after Mithril-provisioned node sync and upload normal artifacts
- [ ] T041 Check CI run #25660500468 or its successor and rerun only unrelated flaky jobs from `/code/cardano-wallet-issue-5278/WIP.md` if needed

---

## Dependencies and Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies.
- **Foundational (Phase 2)**: Depends on Setup completion and blocks all user stories.
- **User Story 1 (Phase 3)**: Depends on Foundational. This is the MVP and should land first.
- **User Story 2 (Phase 4)**: Depends on the setup/benchmark phase boundaries from US1.
- **User Story 3 (Phase 5)**: Depends on the node lifecycle from US1 and can be reviewed after US1 or alongside US2 if the diffs stay separate.
- **Polish (Phase 6)**: Depends on all desired user stories.

### User Story Dependencies

- **US1**: Required first because it creates the Mithril setup, node readiness, and benchmark phase split.
- **US2**: Requires US1 phase names and lifecycle points for accurate provenance and failure classification.
- **US3**: Requires US1 node startup and cleanup logic, then hardens path isolation.

### Reviewable Commit Slices

1. **Slice A - Foundation**: T001 through T006.
2. **Slice B - US1 MVP**: T007 through T018.
3. **Slice C - US2 Observability**: T019 through T027.
4. **Slice D - US3 Parallel Isolation**: T028 through T035.
5. **Slice E - Final Evidence**: T036 through T041.

Each slice should pass `llm/reviews/5279/gate.sh` before review handoff.

---

## Parallel Opportunities

- T002 and T003 can run while T001 is being prepared.
- T005 can run in parallel with T004.
- T007 and T008 are independent baseline checks.
- T019 and T020 are independent US2 proof/documentation tasks.
- T028 and T029 are independent US3 baseline checks.
- T036 and T037 can run in parallel during final documentation cleanup.

---

## Implementation Strategy

### MVP First

1. Complete Phase 1 and Phase 2.
2. Complete Phase 3 for US1.
3. Stop and validate that the workflow/script no longer depends on stale runner
   node databases and that local gates are green.

### Incremental Delivery

1. Add US1 to make the benchmark start from a Mithril-provisioned synced node.
2. Add US2 to make failures diagnosable from logs and artifacts.
3. Add US3 to harden concurrent matrix isolation.
4. Run the full restoration benchmark workflow and record the successful run in
   PR #5279.
