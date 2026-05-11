# Feature Specification: Mithril-Provisioned Restoration Benchmark Runs

**Feature Branch**: `005-mithril-bench-snapshots`
**Created**: 2026-05-11
**Status**: Draft
**Input**: GitHub issue cardano-foundation/cardano-wallet#5278 — "Use Mithril snapshots for every restoration benchmark run"

## Clarifications

### Session 2026-05-11

- Q: What operational signal counts as "node synced/ready tip" before the wallet benchmark starts? → A: Node-reported sync progress reaches ≥ 99.9% (canonical `query tip` `syncProgress` field).
- Q: What is the bounded setup timeout (Mithril download + extraction + node startup + node sync)? → A: 2 hours per matrix leg.
- Q: What is the bounded benchmark timeout (wallet restoration phase alone, post-setup)? → A: 12 hours per matrix leg.
- Q: What is the per-leg node DB lifecycle between runs? → A: Cleanup happens at the START of each run, not the end. The previous run's per-leg DB persists on disk until the next run for that leg begins, then is wiped before fresh Mithril provisioning. There is never more than one DB per leg at rest, and a failed run's DB stays available for triage until the next scheduled run.

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Repeatable restoration benchmarks unaffected by stale runner state (Priority: P1)

The benchmark operator triggers the restoration benchmark workflow and gets restoration timings that reflect wallet-restoration cost only, not the cost of replaying a stale ChainDB or recovering from a node-version mismatch. Each run starts from a known-good node database derived from a published Mithril snapshot, regardless of what the self-hosted runner had on disk previously.

**Why this priority**: This is the core value of the change. Without it, results are unstable and runs frequently fail before the benchmark even begins, making the benchmark unusable as a regression signal.

**Independent Test**: Trigger the restoration benchmark workflow on a runner whose persistent disk is empty, stale, or carries a database from an older node version. All four matrix variants (`base`, `seq0`, `seq1`, `rnd5`) must still complete and produce normal benchmark artifacts.

**Acceptance Scenarios**:

1. **Given** a benchmark runner with no usable persistent node database, **When** the restoration benchmark workflow runs, **Then** the workflow provisions a fresh node database from a Mithril snapshot and the benchmark completes without an unrelated replay/version failure.
2. **Given** a benchmark runner whose persistent database was produced by an older node version, **When** the restoration benchmark workflow runs, **Then** the workflow ignores the existing on-disk database, provisions a fresh one from Mithril, and the benchmark completes.
3. **Given** the workflow is invoked on a fresh schedule, **When** all four matrix legs run, **Then** each leg measures only wallet restoration time and not node bring-up time.

---

### User Story 2 — Failed runs are diagnosable from workflow logs alone (Priority: P2)

When a restoration benchmark fails, the on-call engineer can determine from the workflow logs which Mithril snapshot was used, which Mithril client version produced it, where the node database lived on the runner, how long node sync took, and when the wallet benchmark actually started — without re-running anything or SSHing into the runner.

**Why this priority**: Restoration benchmarks run on shared self-hosted runners and are infrequent. Investigating a failure days later, with no runner shell access, requires that provenance be captured at run time. Without this, an engineer cannot distinguish "Mithril upstream regressed" from "node failed to sync" from "wallet restoration regressed".

**Independent Test**: Force a failure at each stage (Mithril download, extraction, node startup, node sync, benchmark) and confirm that the workflow log contains the provenance fields listed below at the point of failure.

**Acceptance Scenarios**:

1. **Given** a workflow run that fails during Mithril download, **When** an engineer reads the logs, **Then** they see the requested snapshot hash and the Mithril client source/version.
2. **Given** a workflow run that fails during node sync, **When** an engineer reads the logs, **Then** they see the node database path, the sync wait duration so far, and the last-known sync state.
3. **Given** a workflow run that succeeds, **When** an engineer reads the logs, **Then** the recorded benchmark start time is clearly distinct from the node-sync completion time.

---

### User Story 3 — Matrix legs run in parallel without sharing mutable node state (Priority: P2)

The four restoration benchmark variants (`base`, `seq0`, `seq1`, `rnd5`) can run simultaneously on the same runner (or across runners) without one leg's node corrupting another's database or one leg waiting on another's node startup.

**Why this priority**: Parallel matrix execution is the whole reason restoration benchmarks complete in a workable wall-clock window. Sharing mutable node state across legs would either force serialization or produce cross-contaminated results.

**Independent Test**: Trigger the workflow and observe that each matrix leg holds an isolated node database path; verify by inspecting log provenance that no two legs reference the same database path.

**Acceptance Scenarios**:

1. **Given** all four matrix legs are scheduled together, **When** they run in parallel, **Then** each leg's node database path is unique and not modified by any other leg.
2. **Given** two legs run at the same time, **When** one leg's node is bringing up, **Then** the other leg's node startup is not blocked by it.

---

### Edge Cases

- Mithril upstream returns no usable snapshot (network outage, signed-certificate gap, empty aggregator). The workflow must fail fast within the setup timeout and produce diagnosable logs rather than silently fall back to a stale on-disk database.
- Mithril download succeeds but extraction fails (corrupt archive, disk full). The workflow must report the failure stage explicitly and not advance to the benchmark stage.
- Node starts but never reaches a synced tip within the bounded sync timeout. The workflow must abort the run, mark setup as failed (not benchmark as failed), and record the sync wait duration reached.
- The bounded benchmark timeout expires while a wallet restoration is in flight. The workflow must terminate the benchmark cleanly and report it as a benchmark-stage failure, separate from setup-stage failures.
- Two matrix legs request the same Mithril snapshot at the same time. The workflow must allow either independent downloads or shared read-only reuse, but must not let one leg's failure or cleanup affect another's database.
- A successful Mithril snapshot is older than the wallets' birth point used by some matrix legs. The workflow must surface the snapshot hash and snapshot tip so this can be diagnosed; it is out of scope to auto-select a "fresh enough" snapshot.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The restoration benchmark workflow MUST provision a node database from a Mithril snapshot at the start of every run, on every matrix leg, instead of relying on any pre-existing node database on the runner.
- **FR-002**: Each matrix leg (`base`, `seq0`, `seq1`, `rnd5`) MUST operate on an isolated node database, such that two legs running concurrently cannot read or write the same database state.
- **FR-003**: The workflow MUST start the wallet restoration benchmark measurement only after `cardano-node` for that leg reports a sync progress value of at least 99.9% (as exposed by the node's canonical tip query). The signal MUST be polled, not assumed from elapsed time.
- **FR-004**: The reported wallet restoration benchmark time MUST NOT include Mithril download, Mithril extraction, node startup, or node sync time. These setup phases MUST be timed and logged independently.
- **FR-005**: For every run, the workflow logs MUST include, at minimum: (a) Mithril snapshot hash, (b) Mithril client source identifier and version, (c) the node database path used for that leg, (d) the elapsed node-sync wait duration, (e) the wall-clock time at which the wallet benchmark was started.
- **FR-006**: The workflow MUST enforce a bounded timeout of 2 hours per matrix leg on the combined Mithril-provisioning-and-node-sync setup phase, and a separate bounded timeout of 12 hours per matrix leg on the wallet restoration benchmark phase. A timeout in one MUST NOT consume the budget of the other.
- **FR-007**: When the setup phase fails or times out (Mithril download, extraction, node startup, or node sync), the workflow MUST classify the failure as a setup failure, distinct from a benchmark failure, in its logs and exit status.
- **FR-008**: The change MUST be observable end-to-end by linking a successful workflow run in which all four restoration benchmark variants start after Mithril-provisioned node sync and produce the normal benchmark artifacts they produced before this change.
- **FR-009**: Each matrix leg MUST wipe its per-leg node database path at the start of the run, immediately before Mithril provisioning. No end-of-run cleanup step is required; this guarantees at most one per-leg database is at rest between runs and preserves the most recent run's database for triage.

### Key Entities

- **Mithril snapshot**: An immutable, hash-identified node database checkpoint published by the Mithril aggregator and consumed by a Mithril client. Has a snapshot hash, a network (mainnet), and an associated tip.
- **Per-leg node database**: A filesystem location dedicated to one matrix leg's node instance, populated by extracting a Mithril snapshot at the start of each run (after wiping any prior contents at the same path) and written to only by that leg's node process. Between runs, the previous run's database persists at this path (available for triage) until the next run for that leg wipes it.
- **Setup phase**: The portion of a run that downloads the Mithril snapshot, extracts it, starts `cardano-node`, and waits for sync. Bounded by its own timeout. Time spent here is recorded but not attributed to the benchmark metric.
- **Benchmark phase**: The portion of a run that executes the wallet restoration benchmark against an already-synced node. Bounded by its own timeout. Time spent here is the metric of interest.
- **Run provenance record**: The set of log fields emitted on every run that lets a later reader identify the snapshot, client, database path, sync wait, and benchmark start time without re-running.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: After this change, 100% of restoration benchmark workflow runs begin from a Mithril-provisioned node database; 0% depend on a pre-existing on-disk database surviving from a previous run.
- **SC-002**: The benchmark metric reported by each matrix leg contains zero contribution from Mithril download, extraction, node startup, or node sync time, verifiable by comparing the metric against the separately recorded setup duration in the same run.
- **SC-003**: All four restoration benchmark variants run in parallel on the same workflow trigger without cross-leg interference, and each completes against its own isolated node database.
- **SC-004**: Given a failed run, an engineer reading only the workflow logs can identify the failure stage (Mithril download, Mithril extraction, node startup, node sync, or wallet benchmark) and the snapshot hash, Mithril client version, and node database path in use, in under 5 minutes and without consulting the runner host.
- **SC-005**: A specific workflow run is recorded in the closing PR that shows all four restoration benchmark variants starting after Mithril-provisioned node sync and producing the normal benchmark artifacts.

## Assumptions

- A signed mainnet Mithril snapshot is reliably available at workflow trigger time; transient aggregator outages are tolerated by retry within the setup timeout, but a fully unavailable aggregator is treated as a setup failure rather than a fall-through to stale on-disk state.
- The self-hosted benchmark runners have sufficient disk and network bandwidth to download and extract a fresh node database per matrix leg per run, within the setup timeout.
- Issue #5115 (replay-aware handling for existing DBs) is a separate concern; this work does not need to preserve any "reuse existing DB" code path and may remove or bypass it as a consequence.
- The four matrix legs share a runner pool. Resource isolation between legs is achieved by isolated database paths, not by dedicated runners; concurrent setup phases are an expected and acceptable load on a runner.
- Reusing existing Mithril provisioning infrastructure already used elsewhere in CI is preferred over introducing a new, parallel implementation; the choice of which is left to planning.
- Snapshot freshness relative to wallet birth points is out of scope. If a snapshot's tip lies behind a wallet's birth point and the benchmark misbehaves as a result, that is a separate spec.
