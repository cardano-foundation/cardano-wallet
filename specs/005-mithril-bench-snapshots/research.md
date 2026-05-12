# Research: Mithril-Provisioned Restoration Benchmark Runs

**Feature Branch**: `005-mithril-bench-snapshots`
**Date**: 2026-05-11

## Decision: Reuse the existing Mithril snapshot helper

**Decision**: Use `/code/cardano-wallet-issue-5278/run/mainnet/nix/snapshot.sh`
as the benchmark setup entry point for downloading and extracting the latest
mainnet Mithril snapshot. Extend `/code/cardano-wallet-issue-5278/run/common/nix/snapshot.sh`
only as needed to emit snapshot provenance.

**Rationale**: The repo already uses this helper in
`/code/cardano-wallet-issue-5278/.github/workflows/linux-mithril-sync.yml`.
It sources the mainnet Mithril aggregator and verification keys from
`run/mainnet/nix/.env`, runs `mithril-client` through `nix shell --quiet`, wipes
the target `NODE_DB`, downloads the selected snapshot, and moves the extracted
`db/` contents into the node database path. Reusing it avoids a second Mithril
client setup path.

**Alternatives considered**:
- Inline all Mithril commands in `restoration-benchmarks.yml`: rejected because
  it duplicates a pattern already maintained in `run/common/nix/snapshot.sh`.
- Use `run/mainnet/nix/run.sh` with `USE_MITHRIL`: rejected because it also owns
  wallet/node service lifecycle and has cleanup behavior that does not match the
  "preserve DB until next run starts" requirement.
- Use `Cardano.Launcher.Mithril` from Haskell: rejected for this feature because
  the benchmark workflow already has shell-level orchestration and the existing
  CI helper is enough.

## Decision: Own phase separation in `scripts/ci/bench-restore.sh`

**Decision**: Move Mithril provisioning, node startup, node sync polling, and
stage timing into `/code/cardano-wallet-issue-5278/scripts/ci/bench-restore.sh`.
Then run the existing benchmark executable against an already-running node using
`--running-node`.

**Rationale**: The current benchmark executable can either start its own node or
connect to an existing node. If the benchmark binary starts the node, the shell
cannot enforce separate setup and benchmark timeouts. Preparing a synced node in
the script lets the workflow keep setup failures distinct from benchmark
failures while preserving the benchmark implementation and result format.

**Alternatives considered**:
- Keep letting `restore-bench` start the node: rejected because setup and
  benchmark timeouts remain coupled at the process boundary.
- Add a new benchmark executable mode: rejected unless implementation discovers
  that the existing `--running-node` option is insufficient.

## Decision: Wipe the per-leg database at setup start only

**Decision**: For each matrix leg, remove the contents of the configured node
database path immediately before Mithril provisioning. Do not remove the
database on success or failure.

**Rationale**: This satisfies the disk-footprint requirement and leaves the most
recent failed run's database available for triage. The current workflow already
assigns stable, unique database names: `mainnet-1`, `mainnet-2`, `mainnet-3`,
and `mainnet-4`. Keeping those names maintains operator familiarity while
preventing cross-leg state sharing.

**Alternatives considered**:
- Use a new timestamped database directory per run: rejected because it can
  accumulate multiple full ChainDBs per leg.
- Clean up at the end of each run: rejected because it removes failure evidence
  that the spec requires preserving until the next run.

## Decision: Poll `cardano-cli query tip` for readiness

**Decision**: After node startup, poll:

```bash
cardano-cli query tip --mainnet --socket-path "$socket"
```

every 15 seconds. Parse the JSON `.syncProgress` field after removing `%`, and
allow the benchmark phase to start only after the value is at least `99.9`.

**Rationale**: The feature clarification names node-reported `syncProgress` from
the canonical tip query as the readiness signal. Polling the node socket avoids
guessing based on elapsed time or relying on wallet sync state.

**Alternatives considered**:
- Use the wallet `/v2/network/information` endpoint: rejected because this
  benchmark does not need to start `cardano-wallet serve` and the clarified
  signal is node-reported.
- Use the benchmark executable's internal `Ready` wait as the only gate:
  rejected because it hides setup timing and failure classification inside the
  benchmark process.

## Decision: Use a 15 second poll cadence and 20 minute stale heuristic

**Decision**: Poll every 15 seconds. Once `cardano-cli query tip` returns valid
JSON, track the last observed slot and sync progress. If neither value changes
for 20 minutes before readiness is reached, classify the run as
`setup:node-sync` failure and exit non-zero. The hard 2 hour setup timeout still
applies around the entire setup phase.

**Rationale**: A 15 second cadence keeps logs useful without flooding the GitHub
Actions UI. A 20 minute no-progress window is long enough to avoid normal
mainnet block cadence variance and short enough to surface a stuck node before
the full setup budget is wasted.

**Alternatives considered**:
- Poll every second: rejected because it produces noisy logs and unnecessary
  `cardano-cli` invocations.
- Rely only on the 2 hour setup timeout: rejected because it delays diagnosis
  when the node is clearly stalled.

## Decision: Fixed setup and benchmark timeouts

**Decision**: Use a fixed 2 hour setup timeout and a fixed 12 hour benchmark
timeout per matrix leg. Implement both with GNU `timeout` from `nixpkgs#coreutils`.
Keep the GitHub Actions job timeout slightly larger than the sum to allow
checkout, artifact upload, and teardown.

**Rationale**: The spec requires separate bounded budgets. Fixed budgets make
scheduled benchmark results comparable and remove the current manual
`to-tip-timeout` input as a source of run-to-run variation.

**Alternatives considered**:
- Keep the workflow dispatch `to-tip-timeout` choice: rejected because it lets
  operators vary a required benchmark invariant.
- Use only the job-level `timeout-minutes`: rejected because it cannot classify
  whether setup or benchmark exceeded its budget.

## Decision: Emit both logs and JSON provenance

**Decision**: Print line-oriented provenance fields to the workflow log and
write a JSON artifact for each matrix leg. Include Mithril snapshot hash,
Mithril client source/version, node DB path, setup start/end, sync wait
duration, final node tip, and benchmark start time.

**Rationale**: The spec requires diagnosis from workflow logs alone. A JSON
artifact gives reviewers and future scripts a stable format without replacing
human-readable logs.

**Alternatives considered**:
- Artifact only: rejected because the requirement explicitly calls for workflow
  logs to be sufficient.
- Logs only: rejected because structured artifacts are cheap and make PR
  evidence easier to inspect.
