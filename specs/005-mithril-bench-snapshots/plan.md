# Implementation Plan: Mithril-Provisioned Restoration Benchmark Runs

**Branch**: `005-mithril-bench-snapshots` | **Date**: 2026-05-11 | **Spec**: [/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/spec.md](/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/spec.md)
**Input**: Feature specification from `/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/spec.md`
**Issue**: #5278

## Summary

Change the restoration benchmark workflow so every matrix leg starts from a
fresh mainnet Mithril ChainDB snapshot instead of whatever node database happens
to exist on the self-hosted runner. The implementation stays at the CI
orchestration layer: `.github/workflows/restoration-benchmarks.yml` keeps the
four benchmark variants, while `scripts/ci/bench-restore.sh` owns the setup
phase by wiping the per-leg database, calling the existing Mithril snapshot
helper, starting a per-leg `cardano-node`, polling `cardano-cli query tip` until
`syncProgress >= 99.9`, and only then running the restore benchmark with
`--running-node`.

## Technical Context

**Language/Version**: Bash for CI orchestration; Haskell/GHC 9.6.x via the Nix flake for the existing benchmark executable
**Primary Dependencies**: GitHub Actions, Nix flake packages, `cardano-node`, `cardano-cli`, `mithril-client` from `github:input-output-hk/mithril?ref=2543.1-hotfix`, `jq`, GNU `timeout`, existing `ci.benchmarks.restore` derivation
**Storage**: Per-leg filesystem ChainDB under `$HOME/databases/node/<matrix-node-db>` on self-hosted benchmark runners
**Testing**: Shell syntax/lint checks, Nix build of `.#ci.benchmarks.restore`, targeted workflow dispatch, workflow-log inspection for provenance and timeout classification
**Target Platform**: Linux x86_64 self-hosted GitHub Actions runners labelled `cardano-wallet-bench`
**Project Type**: Haskell monorepo with CI workflow and shell-script integration changes
**Performance Goals**: All four matrix legs reach benchmark start within the 2 hour setup budget and preserve the existing restoration benchmark artifacts
**Constraints**: Do not include Mithril download, extraction, node startup, or node sync in the reported restoration benchmark metric; preserve the last per-leg database after a failed run until the next run starts; never share mutable ChainDB state between matrix legs
**Scale/Scope**: One scheduled/manual workflow, four matrix legs (`base`, `seq0`, `seq1`, `rnd5`), one CI script, one existing shared Mithril snapshot helper

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | PASS | The change is confined to benchmark CI orchestration and reuses existing benchmark and Mithril tooling. |
| II. Era-Aware Design | PASS | No protocol-era domain logic changes are planned. The node and benchmark continue to use existing mainnet configs. |
| III. Type Safety as Security | PASS | No cryptographic key, transaction, or wallet type changes are planned. |
| IV. Formal Specification | PASS | REST API and Lean specifications are unaffected. |
| V. Reproducible Builds | PASS | All new runtime tools come from `nix shell --quiet`; the Mithril client source stays pinned to the existing repo-local helper pattern. |
| VI. Comprehensive Testing | PASS | Verification includes shell checks, Nix benchmark build, and an end-to-end restoration benchmark workflow run. |
| VII. Code Quality Gates | PASS | CI must be green before the draft PR is marked ready. |

## Project Structure

### Documentation

```text
/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/
|-- plan.md
|-- research.md
|-- data-model.md
|-- quickstart.md
|-- contracts/
|   `-- restoration-benchmarks.md
|-- checklists/
|   `-- requirements.md
`-- spec.md
```

### Affected Source Code

```text
/code/cardano-wallet-issue-5278/
|-- .github/workflows/
|   |-- restoration-benchmarks.yml     # Fixed setup/benchmark budgets, per-leg setup call, artifact upload
|   `-- linux-mithril-sync.yml         # Reference pattern only; no behavioral change expected
|-- scripts/ci/
|   `-- bench-restore.sh               # Mithril setup, node lifecycle, sync polling, benchmark timeout
|-- run/common/nix/
|   `-- snapshot.sh                    # Reused and possibly extended for provenance output
|-- run/mainnet/nix/
|   |-- .env                           # Mainnet Mithril aggregator and verification keys
|   `-- snapshot.sh                    # Symlink entry point used by benchmark setup
`-- lib/benchmarks/
    |-- src/Cardano/Wallet/BenchShared.hs
    `-- exe/restore-bench.hs           # Expected unchanged; touch only if `--running-node` cannot satisfy phase split
```

**Structure Decision**: Keep implementation in the existing CI surface. Avoid
adding a second benchmark executable or a new package. Prefer a small shell
helper/function inside `scripts/ci/bench-restore.sh`; extract a separate
`scripts/ci/restore-mithril-node.sh` only if the script becomes hard to test.

## Implementation Approach

1. **Workflow contract update**
   - Remove or stop using the operator-selected `to-tip-timeout` input.
   - Set fixed budgets in the restore job: `SETUP_TIMEOUT_SECONDS=7200` and `BENCHMARK_TIMEOUT_SECONDS=43200`.
   - Keep the four matrix entries and their existing unique node database names.
   - Reduce the job timeout to the combined setup and benchmark budget plus CI overhead, for example 870 minutes.

2. **Per-leg setup phase in `scripts/ci/bench-restore.sh`**
   - Treat the third argument as the persistent per-leg database path.
   - Emit setup provenance before destructive work: bench name, network, node DB path, runner temp path, and timestamps.
   - Wipe `${node_db:?}` at the start of the setup phase only.
   - Source `run/mainnet/nix/.env` through a path derived from the runtime repo root and call `run/mainnet/nix/snapshot.sh` with `NODE_DB=$node_db`.
   - Log the Mithril client source identifier and `mithril-client --version` before snapshot listing/download.
   - Capture the snapshot hash selected by the helper. If `snapshot.sh` is extended, make it print and optionally write this value without changing its current behavior for `linux-mithril-sync.yml`.

3. **Node startup and readiness gate**
   - Start one `cardano-node` per matrix leg using the existing mainnet config and the same isolated database path.
   - Put the socket and node log under `$TMPDIR/bench/restore/<bench>/` so concurrent legs do not collide.
   - Poll `cardano-cli query tip --mainnet --socket-path "$socket"` every 15 seconds.
   - Parse `.syncProgress` as a percentage number after removing the trailing percent sign.
   - Proceed only when `syncProgress >= 99.9`.
   - Fail the setup phase if the 2 hour setup timeout expires or if the reported slot/progress stays unchanged for 20 minutes after the node first returns a valid JSON tip.

4. **Benchmark phase**
   - Run the existing `restore` executable with `--running-node "$socket"` so setup and node sync are complete before the measured wallet restoration work starts.
   - Wrap the benchmark command in GNU `timeout --foreground --kill-after=60s "$BENCHMARK_TIMEOUT_SECONDS"`.
   - Keep the existing result extraction, memory summary, heap profile conversion, and artifact names.
   - Pass a very small `--to-tip-timeout` only as a defensive readiness check if the already-running node path still invokes `prepareNode`; it should not be the setup budget.

5. **Failure classification**
   - Emit `FAILURE_STAGE=setup:mithril-list`, `setup:mithril-download`, `setup:node-start`, `setup:node-sync`, or `benchmark:restore` before exiting non-zero.
   - Preserve the per-leg database on all exits. The next run for the same leg will wipe it before provisioning.
   - Kill the node process on script exit, but do not remove its database.

6. **Observability**
   - Emit line-oriented log fields and a small JSON provenance artifact containing snapshot hash, Mithril client source/version, node DB path, setup timings, final tip, and benchmark start time.
   - Extend the workflow artifact upload pattern to include `*.json`.
   - Record a successful end-to-end workflow URL in the PR before marking it ready.

## Phase 0 Research Output

Resolved in [/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/research.md](/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/research.md).

## Phase 1 Design Output

- Data model: [/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/data-model.md](/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/data-model.md)
- Interface contract: [/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/contracts/restoration-benchmarks.md](/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/contracts/restoration-benchmarks.md)
- Quickstart: [/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/quickstart.md](/code/cardano-wallet-issue-5278/specs/005-mithril-bench-snapshots/quickstart.md)

## Constitution Check After Design

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | PASS | Existing CI entry points are reused; no production wallet behavior changes. |
| II. Era-Aware Design | PASS | No era-indexed code changes are required. |
| III. Type Safety as Security | PASS | No type-safety regressions; shell changes only orchestrate benchmark infrastructure. |
| IV. Formal Specification | PASS | No public API or formal spec change. |
| V. Reproducible Builds | PASS | Runtime dependencies are supplied by `nix shell --quiet`; no unpinned external installer is introduced. |
| VI. Comprehensive Testing | PASS | The required proof is a workflow run showing all four variants start after Mithril sync and produce artifacts. |
| VII. Code Quality Gates | PASS | The PR remains draft until CI and the targeted benchmark workflow are green. |

## Complexity Tracking

No constitution violations. No additional complexity exception is requested.
