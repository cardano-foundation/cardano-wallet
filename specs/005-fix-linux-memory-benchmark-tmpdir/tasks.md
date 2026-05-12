# Tasks: Fix Linux Memory Benchmark Temp Directory

**Input**: Design documents from `/code/cardano-wallet/specs/005-fix-linux-memory-benchmark-tmpdir/`
**Prerequisites**: [spec.md](spec.md), [plan.md](plan.md)

## Format: `[ID] [P?] [Story] Description`

- **[P]** means the task can run in parallel before implementation.
- **[US1]** covers restoring the Linux Memory Benchmark job.

## Phase 1: Specification and Review

- [X] T001 [US1] Record the issue #5283 failure mode and acceptance criteria in `spec.md`
- [X] T002 [US1] Plan a short unique temp root that avoids both Unix socket path overflow and fixed shared `/tmp` collisions
- [X] T003 [US1] Run the solo plan review checklist before implementation
- [X] T004 [US1] Run the solo tasks review checklist before implementation

## Phase 2: Workflow Implementation

- [X] T005 [US1] Update `.github/workflows/linux-benchmarks.yml` so the Memory Benchmark step creates and exports a short `mktemp` root under `/tmp`
- [X] T006 [US1] Keep non-memory benchmark matrix entries on `${{ runner.temp }}`
- [X] T007 [US1] Add cleanup for the short memory benchmark temp root

## Phase 3: Validation

- [X] T008 [US1] Run local workflow syntax and socket path-length verification
- [X] T009 [US1] Record verification evidence in the PR body
- [X] T010 [US1] Confirm a GitHub Actions workflow dispatch or `master` run passes the Memory Benchmark job

## Dependencies & Execution Order

T001-T004 must complete before T005. T008 must complete before PR handoff. T010 is an external boundary check after the branch is available in GitHub Actions.
