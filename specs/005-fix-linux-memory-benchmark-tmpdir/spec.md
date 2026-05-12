# Feature Specification: Fix Linux Memory Benchmark Temp Directory

**Feature Branch**: `5283-linux-memory-benchmark-tmpdir`
**Created**: 2026-05-12
**Status**: Draft
**Input**: GitHub issue #5283: "Fix Linux memory benchmark socket path length failure"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Linux memory benchmark starts cardano-node (Priority: P1)

A maintainer dispatches or observes the Linux Benchmarks workflow on `master`, and the Memory Benchmark job starts its local `cardano-node` without failing on the Unix socket path length.

**Why this priority**: The current failure is not a benchmark regression. It prevents the memory benchmark from producing any heap profile or CSV artifacts.

**Independent Test**: Validate the workflow syntax and run a path-length smoke check showing the Memory Benchmark temp root creates a `node.socket` path below the 108-byte Unix socket limit.

**Acceptance Scenarios**:

1. **Given** the Memory Benchmark job runs on a self-hosted runner whose `${{ runner.temp }}` is long, **When** the benchmark step starts, **Then** it uses a short temporary root under `/tmp` before invoking `scripts/ci/bench-memory.sh`
2. **Given** the memory benchmark creates `membench-snapshot/db-node/node.socket`, **When** the generated benchmark temp root is combined with the existing benchmark subdirectories, **Then** the socket path remains below 108 bytes
3. **Given** the memory benchmark finishes or fails, **When** the benchmark step exits, **Then** the short temporary root is cleaned up
4. **Given** non-memory Linux benchmark jobs run in the same matrix, **When** their steps start, **Then** they continue using `${{ runner.temp }}` to avoid unnecessary changes

### Edge Cases

- Linux `sockaddr_un.sun_path` is limited to 108 bytes, so a self-hosted runner path like `/home/gha-runner/actions-runner-7/_work/_temp` can exceed the limit after benchmark subdirectories are appended.
- A single fixed shared `/tmp/gha-bench` directory can collide across many self-hosted runners running as the same user.
- The workflow must preserve artifact upload behavior when a benchmark fails.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The Linux Memory Benchmark step MUST use a short temp root before invoking `scripts/ci/bench-memory.sh`.
- **FR-002**: The short temp root MUST be unique per benchmark step execution.
- **FR-003**: The short temp root MUST be cleaned up when the benchmark step exits.
- **FR-004**: Other Linux benchmark jobs SHOULD continue to use `${{ runner.temp }}` unless they also need a short path.
- **FR-005**: The workflow change MUST preserve the existing matrix command and artifact upload behavior.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A representative memory benchmark socket path built from the new temp root is less than 108 bytes.
- **SC-002**: `actionlint` accepts `.github/workflows/linux-benchmarks.yml` with the repository's custom self-hosted runner labels ignored.
- **SC-003**: A workflow dispatch or `master` Linux Benchmarks run shows the Memory Benchmark job producing `memory.hp`, `memory.svg`, and `memory-bench-results.csv`.

## Exclusions

- This change does not modify benchmark executables or measured benchmark behavior.
- This change does not change the DB, API, read-blocks, or latency benchmark temp directory behavior beyond keeping their existing workflow environment.
