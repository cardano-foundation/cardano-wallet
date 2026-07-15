# Feature Specification: Bound Unit Test Memory

**Feature Branch**: `fix/5309-unit-memory-leak`
**Created**: 2026-07-15
**Status**: Draft
**Input**: GitHub issue #5309: the `cardano-wallet-unit` suite retains
approximately 17 GB of live heap and the intended default heap limit is
silently discarded in six test executables.

## User Scenarios & Testing

### User Story 1 - Run the full unit suite safely (Priority: P1)

As a contributor or CI operator, I can run the complete
`cardano-wallet-unit` suite on a memory-constrained machine without the test
process or an unrelated process being killed by the operating system.

**Why this priority**: The current behavior makes the primary unit check
unreliable and can destabilize the host that runs it.

**Independent Test**: Run the complete suite with its configured concurrency
and a 2 GiB heap ceiling. It completes normally and reports all test results
without exhausting the ceiling.

**Acceptance Scenarios**:

1. **Given** a clean build from the current default branch, **When** the full
   unit suite runs with four capabilities and a 2 GiB heap ceiling, **Then** it
   exits normally without a heap overflow or OS-level OOM kill.
2. **Given** every top-level spec group is enabled, **When** the full suite
   finishes, **Then** the fix has not hidden, skipped, or filtered tests to
   obtain the lower memory footprint.
3. **Given** the suite runs long enough to exercise all groups, **When** major
   garbage collections occur, **Then** live memory does not grow without bound
   as completed test groups accumulate.

---

### User Story 2 - Fail safely if memory regresses (Priority: P2)

As a contributor or CI operator, I receive a bounded, diagnosable test failure
if a suite exceeds its supported heap budget instead of an opaque host-level
OOM kill.

**Why this priority**: A correctly applied ceiling limits blast radius and
makes future regressions visible at the failing executable.

**Independent Test**: Inspect each affected executable's runtime defaults and
confirm that both the heap ceiling and four-capability setting are present;
then verify that an explicitly smaller ceiling produces a process-local heap
failure.

**Acceptance Scenarios**:

1. **Given** any of the six affected test executables, **When** its compiled
   runtime defaults are inspected, **Then** both the 2 GiB heap ceiling and
   four-capability setting are present.
2. **Given** a deliberately undersized heap ceiling, **When** a test executable
   exceeds it, **Then** that executable reports heap exhaustion and exits
   without causing the operating system to select another process for
   termination.
3. **Given** a contributor supplies explicit runtime options, **When** the test
   starts, **Then** the usual command-line override behavior remains available.

---

### User Story 3 - Detect the memory regression before merge (Priority: P3)

As a maintainer, I have repeatable evidence that the full-suite memory problem
is fixed and can detect a recurrence during review.

**Why this priority**: A one-off local observation is not sufficient protection
for a costly and destructive regression.

**Independent Test**: Execute the documented bounded regression check on the
affected suite and confirm that it distinguishes the fixed behavior from the
reported failure while recording the suite's exit status and memory summary.

**Acceptance Scenarios**:

1. **Given** the regression check runs on the fixed tree, **When** it completes,
   **Then** it records a successful exit and evidence that the configured heap
   budget was respected.
2. **Given** the retained-memory defect is reintroduced, **When** the same check
   runs, **Then** it fails at the test process rather than relying on an
   OS-level OOM kill.

### Edge Cases

- The suite reports an unrelated assertion failure while remaining within the
  memory budget; the assertion must remain visible and must not be classified
  as a memory regression.
- A contributor requests fewer or more capabilities explicitly; the memory
  safety behavior must remain bounded and the override must not be silently
  ignored.
- A test reaches the heap ceiling because of a single legitimate high-water
  mark rather than cumulative retention; diagnostics must still identify the
  process-local limit rather than masking the result.
- A platform does not expose peak RSS in the same way as Linux; the portable
  acceptance condition is successful completion under the heap ceiling.
- The test runner executes specs concurrently and completion order varies;
  correctness and memory safety must not depend on a particular ordering.

## Requirements

### Functional Requirements

- **FR-001**: The complete `cardano-wallet-unit` suite MUST complete with a
  2 GiB heap ceiling and its configured four-way concurrency.
- **FR-002**: The solution MUST eliminate cumulative retention of completed
  test work rather than merely raising the heap ceiling.
- **FR-003**: The solution MUST preserve the full set of tests and MUST NOT
  obtain the memory reduction by filtering, disabling, or silently skipping
  spec groups.
- **FR-004**: Each of the six affected test executables MUST include both its
  intended 2 GiB heap ceiling and four-capability runtime default.
- **FR-005**: A future heap-budget violation MUST fail in the offending test
  process with a diagnosable heap-exhaustion result before it can grow to the
  reported host-level failure mode.
- **FR-006**: Contributors MUST retain the ability to override runtime settings
  explicitly for diagnostics.
- **FR-007**: The change MUST include a repeatable, bounded full-suite proof
  that records exit status and memory-budget compliance.
- **FR-008**: Unrelated test failures, including the reported
  `UnexpectedPrefix` failure, MUST remain independently observable and MUST
  NOT be suppressed or treated as successful memory verification.
- **FR-009**: Default parallel execution MUST be preserved unless evidence
  shows it is the source of the retention and an alternative provides
  equivalent practical test throughput.

## Success Criteria

### Measurable Outcomes

- **SC-001**: The complete `cardano-wallet-unit` suite exits normally under an
  enforced 2 GiB heap ceiling with all top-level spec groups enabled.
- **SC-002**: All 6 affected executables report both intended runtime defaults
  when their compiled configuration is inspected.
- **SC-003**: Repeating the bounded full-suite check produces no OS-level OOM
  kill and no monotonic multi-gigabyte live-memory accumulation across
  completed spec groups.
- **SC-004**: A deliberately undersized diagnostic run fails with a local heap
  exhaustion exit instead of requiring host-level process termination.
- **SC-005**: The repository's formatting, build, and relevant unit-test gates
  pass without reducing test coverage.

## Assumptions

- The 2 GiB value already declared by the project is the supported heap budget;
  increasing it is out of scope unless measurements prove the fixed suite has
  a legitimate requirement above that budget.
- The six affected executables are the unit suites in `cardano-wallet-unit`,
  `cardano-wallet-read`, `crypto-primitives`, `cardano-wallet-primitive`,
  `std-gen-seed`, and `wai-middleware-logging`.
- The reported `UnexpectedPrefix` assertion is a separate defect unless the
  investigation demonstrates a causal relationship to retained memory.
- Linux provides the reference environment for detailed residency evidence;
  successful completion under the heap ceiling is the cross-platform contract.
- The fix may change the shared test harness if investigation shows that the
  defect affects more than one suite, but production wallet behavior is out of
  scope.
