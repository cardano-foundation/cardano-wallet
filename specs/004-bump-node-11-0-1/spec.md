# Feature Specification: Upgrade dependencies to cardano-node 11.0.1

**Feature Branch**: `004-bump-node-11-0-1`
**Created**: 2026-05-11
**Status**: Draft
**Input**: GitHub issue #5275: "Upgrade dependencies to cardano-node 11.0.1"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Wallet resolves and builds with node 11.0.1 dependencies (Priority: P1)

A wallet developer updates the repository dependency set to the versions expected by cardano-node 11.0.1 and gets a successful full wallet build without dependency-resolution conflicts.

**Why this priority**: The dependency set is the foundation for every downstream adaptation, test, and release artifact.

**Independent Test**: Run the repository's full build gate from a clean environment; dependency resolution and compilation complete successfully with the 11.0.1-aligned package set.

**Acceptance Scenarios**:

1. **Given** the repository is using the current cardano-wallet dependency set, **When** dependencies are aligned to cardano-node 11.0.1, **Then** the wallet resolves `cardano-api` and `cardano-cli` on the 11.x series, `cardano-ledger-conway` at `1.22.1.0` or newer, and the requested CHaP state from 2026-05-02
2. **Given** the repository build is run after the dependency alignment, **When** dependency resolution completes, **Then** no packages are selected outside the supported cardano-node 11.0.1 ecosystem
3. **Given** the aligned dependency set, **When** the wallet build gate is run from a clean environment, **Then** all wallet libraries and executables compile successfully

---

### User Story 2 - Wallet behavior survives cardano-api 11.0 changes (Priority: P2)

A wallet developer adapts the remaining wallet integration points with the upgraded Cardano package interfaces so the wallet keeps the same externally visible behavior after the `cardano-api` 11.0 upgrade.

**Why this priority**: A successful dependency resolution is not enough; the wallet must continue to build, submit, inspect, and expose transactions consistently across the upgraded upstream interface.

**Independent Test**: Run the unit and integration tests that cover transaction construction, transaction submission, delegation, voting, metadata, address, era, and local-state-query behavior; tests pass under the 11.0.1 dependency set.

**Acceptance Scenarios**:

1. **Given** wallet code paths still using upgraded Cardano interfaces, **When** the 11.0 interface changes are applied, **Then** each affected wallet behavior is either preserved or explicitly replaced by an equivalent ledger-native behavior already accepted by the project
2. **Given** the ongoing sealed-transaction decommission work, **When** this dependency upgrade is implemented, **Then** it does not broaden that separate scope beyond changes required to keep the 11.0.1 upgrade buildable and testable
3. **Given** wallet users submit and inspect transactions through existing wallet interfaces, **When** those flows run against the upgraded dependency set, **Then** their observable results remain compatible with the pre-upgrade behavior

---

### User Story 3 - Runtime and local test environments use node 11.0.1 (Priority: P3)

A wallet developer runs wallet runtime checks against a cardano-node 11.0.1 binary and verifies that the local cluster and end-to-end workflows remain compatible with the upgraded node.

**Why this priority**: Runtime compatibility catches node protocol, configuration, and local-cluster changes that a build-only validation cannot detect.

**Independent Test**: Run the local-cluster, integration, and end-to-end gates using the cardano-node 11.0.1 runtime input; tests pass or any required local-cluster prerequisite is tracked separately and linked.

**Acceptance Scenarios**:

1. **Given** the runtime node input is updated to cardano-node 11.0.1, **When** wallet integration tests start a node-backed environment, **Then** the wallet connects successfully and protocol negotiation succeeds
2. **Given** cardano-node 11.0 changes experimental hard-fork defaults and protocol-version expectations, **When** local-cluster tests are run, **Then** the required genesis or configuration compatibility is present or provided by the separate minimal local-cluster PR identified in issue #5275
3. **Given** end-to-end tests run against cardano-node 11.0.1, **When** wallet workflows complete, **Then** no regression is observed in node-backed wallet behavior

---

### User Story 4 - Maintainers can review the upgrade in controlled slices (Priority: P4)

A maintainer reviews the upgrade as an ordered stack where dependency metadata, runtime metadata, and per-library adaptations can be checked independently.

**Why this priority**: The upgrade affects many packages; reviewability reduces regression risk and makes failed gates easier to isolate.

**Independent Test**: Inspect the change stack and verify that each review slice has a clear validation result before the next slice depends on it.

**Acceptance Scenarios**:

1. **Given** the upgrade touches many wallet sublibraries, **When** the change set is prepared, **Then** each affected slice has an identifiable build, formatting, linting, or test result
2. **Given** a validation failure appears during the upgrade, **When** maintainers inspect the stack, **Then** the failure can be attributed to a bounded dependency or library slice

### Edge Cases

- The 11.0.1 dependency set may expose transitive Cardano package versions that are not visible unless the repository CHaP state matches the node release expectation.
- `cardano-api` 11.0 may remove, rename, or relocate APIs used by the remaining wallet import sites, especially in transaction, certificate, metadata, era, and local-state-query code.
- `ouroboros-consensus` and `ouroboros-network` are already on compatible major versions in the wallet; the upgrade must avoid unnecessary churn in those packages while still validating that the selected versions remain compatible.
- Local-cluster tests can fail for node-configuration reasons unrelated to wallet behavior if the cardano-node 11.0 experimental hard-fork defaults and protocol-version expectations are not reflected in genesis/configuration data.
- The ongoing sealed-transaction decommission can overlap the same files; the upgrade must preserve a clear boundary between required compatibility work and broader decommissioning work.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The wallet dependency set MUST align with cardano-node 11.0.1 as the target release for this upgrade.
- **FR-002**: The accepted `cardano-api` and `cardano-cli` versions MUST move from the 10.x series to the 11.x series required by cardano-node 11.0.1.
- **FR-003**: The accepted `cardano-ledger-conway` version MUST include `1.22.1.0` or newer when resolving the upgraded dependency set.
- **FR-004**: The Cardano Haskell Packages state visible to the wallet MUST include the CHaP index-state `2026-05-02T16:21:41Z` requested for this upgrade.
- **FR-005**: The wallet runtime node input MUST identify cardano-node 11.0.1 for node-backed validation and release artifacts.
- **FR-006**: Remaining wallet usage of upgraded Cardano package interfaces MUST be compatible with the 11.0 changes or replaced by project-approved equivalent behavior.
- **FR-007**: Existing wallet transaction, delegation, voting, metadata, era, network, and local-state-query behaviors MUST remain externally compatible unless a deliberate behavior change is documented and accepted.
- **FR-008**: The upgrade MUST preserve compatibility with the already-selected `ouroboros-consensus` and `ouroboros-network` major versions unless cardano-node 11.0.1 validation proves a version adjustment is required.
- **FR-009**: Local-cluster compatibility for cardano-node 11.0.1 MUST be either included as a prerequisite before runtime validation or linked to the separate minimal PR described in issue #5275.
- **FR-010**: The upgrade MUST provide validation evidence for dependency resolution, clean builds, formatting, linting, unit tests, integration tests, and end-to-end tests before it is considered complete.
- **FR-011**: The upgrade MUST be organized so maintainers can review dependency metadata changes separately from code adaptations and runtime/local-cluster changes.
- **FR-012**: The upgrade MUST reference the previous cardano-node 10.7.1 bump as historical context without depending on obsolete 10.7.1 constraints.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A clean dependency resolution selects cardano-node 11.0.1-aligned Cardano packages with zero solver conflicts.
- **SC-002**: The repository clean build gate completes successfully for all wallet libraries, tests, executables, and release-relevant artifacts affected by the upgrade.
- **SC-003**: Formatting and linting gates complete successfully for every changed source and dependency metadata file.
- **SC-004**: Unit and integration tests covering transaction, delegation, voting, metadata, era, network, and local-state-query behavior pass under the upgraded dependency set.
- **SC-005**: End-to-end tests against cardano-node 11.0.1 pass, or any blocking local-cluster prerequisite is completed and linked before final acceptance.
- **SC-006**: Maintainers can trace every required dependency target from issue #5275 to a resolved repository change or an explicitly scoped follow-up.

## Assumptions

- Issue #5275 is the source of truth for target scope: cardano-node 11.0.1, `cardano-api`/`cardano-cli` 11.x, `cardano-ledger-conway >= 1.22.1.0`, CHaP index-state `2026-05-02T16:21:41Z`, and cardano-node-runtime `11.0.1`.
- The previous cardano-node 10.7.1 bump from #5247/#5250 is useful precedent for process and regression checks, but this feature replaces its target dependency versions.
- The separate local-cluster genesis/configuration PR mentioned in issue #5275 may land before or alongside this upgrade; if it remains separate, this feature must consume and link it rather than duplicate it.
- The sealed-transaction decommission tracked in `specs/003-decommission-sealedtx-remainder/plan.md` remains a separate workstream except where cardano-api 11.0 compatibility requires narrow coordination.
