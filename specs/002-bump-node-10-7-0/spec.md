# Feature Specification: Bump dependencies to cardano-node 10.7.0

**Feature Branch**: `002-bump-node-10-7-0`
**Created**: 2026-04-15
**Status**: Draft
**Input**: Bump all Cardano dependency constraints and .cabal version bounds to match cardano-node 10.7.0

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Wallet builds against node 10.7.0 dependencies (Priority: P1)

A developer checks out the wallet repo and runs `cabal build all`. The build succeeds with all dependency versions aligned to cardano-node 10.7.0.

**Why this priority**: Without a successful build, no other work is possible.

**Independent Test**: Run `cabal build all -O0` in nix develop shell. Build succeeds with no version conflicts.

**Acceptance Scenarios**:

1. **Given** updated cabal.project constraints and .cabal bounds, **When** `cabal build all -O0` is run, **Then** the build resolves and compiles successfully
2. **Given** the ouroboros package consolidation (consensus-cardano/diffusion/protocol absorbed into ouroboros-consensus 1.0.0.0, network-api/framework/protocols absorbed into ouroboros-network 1.1.0.0), **When** building, **Then** no references to removed packages remain

---

### User Story 2 - E2E tests pass against node 10.7.0 (Priority: P2)

The E2E test suite runs against a cardano-node 10.7.0 binary on preprod and passes.

**Why this priority**: Confirms runtime compatibility beyond just compilation.

**Independent Test**: Run `just e2e` with the bumped flake input. Tests pass.

**Acceptance Scenarios**:

1. **Given** cardano-node-runtime flake input set to 10.7.0, **When** `just e2e` runs, **Then** all E2E tests pass
2. **Given** the new NodeToClientV_23 protocol version, **When** wallet connects to node, **Then** protocol negotiation succeeds

---

### Edge Cases

- Ouroboros packages that were standalone are now sublibraries — imports and cabal deps must be rewritten, not just version-bumped
- `bump-constraints.sh` may not handle removed packages (ouroboros-consensus-cardano, etc.) — manual cleanup needed
- `cardano-ledger-read` and `cardano-balance-tx` (source-repository-package pins) may need their own bumps if they depend on the new ledger versions
- `Tx Size` type changed from `Integer` to `Word32` — any wallet code using this type needs updating

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: `cabal.project` constraints MUST be updated to match cardano-node 10.7.0 freeze file versions
- **FR-002**: `cabal.project` index-state MUST align with cardano-node 10.7.0's CHaP revision
- **FR-003**: All `.cabal` files MUST have version bounds updated via `bump-constraints.sh` using the node 10.7.0 freeze file
- **FR-004**: References to absorbed ouroboros packages (ouroboros-consensus-cardano, ouroboros-consensus-diffusion, ouroboros-consensus-protocol, ouroboros-network-api, ouroboros-network-framework, ouroboros-network-protocols) MUST be removed or replaced
- **FR-005**: `flake.nix` cardano-node-runtime input MUST point to 10.7.0
- **FR-006**: `flake.lock` CHaP input MUST match cardano-node 10.7.0's CHaP rev (887d73ce)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `cabal build all -O0` succeeds with zero version conflicts
- **SC-002**: `just e2e` passes against node 10.7.0
- **SC-003**: Zero references to removed ouroboros packages remain in .cabal files or cabal.project

## Assumptions

- The freeze file at `/tmp/cardano-node-10.7.0/cabal.project.freeze` is the source of truth for target versions
- `bump-constraints.sh` handles version bound updates for packages that still exist; removed packages need manual handling
- Source-repository-package pins (cardano-ledger-read, cardano-balance-tx, cardano-coin-selection) may need separate bumps
- The `updating-dependencies.md` doc defines the canonical process: freeze, topo order, bump-constraints.sh, manual fixes
- Code changes to adapt to API breakages (LedgerDB V2, submitTxToNodeLocal, WrapTx removal, etc.) are out of scope for the dependency bump itself — they will be addressed in follow-up commits
