# Tasks: Upgrade dependencies to cardano-node 11.0.1

**Input**: Design documents from `/code/cardano-wallet/specs/004-bump-node-11-0-1/`
**Prerequisites**: [plan.md](plan.md), [spec.md](spec.md), [research.md](research.md), [data-model.md](data-model.md), [quickstart.md](quickstart.md)

**Execution Rule**: No component work runs in parallel. The implementation order is node freeze, upstream dependency gates, wallet metadata, then one Cabal component at a time in dependency-first topology. After a component closes, later tasks must not edit that component.

## Format: `[ID] [P?] [Story] Description`

- **[P]** is intentionally unused for component tasks.
- **[US1]** covers dependency resolution and buildability.
- **[US2]** covers Cardano interface/API adaptation while staying inside the current component slice.
- **[US3]** covers runtime, local-cluster, integration, and e2e validation.
- **[US4]** covers reviewability, patch stack discipline, and no-return enforcement.

## Phase 1: Node Freeze and Stack Setup

**Purpose**: Establish the target dependency source of truth before wallet edits.

- [ ] T001 [US4] Record issue #5275 targets and dirty worktree/upstream checkout status in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [ ] T002 [US1] Checkout cardano-node tag `11.0.1`, generate `/code/cardano-node/cabal.project.freeze`, and copy it to `/tmp/cardano-node-11.0.1/cabal.project.freeze`
- [ ] T003 [US1] Extract CHaP revision/index-state and target Cardano package versions from `/code/cardano-node/flake.lock` and `/tmp/cardano-node-11.0.1/cabal.project.freeze` into `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [ ] T004 [US4] Generate or confirm Cabal topology with `nix develop --quiet -c cabal-plan topo` and update `/code/cardano-wallet/specs/004-bump-node-11-0-1/plan.md` plus `/code/cardano-wallet/specs/004-bump-node-11-0-1/tasks.md` if the order differs
- [ ] T005 [US4] Create the stgit stack described by this task list from `/code/cardano-wallet`, with one patch for metadata, one patch per upstream pin update, and one patch per Cabal component

**Checkpoint**: The node freeze exists, target versions are recorded, and the implementation stack is ordered before code edits.

---

## Phase 2: Upstream Dependency Gates

**Purpose**: Adapt dependencies that wallet consumes through `source-repository-package` before local wallet consumers are closed.

- [ ] T006 [US1] Adapt `cardano-ledger-read` central constraints in `/code/cardano-ledger-read/cabal.project` and only non-version package metadata in `/code/cardano-ledger-read/cardano-ledger-read.cabal` to the node 11.0.1 freeze
- [ ] T007 [US2] Fix and validate `cardano-ledger-read` source/tests under `/code/cardano-ledger-read/`, then record the resulting commit/tag and validation evidence in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [ ] T008 [US1] Adapt `cardano-balance-transaction` central constraints in `/code/cardano-balance-transaction/cabal.project` and only non-version package metadata in `/code/cardano-balance-transaction/cardano-balance-tx.cabal` to the node 11.0.1 freeze
- [ ] T009 [US2] Fix and validate `cardano-balance-transaction` source/tests under `/code/cardano-balance-transaction/`, then record the resulting commit/tag and validation evidence in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [ ] T010 [US1] Prefetch the validated upstream commits and update wallet source-repository-package tags and sha256 values for `cardano-ledger-read` and `cardano-balance-tx` in `/code/cardano-wallet/cabal.project`

**Checkpoint**: Wallet consumers will see the final upstream dependency surface before local component work starts.

---

## Phase 3: Wallet Dependency Metadata

**Purpose**: Align wallet-wide metadata to the node freeze before component slices.

- [ ] T011 [US1] Update CHaP index-state, Cardano package constraints, and solver metadata from `/tmp/cardano-node-11.0.1/cabal.project.freeze` in `/code/cardano-wallet/cabal.project`
- [ ] T012 [US1] Update `cardano-node-runtime` to 11.0.1 and align CHaP/runtime locks in `/code/cardano-wallet/flake.nix` and `/code/cardano-wallet/flake.lock`
- [ ] T013 [US1] Run wallet dependency resolution from `/code/cardano-wallet/cabal.project`, explicitly verify whether `ouroboros-consensus` and `ouroboros-network` remain on the already-selected compatible major versions, and record any package removals, replacements, or freeze-driven ouroboros adjustment in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [ ] T014 [US4] Refresh the metadata patch and verify no component-owned source directory under `/code/cardano-wallet/lib/` has been edited yet

**Checkpoint**: Solver metadata is ready and component ownership boundaries are still clean.

---

## Phase 4: Component Vertical Slices

**Purpose**: Fix one Cabal component at a time in dependency-first topology. Each task owns the component `.cabal` file and files under the listed component directory; `.cabal` edits must not introduce version bounds. Before refreshing the component patch/commit, record the exact close commands in the Component Closure Evidence table, run formatting, linting, the recorded Nix build/test gate, and green unit tests whenever the component declares a Cabal `test-suite`. The component's unit-test fixes and green unit-test evidence must be committed together with that component. Then do not edit that component again.

### Component Closure Evidence

Before each component task T015-T046 is marked complete, add the exact commands used to close that component. A row is not valid with placeholders. If the component has a Cabal `test-suite`, the evidence must include the exact unit-test command and a green result; if it has no test suite, write `no Cabal test-suite` explicitly.

| Task | Component | Close command evidence |
|------|-----------|------------------------|
| T015 | cardano-wallet-read | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T016 | delta-types | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T017 | text-class | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T018 | cardano-numeric | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T019 | std-gen-seed | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T020 | cardano-wallet-test-utils | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T021 | crypto-primitives | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T022 | faucet | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T023 | flaky-tests | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T024 | iohk-monitoring-extra | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T025 | cardano-wallet-launcher | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T026 | delta-store | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T027 | wai-middleware-logging | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T028 | cardano-wallet-secrets | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T029 | cardano-wallet-application-extras | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T030 | cardano-wallet-application-tls | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T031 | temporary-extra | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T032 | delta-chain | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T033 | delta-table | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T034 | cardano-api-extra | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T035 | cardano-wallet-primitive | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T036 | address-derivation-discovery | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T037 | cardano-wallet-network-layer | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T038 | cardano-wallet | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T039 | local-cluster | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T040 | cardano-wallet-api | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T041 | cardano-wallet-ui | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T042 | cardano-wallet-application | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T043 | cardano-wallet-integration | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T044 | cardano-wallet-unit | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T045 | cardano-wallet-benchmarks | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T046 | cardano-wallet-blackbox-benchmarks | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |

- [ ] T015 [US1] Adapt and close `cardano-wallet-read` in `/code/cardano-wallet/lib/cardano-wallet-read/cardano-wallet-read.cabal` and `/code/cardano-wallet/lib/cardano-wallet-read/`
- [ ] T016 [US1] Adapt and close `delta-types` in `/code/cardano-wallet/lib/delta-types/delta-types.cabal` and `/code/cardano-wallet/lib/delta-types/`
- [ ] T017 [US1] Adapt and close `text-class` in `/code/cardano-wallet/lib/text-class/text-class.cabal` and `/code/cardano-wallet/lib/text-class/`
- [ ] T018 [US1] Adapt and close `cardano-numeric` in `/code/cardano-wallet/lib/numeric/cardano-numeric.cabal` and `/code/cardano-wallet/lib/numeric/`
- [ ] T019 [US1] Adapt and close `std-gen-seed` in `/code/cardano-wallet/lib/std-gen-seed/std-gen-seed.cabal` and `/code/cardano-wallet/lib/std-gen-seed/`
- [ ] T020 [US1] Adapt and close `cardano-wallet-test-utils` in `/code/cardano-wallet/lib/test-utils/cardano-wallet-test-utils.cabal` and `/code/cardano-wallet/lib/test-utils/`
- [ ] T021 [US1] Adapt and close `crypto-primitives` in `/code/cardano-wallet/lib/crypto-primitives/crypto-primitives.cabal` and `/code/cardano-wallet/lib/crypto-primitives/`
- [ ] T022 [US1] Adapt and close `faucet` in `/code/cardano-wallet/lib/faucet/faucet.cabal` and `/code/cardano-wallet/lib/faucet/`
- [ ] T023 [US1] Adapt and close `flaky-tests` in `/code/cardano-wallet/lib/flaky-tests/flaky-tests.cabal` and `/code/cardano-wallet/lib/flaky-tests/`
- [ ] T024 [US1] Adapt and close `iohk-monitoring-extra` in `/code/cardano-wallet/lib/iohk-monitoring-extra/iohk-monitoring-extra.cabal` and `/code/cardano-wallet/lib/iohk-monitoring-extra/`
- [ ] T025 [US1] Adapt and close `cardano-wallet-launcher` in `/code/cardano-wallet/lib/launcher/cardano-wallet-launcher.cabal` and `/code/cardano-wallet/lib/launcher/`
- [ ] T026 [US1] Adapt and close `delta-store` in `/code/cardano-wallet/lib/delta-store/delta-store.cabal` and `/code/cardano-wallet/lib/delta-store/`
- [ ] T027 [US1] Adapt and close `wai-middleware-logging` in `/code/cardano-wallet/lib/wai-middleware-logging/wai-middleware-logging.cabal` and `/code/cardano-wallet/lib/wai-middleware-logging/`
- [ ] T028 [US1] Adapt and close `cardano-wallet-secrets` in `/code/cardano-wallet/lib/secrets/cardano-wallet-secrets.cabal` and `/code/cardano-wallet/lib/secrets/`
- [ ] T029 [US1] Adapt and close `cardano-wallet-application-extras` in `/code/cardano-wallet/lib/application-extras/cardano-wallet-application-extras.cabal` and `/code/cardano-wallet/lib/application-extras/`
- [ ] T030 [US1] Adapt and close `cardano-wallet-application-tls` in `/code/cardano-wallet/lib/application-tls/cardano-wallet-application-tls.cabal` and `/code/cardano-wallet/lib/application-tls/`
- [ ] T031 [US1] Adapt and close `temporary-extra` in `/code/cardano-wallet/lib/temporary-extra/temporary-extra.cabal` and `/code/cardano-wallet/lib/temporary-extra/`
- [ ] T032 [US1] Adapt and close `delta-chain` in `/code/cardano-wallet/lib/delta-chain/delta-chain.cabal` and `/code/cardano-wallet/lib/delta-chain/`
- [ ] T033 [US1] Adapt and close `delta-table` in `/code/cardano-wallet/lib/delta-table/delta-table.cabal` and `/code/cardano-wallet/lib/delta-table/`
- [ ] T034 [US2] Adapt and close `cardano-api-extra` in `/code/cardano-wallet/lib/cardano-api-extra/cardano-api-extra.cabal` and `/code/cardano-wallet/lib/cardano-api-extra/`
- [ ] T035 [US2] Adapt and close `cardano-wallet-primitive` in `/code/cardano-wallet/lib/primitive/cardano-wallet-primitive.cabal` and `/code/cardano-wallet/lib/primitive/`
- [ ] T036 [US2] Adapt and close `address-derivation-discovery` in `/code/cardano-wallet/lib/address-derivation-discovery/address-derivation-discovery.cabal` and `/code/cardano-wallet/lib/address-derivation-discovery/`
- [ ] T037 [US2] Adapt and close `cardano-wallet-network-layer` in `/code/cardano-wallet/lib/network-layer/cardano-wallet-network-layer.cabal` and `/code/cardano-wallet/lib/network-layer/`
- [ ] T038 [US2] Adapt and close `cardano-wallet` in `/code/cardano-wallet/lib/wallet/cardano-wallet.cabal` and `/code/cardano-wallet/lib/wallet/`
- [ ] T039 [US3] Adapt and close `local-cluster` in `/code/cardano-wallet/lib/local-cluster/local-cluster.cabal` and `/code/cardano-wallet/lib/local-cluster/`, consuming the separate node 11.0.1 local-cluster PR if available
- [ ] T040 [US2] Adapt and close `cardano-wallet-api` in `/code/cardano-wallet/lib/api/cardano-wallet-api.cabal` and `/code/cardano-wallet/lib/api/`
- [ ] T041 [US2] Adapt and close `cardano-wallet-ui` in `/code/cardano-wallet/lib/ui/cardano-wallet-ui.cabal` and `/code/cardano-wallet/lib/ui/`
- [ ] T042 [US2] Adapt and close `cardano-wallet-application` in `/code/cardano-wallet/lib/application/cardano-wallet-application.cabal` and `/code/cardano-wallet/lib/application/`
- [ ] T043 [US3] Adapt and close `cardano-wallet-integration` in `/code/cardano-wallet/lib/integration/cardano-wallet-integration.cabal` and `/code/cardano-wallet/lib/integration/`
- [ ] T044 [US2] Adapt and close `cardano-wallet-unit` in `/code/cardano-wallet/lib/unit/cardano-wallet-unit.cabal` and `/code/cardano-wallet/lib/unit/`
- [ ] T045 [US4] Adapt and close `cardano-wallet-benchmarks` in `/code/cardano-wallet/lib/benchmarks/cardano-wallet-benchmarks.cabal` and `/code/cardano-wallet/lib/benchmarks/`
- [ ] T046 [US4] Adapt and close `cardano-wallet-blackbox-benchmarks` in `/code/cardano-wallet/lib/wallet-benchmarks/cardano-wallet-blackbox-benchmarks.cabal` and `/code/cardano-wallet/lib/wallet-benchmarks/`

**Checkpoint**: Every local Cabal component is closed once, in order, with no backtracking edits. Every component that declares a Cabal `test-suite` is committed together with green unit tests for that component.

---

## Phase 5: Runtime and Whole-Repository Validation

**Purpose**: Validate that the vertically closed stack works as a complete wallet system.

- [ ] T047 [US3] Run the local-cluster build/test gate for node 11.0.1 using `/code/cardano-wallet/lib/local-cluster/` and record whether the separate local-cluster prerequisite PR was consumed
- [ ] T048 [US3] Run integration executable and integration test gates from `/code/cardano-wallet/lib/integration/cardano-wallet-integration.cabal`
- [ ] T049 [US3] Run end-to-end tests against cardano-node 11.0.1 from `/code/cardano-wallet/flake.nix` runtime outputs
- [ ] T050 [US1] Run the final clean repository build gate from `/code/cardano-wallet/cabal.project` and `/code/cardano-wallet/flake.nix`
- [ ] T051 [US4] Audit `git diff --name-only` from `/code/cardano-wallet` against the component order in `/code/cardano-wallet/specs/004-bump-node-11-0-1/tasks.md` and document any reopened component before review

**Checkpoint**: The upgrade is reviewable, validated, and traceable back to issue #5275.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1** blocks everything: no freeze, no target dependency truth.
- **Phase 2** depends on Phase 1: upstream wallet dependencies must validate before local consumers close.
- **Phase 3** depends on Phase 2: wallet metadata must point at the final upstream dependency surface.
- **Phase 4** depends on Phase 3: component slices run strictly in listed order.
- **Phase 5** depends on Phase 4: whole-repository gates run only after all components close.

### Component No-Return Rule

When T015 closes, no later task may edit `/code/cardano-wallet/lib/cardano-wallet-read/`. The same rule applies to every component task T016 through T046. A later failure that truly belongs to a closed component requires stopping, documenting the reopen in this file, and correcting the patch stack before continuing.

### Parallel Opportunities

None for component implementation. The only safe parallel work is read-only investigation before Phase 1 or independent upstream repo preparation before wallet pins are updated, and even that must not close wallet consumer components out of order.

## Implementation Strategy

1. Freeze node 11.0.1.
2. Fix upstream dependencies.
3. Align wallet metadata.
4. Close components one by one in dependency-first topology.
5. Run runtime and whole-repository gates.
