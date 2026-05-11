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

- [X] T001 [US4] Record issue #5275 targets and dirty worktree/upstream checkout status in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [X] T002 [US1] Checkout cardano-node tag `11.0.1`, generate `/code/cardano-node/cabal.project.freeze`, and copy it to `/tmp/cardano-node-11.0.1/cabal.project.freeze`
- [X] T003 [US1] Extract CHaP revision/index-state and target Cardano package versions from `/code/cardano-node/flake.lock` and `/tmp/cardano-node-11.0.1/cabal.project.freeze` into `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [X] T004 [US4] Generate or confirm the all-component Cabal topology from `dist-newstyle/cache/plan.json`, including local test/exe/bench dependencies, and update `/code/cardano-wallet/specs/004-bump-node-11-0-1/plan.md` plus `/code/cardano-wallet/specs/004-bump-node-11-0-1/tasks.md` if the order differs
- [X] T005 [US4] Initialize the stgit stack from `/code/cardano-wallet`, with existing documentation, upstream pin, metadata, and topology patches imported; create exactly one new patch per Cabal component when that component task starts

**Checkpoint**: The node freeze exists, target versions are recorded, and the implementation stack is ordered before code edits.

---

## Phase 2: Upstream Dependency Gates

**Purpose**: Adapt dependencies that wallet consumes through `source-repository-package` before local wallet consumers are closed.

- [X] T006 [US1] Adapt `cardano-ledger-read` central constraints in `/code/cardano-ledger-read/cabal.project` and only non-version package metadata in `/code/cardano-ledger-read/cardano-ledger-read.cabal` to the node 11.0.1 freeze
- [X] T007 [US2] Fix and validate `cardano-ledger-read` source/tests under `/code/cardano-ledger-read/`, then record the resulting commit/tag and validation evidence in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [X] T008 [US1] Adapt `cardano-balance-transaction` central constraints in `/code/cardano-balance-transaction/cabal.project` and only non-version package metadata in `/code/cardano-balance-transaction/cardano-balance-tx.cabal` to the node 11.0.1 freeze
- [X] T009 [US2] Fix and validate `cardano-balance-transaction` source/tests under `/code/cardano-balance-transaction/`, then record the resulting commit/tag and validation evidence in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [X] T010 [US1] Prefetch the validated upstream commits and update wallet source-repository-package tags and sha256 values for `cardano-ledger-read` and `cardano-balance-tx` in `/code/cardano-wallet/cabal.project`

**Checkpoint**: Wallet consumers will see the final upstream dependency surface before local component work starts.

---

## Phase 3: Wallet Dependency Metadata

**Purpose**: Align wallet-wide metadata to the node freeze before component slices.

- [X] T011 [US1] Update CHaP index-state, Cardano package constraints, and solver metadata from `/tmp/cardano-node-11.0.1/cabal.project.freeze` in `/code/cardano-wallet/cabal.project`
- [X] T012 [US1] Update `cardano-node-runtime` to 11.0.1 and align CHaP/runtime locks in `/code/cardano-wallet/flake.nix` and `/code/cardano-wallet/flake.lock`
- [X] T013 [US1] Run wallet dependency resolution from `/code/cardano-wallet/cabal.project`, explicitly verify whether `ouroboros-consensus` and `ouroboros-network` remain on the already-selected compatible major versions, and record any package removals, replacements, or freeze-driven ouroboros adjustment in `/code/cardano-wallet/specs/004-bump-node-11-0-1/research.md`
- [X] T014 [US4] Refresh the metadata patch and verify no component-owned source directory under `/code/cardano-wallet/lib/` has been edited yet

**Checkpoint**: Solver metadata is ready and component ownership boundaries are still clean.

---

## Phase 4: Component Vertical Slices

**Purpose**: Fix one Cabal component at a time in dependency-first topology. Each task owns the component `.cabal` file and files under the listed component directory; `.cabal` edits must not introduce version bounds. Before refreshing the component patch/commit, record the exact close commands in the Component Closure Evidence table, run formatting, linting, the recorded Nix build/test gate, and green unit tests whenever the component declares a Cabal `test-suite`. The component's unit-test fixes and green unit-test evidence must be committed together with that component. Then do not edit that component again.

### Component Closure Evidence

Before each component task T015-T046 is marked complete, add the exact commands used to close that component. A row is not valid with placeholders. If the component has a Cabal `test-suite`, the evidence must include the exact unit-test command and a green result; if it has no test suite, write `no Cabal test-suite` explicitly.

| Task | Component | Close command evidence |
|------|-----------|------------------------|
| T015 | cardano-numeric | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c fourmolu --mode check lib/numeric/src/Cardano/Numeric/Util.hs lib/numeric/test/unit/Cardano/Numeric/UtilSpec.hs lib/numeric/test/unit/numeric-unit-test.hs`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/numeric/src lib/numeric/test/unit`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/numeric/cardano-numeric.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link .#packages.x86_64-linux.tests.cardano-numeric.unit`; `nix build --accept-flake-config --allow-import-from-derivation --no-link .#checks.x86_64-linux.cardano-numeric.unit`; `/nix/store/wamyrg4404svcizmamy7h1x7x012pf4x-cardano-numeric-test-unit-2020.12.8/bin/unit` -> 11 examples, 0 failures |
| T016 | text-class | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c fourmolu --mode check lib/text-class/src/Data/Text/Class.hs lib/text-class/src/Test/Text/Roundtrip.hs lib/text-class/test/unit/Data/Text/ClassSpec.hs lib/text-class/test/unit/text-class-unit-test.hs`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/text-class/src lib/text-class/test/unit`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/text-class/text-class.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link .#packages.x86_64-linux.tests.text-class.unit`; `nix build --accept-flake-config --allow-import-from-derivation --no-link .#checks.x86_64-linux.text-class.unit`; `/nix/store/pgjg3b5b1rs85ix77c9w4jj8hb0l0jrs-text-class-test-unit-0.2026.4.17/bin/unit` -> 27 examples, 0 failures |
| T017 | cardano-wallet-read | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/cardano-wallet-read/haskell/**/*.hs lib/cardano-wallet-read/test/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/cardano-wallet-read/haskell lib/cardano-wallet-read/test` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/cardano-wallet-read/cardano-wallet-read.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-read.test` -> `/nix/store/845kr0zrbwha60mqp6f84cxac826pnhn-cardano-wallet-read-test-test-1.0.0.1`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-read.test` -> `/nix/store/lyyn7dim70b3365dkxrw7cq3ilnc309x-cardano-wallet-read-test-test-1.0.0.1-check`; `/nix/store/845kr0zrbwha60mqp6f84cxac826pnhn-cardano-wallet-read-test-test-1.0.0.1/bin/test` -> 24 examples, 0 failures |
| T018 | cardano-wallet-test-utils | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/test-utils/src/**/*.hs lib/test-utils/test/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/test-utils/src lib/test-utils/test` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/test-utils/cardano-wallet-test-utils.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-test-utils.unit` -> `/nix/store/angyzm0ic61fg8y6f74xznigw5w3jndn-cardano-wallet-test-utils-test-unit-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-test-utils.unit` -> `/nix/store/zx5h1fh89in4ra5ypn9gs6b85j6vcgqw-cardano-wallet-test-utils-test-unit-0.2026.4.17-check`; `/nix/store/angyzm0ic61fg8y6f74xznigw5w3jndn-cardano-wallet-test-utils-test-unit-0.2026.4.17/bin/unit` -> 70 examples, 0 failures |
| T019 | cardano-wallet-launcher | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/launcher/src/**/*.hs lib/launcher/test/unit/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/launcher/src lib/launcher/test/unit` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/launcher/cardano-wallet-launcher.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-launcher.unit` -> `/nix/store/z6dzabzfv6z27jlswkf2zs8ki9nblhag-cardano-wallet-launcher-test-unit-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-launcher.unit` -> `/nix/store/72b2ayl3ypr6z7g30wp7hi55cn441lqa-cardano-wallet-launcher-test-unit-0.2026.4.17-check`; `/nix/store/z6dzabzfv6z27jlswkf2zs8ki9nblhag-cardano-wallet-launcher-test-unit-0.2026.4.17/bin/unit` -> 23 examples, 0 failures |
| T020 | crypto-primitives | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/crypto-primitives/src/**/*.hs lib/crypto-primitives/test/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/crypto-primitives/src lib/crypto-primitives/test` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/crypto-primitives/crypto-primitives.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.crypto-primitives.test` -> `/nix/store/xm1qibz2lg51rc2r42yv3s4j72gxa017-crypto-primitives-test-test-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.crypto-primitives.test` -> `/nix/store/ik84cqyibghazf1k8l67916qd1bb2g3s-crypto-primitives-test-test-0.2026.4.17-check`; `/nix/store/xm1qibz2lg51rc2r42yv3s4j72gxa017-crypto-primitives-test-test-0.2026.4.17/bin/test` -> 49 examples, 0 failures |
| T021 | delta-types | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/delta-types/src/**/*.hs lib/delta-types/test/unit/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/delta-types/src lib/delta-types/test/unit` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/delta-types/delta-types.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.delta-types.unit` -> `/nix/store/4s0viww384dns8w3gmwqwcfjkxxy66rn-delta-types-test-unit-1.0.0.0`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.delta-types.unit` -> `/nix/store/26iixfxg9p60nmf5yc6v87qm5s3qaa1n-delta-types-test-unit-1.0.0.0-check`; `/nix/store/4s0viww384dns8w3gmwqwcfjkxxy66rn-delta-types-test-unit-1.0.0.0/bin/unit` -> 1 example, 0 failures |
| T022 | cardano-wallet-primitive | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/primitive/lib/**/*.hs lib/primitive/test/data/**/*.hs lib/primitive/test/spec/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/primitive/lib lib/primitive/test/data lib/primitive/test/spec` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/primitive/cardano-wallet-primitive.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-primitive.test` -> `/nix/store/pnd771ayq6hkcqlbm9kx9n2f07r48hpz-cardano-wallet-primitive-test-test-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-primitive.test` -> `/nix/store/9b8w9kz4qcyqa14nqlwp8sj4hfrhpbq8-cardano-wallet-primitive-test-test-0.2026.4.17-check`; `cd /code/cardano-wallet/lib/primitive && /nix/store/pnd771ayq6hkcqlbm9kx9n2f07r48hpz-cardano-wallet-primitive-test-test-0.2026.4.17/bin/test` -> 450 examples, 0 failures |
| T023 | cardano-wallet-secrets | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/secrets/src/**/*.hs lib/secrets/test/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/secrets/src lib/secrets/test` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/secrets/cardano-wallet-secrets.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-secrets.test` -> `/nix/store/s8p23dxn49qd6wn541r7agjrh74l9q2w-cardano-wallet-secrets-test-test-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-secrets.test` -> `/nix/store/xdijrvf471a56f21dh7mbaa2v5vv6j3m-cardano-wallet-secrets-test-test-0.2026.4.17-check`; `/nix/store/s8p23dxn49qd6wn541r7agjrh74l9q2w-cardano-wallet-secrets-test-test-0.2026.4.17/bin/test` -> 21 examples, 0 failures |
| T024 | address-derivation-discovery | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/address-derivation-discovery/lib/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/address-derivation-discovery/lib` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/address-derivation-discovery/address-derivation-discovery.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#legacyPackages.x86_64-linux.hsPkgs.address-derivation-discovery.components.library` -> `/nix/store/zgs1hgwais0f2bhgvb256a9x1w01whcr-address-derivation-discovery-lib-address-derivation-discovery-0.2026.4.17`; no Cabal test-suite |
| T025 | cardano-api-extra | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/cardano-api-extra/lib/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/cardano-api-extra/lib` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/cardano-api-extra/cardano-api-extra.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#legacyPackages.x86_64-linux.hsPkgs.cardano-api-extra.components.library` -> `/nix/store/a7irhfqyddna9ysby6ri1gzp5irrdr1a-cardano-api-extra-lib-cardano-api-extra-0.2026.4.17`; no Cabal test-suite |
| T026 | iohk-monitoring-extra | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/iohk-monitoring-extra/src/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/iohk-monitoring-extra/src` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/iohk-monitoring-extra/iohk-monitoring-extra.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#legacyPackages.x86_64-linux.hsPkgs.iohk-monitoring-extra.components.library` -> `/nix/store/4m6xsfjhdk8l584m2fhmd3720wzifn3y-iohk-monitoring-extra-lib-iohk-monitoring-extra-0.2026.4.17`; no Cabal test-suite |
| T027 | cardano-wallet-network-layer | `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c bash -lc 'shopt -s globstar; fourmolu --mode check lib/network-layer/src/**/*.hs lib/network-layer/test/**/*.hs'`; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c hlint lib/network-layer/src lib/network-layer/test` -> No hints; `nix develop --accept-flake-config --allow-import-from-derivation --quiet -c cabal-fmt --check lib/network-layer/cardano-wallet-network-layer.cabal`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#packages.x86_64-linux.tests.cardano-wallet-network-layer.unit` -> `/nix/store/0dw4xbi8dj8dzdxvcc7qa40h44ykcjcb-cardano-wallet-network-layer-test-unit-0.2026.4.17`; `nix build --accept-flake-config --allow-import-from-derivation --no-link --print-out-paths .#checks.x86_64-linux.cardano-wallet-network-layer.unit` -> `/nix/store/mgyci1gkgridim6qbznj7sb2s4wwkxm3-cardano-wallet-network-layer-test-unit-0.2026.4.17-check`; `/nix/store/0dw4xbi8dj8dzdxvcc7qa40h44ykcjcb-cardano-wallet-network-layer-test-unit-0.2026.4.17/bin/unit` -> 8 examples, 0 failures |
| T028 | delta-store | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T029 | cardano-wallet | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T030 | cardano-wallet-api | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T031 | cardano-wallet-application-tls | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T032 | wai-middleware-logging | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T033 | cardano-wallet-ui | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T034 | cardano-wallet-application | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T035 | cardano-wallet-application-extras | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T036 | faucet | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T037 | temporary-extra | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T038 | local-cluster | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T039 | cardano-wallet-integration | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T040 | cardano-wallet-unit | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T041 | cardano-wallet-benchmarks | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T042 | cardano-wallet-blackbox-benchmarks | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T043 | delta-chain | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T044 | delta-table | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T045 | flaky-tests | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |
| T046 | std-gen-seed | Must record exact fourmolu, hlint, cabal-fmt, Nix, and unit-test commands before marking this task complete |

- [X] T015 [US1] Adapt and close `cardano-numeric` in `/code/cardano-wallet/lib/numeric/cardano-numeric.cabal` and `/code/cardano-wallet/lib/numeric/`
- [X] T016 [US1] Adapt and close `text-class` in `/code/cardano-wallet/lib/text-class/text-class.cabal` and `/code/cardano-wallet/lib/text-class/`
- [X] T017 [US1] Adapt and close `cardano-wallet-read` in `/code/cardano-wallet/lib/cardano-wallet-read/cardano-wallet-read.cabal` and `/code/cardano-wallet/lib/cardano-wallet-read/`
- [X] T018 [US1] Adapt and close `cardano-wallet-test-utils` in `/code/cardano-wallet/lib/test-utils/cardano-wallet-test-utils.cabal` and `/code/cardano-wallet/lib/test-utils/`
- [X] T019 [US1] Adapt and close `cardano-wallet-launcher` in `/code/cardano-wallet/lib/launcher/cardano-wallet-launcher.cabal` and `/code/cardano-wallet/lib/launcher/`
- [X] T020 [US1] Adapt and close `crypto-primitives` in `/code/cardano-wallet/lib/crypto-primitives/crypto-primitives.cabal` and `/code/cardano-wallet/lib/crypto-primitives/`
- [X] T021 [US1] Adapt and close `delta-types` in `/code/cardano-wallet/lib/delta-types/delta-types.cabal` and `/code/cardano-wallet/lib/delta-types/`
- [X] T022 [US2] Adapt and close `cardano-wallet-primitive` in `/code/cardano-wallet/lib/primitive/cardano-wallet-primitive.cabal` and `/code/cardano-wallet/lib/primitive/`
- [X] T023 [US1] Adapt and close `cardano-wallet-secrets` in `/code/cardano-wallet/lib/secrets/cardano-wallet-secrets.cabal` and `/code/cardano-wallet/lib/secrets/`
- [X] T024 [US2] Adapt and close `address-derivation-discovery` in `/code/cardano-wallet/lib/address-derivation-discovery/address-derivation-discovery.cabal` and `/code/cardano-wallet/lib/address-derivation-discovery/`
- [X] T025 [US2] Adapt and close `cardano-api-extra` in `/code/cardano-wallet/lib/cardano-api-extra/cardano-api-extra.cabal` and `/code/cardano-wallet/lib/cardano-api-extra/`
- [X] T026 [US1] Adapt and close `iohk-monitoring-extra` in `/code/cardano-wallet/lib/iohk-monitoring-extra/iohk-monitoring-extra.cabal` and `/code/cardano-wallet/lib/iohk-monitoring-extra/`
- [X] T027 [US2] Adapt and close `cardano-wallet-network-layer` in `/code/cardano-wallet/lib/network-layer/cardano-wallet-network-layer.cabal` and `/code/cardano-wallet/lib/network-layer/`
- [ ] T028 [US1] Adapt and close `delta-store` in `/code/cardano-wallet/lib/delta-store/delta-store.cabal` and `/code/cardano-wallet/lib/delta-store/`
- [ ] T029 [US2] Adapt and close `cardano-wallet` in `/code/cardano-wallet/lib/wallet/cardano-wallet.cabal` and `/code/cardano-wallet/lib/wallet/`
- [ ] T030 [US2] Adapt and close `cardano-wallet-api` in `/code/cardano-wallet/lib/api/cardano-wallet-api.cabal` and `/code/cardano-wallet/lib/api/`
- [ ] T031 [US1] Adapt and close `cardano-wallet-application-tls` in `/code/cardano-wallet/lib/application-tls/cardano-wallet-application-tls.cabal` and `/code/cardano-wallet/lib/application-tls/`
- [ ] T032 [US1] Adapt and close `wai-middleware-logging` in `/code/cardano-wallet/lib/wai-middleware-logging/wai-middleware-logging.cabal` and `/code/cardano-wallet/lib/wai-middleware-logging/`
- [ ] T033 [US2] Adapt and close `cardano-wallet-ui` in `/code/cardano-wallet/lib/ui/cardano-wallet-ui.cabal` and `/code/cardano-wallet/lib/ui/`
- [ ] T034 [US2] Adapt and close `cardano-wallet-application` in `/code/cardano-wallet/lib/application/cardano-wallet-application.cabal` and `/code/cardano-wallet/lib/application/`
- [ ] T035 [US1] Adapt and close `cardano-wallet-application-extras` in `/code/cardano-wallet/lib/application-extras/cardano-wallet-application-extras.cabal` and `/code/cardano-wallet/lib/application-extras/`
- [ ] T036 [US1] Adapt and close `faucet` in `/code/cardano-wallet/lib/faucet/faucet.cabal` and `/code/cardano-wallet/lib/faucet/`
- [ ] T037 [US1] Adapt and close `temporary-extra` in `/code/cardano-wallet/lib/temporary-extra/temporary-extra.cabal` and `/code/cardano-wallet/lib/temporary-extra/`
- [ ] T038 [US3] Adapt and close `local-cluster` in `/code/cardano-wallet/lib/local-cluster/local-cluster.cabal` and `/code/cardano-wallet/lib/local-cluster/`, consuming the separate node 11.0.1 local-cluster PR if available
- [ ] T039 [US3] Adapt and close `cardano-wallet-integration` in `/code/cardano-wallet/lib/integration/cardano-wallet-integration.cabal` and `/code/cardano-wallet/lib/integration/`
- [ ] T040 [US2] Adapt and close `cardano-wallet-unit` in `/code/cardano-wallet/lib/unit/cardano-wallet-unit.cabal` and `/code/cardano-wallet/lib/unit/`
- [ ] T041 [US4] Adapt and close `cardano-wallet-benchmarks` in `/code/cardano-wallet/lib/benchmarks/cardano-wallet-benchmarks.cabal` and `/code/cardano-wallet/lib/benchmarks/`
- [ ] T042 [US4] Adapt and close `cardano-wallet-blackbox-benchmarks` in `/code/cardano-wallet/lib/wallet-benchmarks/cardano-wallet-blackbox-benchmarks.cabal` and `/code/cardano-wallet/lib/wallet-benchmarks/`
- [ ] T043 [US1] Adapt and close `delta-chain` in `/code/cardano-wallet/lib/delta-chain/delta-chain.cabal` and `/code/cardano-wallet/lib/delta-chain/`
- [ ] T044 [US1] Adapt and close `delta-table` in `/code/cardano-wallet/lib/delta-table/delta-table.cabal` and `/code/cardano-wallet/lib/delta-table/`
- [ ] T045 [US1] Adapt and close `flaky-tests` in `/code/cardano-wallet/lib/flaky-tests/flaky-tests.cabal` and `/code/cardano-wallet/lib/flaky-tests/`
- [ ] T046 [US1] Adapt and close `std-gen-seed` in `/code/cardano-wallet/lib/std-gen-seed/std-gen-seed.cabal` and `/code/cardano-wallet/lib/std-gen-seed/`

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

When T015 closes, no later task may edit `/code/cardano-wallet/lib/numeric/`. The same rule applies to every component task T016 through T046 for that task's listed component directory. A later failure that truly belongs to a closed component requires stopping, documenting the reopen in this file, and correcting the patch stack before continuing.

### Parallel Opportunities

None for component implementation. The only safe parallel work is read-only investigation before Phase 1 or independent upstream repo preparation before wallet pins are updated, and even that must not close wallet consumer components out of order.

## Implementation Strategy

1. Freeze node 11.0.1.
2. Fix upstream dependencies.
3. Align wallet metadata.
4. Close components one by one in dependency-first topology.
5. Run runtime and whole-repository gates.
