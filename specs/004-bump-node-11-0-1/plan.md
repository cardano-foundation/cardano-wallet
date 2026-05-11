# Implementation Plan: Upgrade dependencies to cardano-node 11.0.1

**Branch**: `004-bump-node-11-0-1` | **Date**: 2026-05-11 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/code/cardano-wallet/specs/004-bump-node-11-0-1/spec.md`

## Summary

Upgrade Cardano Wallet to the dependency set aligned with cardano-node 11.0.1. The work starts by freezing the target cardano-node release and adapting upstream wallet dependencies (`cardano-ledger-read` and `cardano-balance-transaction`) before local wallet consumers are touched. Local wallet work is then organized as a dependency-first component stack: establish target metadata and source-repository pins, generate the Cabal component topology, then fix one Cabal component vertically from dependency leaves to dependents. Once a component is validated and its patch is refreshed, later patches must not edit it.

## Status

- **Completed**: Node 11.0.1 freeze, upstream `cardano-ledger-read` and `cardano-balance-transaction` PRs, wallet source-repository pins, wallet dependency metadata, StGit stack setup, `cardano-numeric`, `text-class`, `cardano-wallet-read`, `cardano-wallet-test-utils`, and `cardano-wallet-launcher`.
- **Current**: Corrected topology includes all local test/exe/bench dependencies. The next component slice is `crypto-primitives`.
- **Blockers**: None for the next component slice.

## Technical Context

**Language/Version**: Haskell, current repository GHC from `nix develop`
**Primary Dependencies**: cardano-node 11.0.1, cardano-api/cardano-cli 11.x, cardano-ledger-conway >= 1.22.1.0, CHaP index-state 2026-05-02T16:21:41Z, existing ouroboros-consensus 3.0.1 and ouroboros-network 1.1 series
**Storage**: N/A, except existing wallet/local-cluster test data may need compatibility updates
**Testing**: Nix build outputs, fourmolu, hlint, cabal-fmt, unit tests, integration tests, local-cluster tests, e2e tests
**Target Platform**: Linux primary; macOS and Windows cross-compilation compatibility must remain intact through Nix/CI
**Project Type**: Haskell library/application monorepo with multiple Cabal packages
**Performance Goals**: No wallet-facing performance regression accepted by this upgrade; benchmark suites must continue to build and remain runnable
**Constraints**: One Cabal component at a time; dependency-first topology; no edits to a closed component; Nix builds are the authoritative component gates because Cabal incremental builds can hide transitive breakage
**Scale/Scope**: Repository-wide dependency upgrade across Cardano package constraints, node freeze data, upstream wallet dependency repos, source-repository-package pins, runtime node input, and affected wallet components

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | PASS | Node dependency upgrades are maintenance work; the topology gate limits regression risk. |
| II. Era-Aware Design | PASS | cardano-node 11.0.1 era/protocol compatibility is part of local-cluster and integration validation. |
| III. Type Safety as Security | PASS | API adaptation must preserve typed transaction and ledger boundaries; no shortcut through untyped conversions. |
| IV. Formal Specification | PASS | No API contract change is intended; any observed API contract change becomes a blocker. |
| V. Reproducible Builds | PASS | CHaP, flake inputs, source-repository-package pins, and Nix build gates are explicit deliverables. |
| VI. Comprehensive Testing | PASS | Each component closes only after its vertical gate; full integration/e2e gates close the feature. |
| VII. Code Quality Gates | PASS | fourmolu, hlint, cabal-fmt, clean Nix builds, and relevant tests are required before closing patches. |

## Project Structure

### Documentation (this feature)

```text
specs/004-bump-node-11-0-1/
├── spec.md
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── tasks.md
└── checklists/
    └── requirements.md
```

### Source Code (repository root)

```text
cabal.project                         # central constraints, index-state, source-repository-package pins
flake.nix                             # cardano-node-runtime input
flake.lock                            # CHaP and runtime dependency locks
/code/cardano-node                    # target node 11.0.1 freeze source
/code/cardano-ledger-read             # upstream dependency adapted before wallet consumers
/code/cardano-balance-transaction     # upstream dependency adapted before wallet consumers
lib/*/*.cabal                         # component dependency metadata
lib/*/{src,lib,haskell,test,bench,exe} # component-owned implementation and tests
lib/local-cluster/                    # local cardano-node configuration/test data if prerequisite PR is consumed here
docs/site/src/contributor/notes/      # dependency workflow documentation only if process changes are discovered
```

**Structure Decision**: Do not introduce new product code structure. This feature changes existing dependency metadata and existing component-owned source/test files only.

## Topology Gate

The implementation unit is a Cabal component slice, not a module and not a user-story fragment. The order is dependency-first: if component A depends on component B, B must be fixed and closed before A. This is the inverse of the "blast radius" order: lower-level components are stabilized first, then their consumers are adapted.

Before editing component code:

1. Generate or confirm the current Cabal component topology after the 11.0.1 solver plan exists. The ordering input is the package-level union of every local Cabal component's local dependencies from `dist-newstyle/cache/plan.json`, including libraries, public sublibraries, tests, executables, benchmarks, and `exe-depends`.
2. Update this plan and `tasks.md` if the generated order differs from the seed order below.
3. Create a patch stack in exactly that order, one patch per component.
4. For each component patch/commit, update its non-version Cabal metadata, owned source, and owned tests together, then record formatting, linting, Nix build/test evidence, and green unit test evidence before moving to the next patch. If the component declares any Cabal `test-suite`, that test suite must pass in the same patch/commit that closes the component. Version constraints remain centralized in `cabal.project`; `.cabal` edits are limited to package names, component references, exposed modules, or other non-version metadata required by the upgrade.
5. After a component patch is refreshed, later patches must not edit files under that component's ownership. The only exception is an emergency topology correction documented in `tasks.md` and in the patch message.

## Confirmed Component Order

This order is confirmed by the topology gate after the node 11.0.1 solver plan exists. The command first runs `cabal build all --dry-run --enable-tests -O0`, then reads `dist-newstyle/cache/plan.json`, unions local dependencies from every component entry for each local package, and topologically sorts packages dependency-first. Independent packages retain the already-committed/seed order so closed independent leaves remain valid. This all-component order supersedes the earlier first-library-occurrence order because test suites are part of each package's closure evidence.

```text
00. cardano-numeric                      lib/numeric/cardano-numeric.cabal
01. text-class                           lib/text-class/text-class.cabal
02. cardano-wallet-read                  lib/cardano-wallet-read/cardano-wallet-read.cabal
03. cardano-wallet-test-utils            lib/test-utils/cardano-wallet-test-utils.cabal
04. cardano-wallet-launcher              lib/launcher/cardano-wallet-launcher.cabal
05. crypto-primitives                    lib/crypto-primitives/crypto-primitives.cabal
06. delta-types                          lib/delta-types/delta-types.cabal
07. cardano-wallet-primitive             lib/primitive/cardano-wallet-primitive.cabal
08. cardano-wallet-secrets               lib/secrets/cardano-wallet-secrets.cabal
09. address-derivation-discovery         lib/address-derivation-discovery/address-derivation-discovery.cabal
10. cardano-api-extra                    lib/cardano-api-extra/cardano-api-extra.cabal
11. iohk-monitoring-extra                lib/iohk-monitoring-extra/iohk-monitoring-extra.cabal
12. cardano-wallet-network-layer         lib/network-layer/cardano-wallet-network-layer.cabal
13. delta-store                          lib/delta-store/delta-store.cabal
14. cardano-wallet                       lib/wallet/cardano-wallet.cabal
15. cardano-wallet-api                   lib/api/cardano-wallet-api.cabal
16. cardano-wallet-application-tls       lib/application-tls/cardano-wallet-application-tls.cabal
17. wai-middleware-logging               lib/wai-middleware-logging/wai-middleware-logging.cabal
18. cardano-wallet-ui                    lib/ui/cardano-wallet-ui.cabal
19. cardano-wallet-application           lib/application/cardano-wallet-application.cabal
20. cardano-wallet-application-extras    lib/application-extras/cardano-wallet-application-extras.cabal
21. faucet                               lib/faucet/faucet.cabal
22. temporary-extra                      lib/temporary-extra/temporary-extra.cabal
23. local-cluster                        lib/local-cluster/local-cluster.cabal
24. cardano-wallet-integration           lib/integration/cardano-wallet-integration.cabal
25. cardano-wallet-unit                  lib/unit/cardano-wallet-unit.cabal
26. cardano-wallet-benchmarks            lib/benchmarks/cardano-wallet-benchmarks.cabal
27. cardano-wallet-blackbox-benchmarks   lib/wallet-benchmarks/cardano-wallet-blackbox-benchmarks.cabal
28. delta-chain                          lib/delta-chain/delta-chain.cabal
29. delta-table                          lib/delta-table/delta-table.cabal
30. flaky-tests                          lib/flaky-tests/flaky-tests.cabal
31. std-gen-seed                         lib/std-gen-seed/std-gen-seed.cabal
```

External source-repository-package compatibility (`cardano-ledger-read`, `cardano-balance-tx`, `cardano-coin-selection`) is handled before local consumers close, but those external packages are not local component slices.

## Node Freeze Gate

The node freeze is an input artifact, not an implementation afterthought.

1. Checkout `/code/cardano-node` at tag `11.0.1`.
2. Generate the target freeze with `cabal update && cabal freeze`.
3. Store or reference the resulting freeze file for this upgrade, conventionally `/tmp/cardano-node-11.0.1/cabal.project.freeze` or an equivalent documented path.
4. Extract all wallet-relevant Cardano package versions, CHaP revision/index-state, and any package removals or source-repository changes from that freeze.
5. Use this freeze as the source of truth for `cabal.project` constraints and dependency compatibility decisions. Do not add per-package version bounds to `.cabal` files.

The plan is not ready for local component work until the freeze exists and the target version table has been derived from it.

## Upstream Dependency Gate

`cardano-ledger-read` and `cardano-balance-transaction` are upstream wallet dependencies that can block multiple local components. They must be handled before local consumer patches close.

### cardano-ledger-read

- Local checkout: `/code/cardano-ledger-read`
- Current wallet pin: `cc9ca75f4b14967c714af502b50f7eee44d7a53c`
- Work: align its central dependency constraints to the node 11.0.1 freeze, adapt code/tests, validate in its own repo, and produce a commit/tag plus sha256 for the wallet source-repository-package pin. Any `.cabal` edits must be non-version metadata only.

### cardano-balance-transaction

- Local checkout: `/code/cardano-balance-transaction`
- Current wallet pin: `5d69cc9bd47062b363929c877b83f0ab96369583`
- Work: align its central dependency constraints to the node 11.0.1 freeze, adapt transaction balancing code/tests, validate in its own repo, and produce a commit/tag plus sha256 for the wallet `cardano-balance-tx` source-repository-package pin. Any `.cabal` edits must be non-version metadata only.

### Wallet pin update

After both upstream repos validate, update `cabal.project` with their new tags and sha256 values before closing local components that consume them. If one upstream repo needs no code changes, still record the compatibility proof and whether the pin remains unchanged.

## Phase Plan

### Phase 0: Research

- Confirm target versions and CHaP state from cardano-node 11.0.1.
- Generate the node 11.0.1 freeze and derive the wallet target version table from it.
- Adapt and validate `cardano-ledger-read` against the freeze if required.
- Adapt and validate `cardano-balance-transaction` against the freeze if required.
- Identify remaining source-repository-package pins that must move with the ledger/API set.
- Verify whether `ouroboros-consensus` and `ouroboros-network` remain on the already-selected compatible major versions, or document the freeze-driven reason for any adjustment.
- Confirm local-cluster prerequisite status for the cardano-node 11.0 experimental hard-fork/protocol-version behavior.
- Generate or confirm component topology after dependency metadata resolves.

### Phase 1: Design

- Model dependency targets, external pins, component slices, validation evidence, and closed-component locks.
- Define the vertical component gate and the no-return rule.
- Create quickstart commands for topology generation, per-component validation, and final feature validation.

### Phase 2: Tasks

- Generate the node freeze and set target metadata from it.
- Complete upstream dependency work and source-repository-package pins.
- Create the ordered patch stack.
- Execute component slices in the topology order.
- Close with runtime, local-cluster, integration, e2e, and review traceability gates.

## Complexity Tracking

No constitution violations are accepted. The only non-standard process choice is making component topology stricter than generic Spec Kit user-story ordering; this is required because dependency upgrades become unstable when source edits are split by feature area instead of by Cabal component ownership.
