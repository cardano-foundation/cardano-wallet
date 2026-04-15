# Implementation Plan: Bump dependencies to cardano-node 10.7.0

**Branch**: `002-bump-node-10-7-0` | **Date**: 2026-04-15 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-bump-node-10-7-0/spec.md`

## Summary

Update all Cardano dependency version constraints and `.cabal` bounds to match cardano-node 10.7.0, following the process in `docs/site/src/contributor/notes/updating-dependencies.md`. The freeze file from cardano-node 10.7.0 is the source of truth. The `.cabal` files are updated in topological order using `bump-constraints.sh`.

## Technical Context

**Language/Version**: Haskell, GHC 9.12 (unchanged)
**Primary Dependencies**: cardano-node 10.7.0, ouroboros-consensus 1.0.0.0, ouroboros-network 1.1.0.0, cardano-ledger-core 1.19.0.0, cardano-api 10.25.0.0, plutus-core 1.59.0.0
**Storage**: N/A (no storage changes)
**Testing**: cabal test, just e2e, just integration-tests-cabal-options
**Target Platform**: Linux (primary), macOS, Windows (cross-compiled)
**Project Type**: Library/application monorepo
**Constraints**: Must build on all three platforms. E2E must pass on preprod.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | PASS | Node upgrade is core maintenance activity |
| II. Era-Aware Design | PASS | No era logic changes in this PR, just version bumps |
| III. Type Safety as Security | PASS | No type changes in this PR |
| IV. Formal Specification | N/A | No spec changes needed for dependency bump |
| V. Reproducible Builds | PASS | CHaP aligned with node 10.7.0, flake.lock updated, SHA256 pins maintained |
| VI. Comprehensive Testing | PASS | E2E already running against 10.7.0 in probe PR #5248 |
| VII. Code Quality Gates | PASS | Will run full CI after bump |

## Project Structure

### Documentation (this feature)

```text
specs/002-bump-node-10-7-0/
├── spec.md
├── plan.md              # This file
├── research.md          # Version delta analysis
└── checklists/
    └── requirements.md
```

### Source Code (repository root)

No new files. Changes touch:

```text
cabal.project                    # Constraints, index-state
flake.nix                        # cardano-node-runtime input
flake.lock                       # CHaP, cardano-node-runtime
lib/*/cardano-*.cabal            # Version bounds (via bump-constraints.sh)
```

## Process

Following `updating-dependencies.md` steps:

### Step 1: Choose target version
cardano-node 10.7.0 (van Rossem hard fork, protocol version 11)

### Step 2: Freeze from node
Freeze file: `/tmp/cardano-node-10.7.0/cabal.project.freeze`

### Step 3: Align CHaP
CHaP rev `887d73ce434831e3a67df48e070f4f979b3ac5a6` (same as node 10.7.0)
Index-state: `hackage.haskell.org 2026-02-17T10:15:41Z`, `cardano-haskell-packages 2026-03-23T18:21:55Z`

### Step 4: Update cabal.project constraints
Replace the `-- Cardano Node 10.6.3 dependencies:` block with versions from freeze file.

Key version changes:

| Package | 10.6.3 | 10.7.0 |
|---------|--------|--------|
| cardano-api | 10.23.0.0 | 10.25.0.0 |
| cardano-binary | 1.7.2.0 | 1.8.0.0 |
| cardano-crypto-class | 2.2.3.2 | 2.3.1.0 |
| cardano-crypto-praos | 2.2.1.1 | 2.2.2.0 |
| cardano-crypto-wrapper | 1.6.1.0 | 1.7.0.0 |
| cardano-data | 1.2.4.1 | 1.3.0.0 |
| cardano-ledger-allegra | 1.8.0.0 | 1.9.0.0 |
| cardano-ledger-alonzo | 1.14.0.0 | 1.15.0.0 |
| cardano-ledger-api | 1.12.1.0 | 1.13.0.0 |
| cardano-ledger-babbage | 1.12.0.0 | 1.13.0.0 |
| cardano-ledger-binary | 1.7.1.0 | 1.8.0.0 |
| cardano-ledger-byron | 1.2.0.0 | 1.3.0.0 |
| cardano-ledger-conway | 1.20.0.0 | 1.21.0.0 |
| cardano-ledger-core | 1.18.0.0 | 1.19.0.0 |
| cardano-ledger-mary | 1.9.0.0 | 1.10.0.0 |
| cardano-ledger-shelley | 1.17.0.0 | 1.18.0.0 |
| cardano-prelude | 0.2.1.0 | 0.2.1.0 (unchanged) |
| cardano-protocol-tpraos | 1.4.1.0 | 1.5.0.0 |
| cardano-slotting | 0.2.0.1 | 0.2.1.0 |
| cardano-strict-containers | 0.1.6.0 | 0.1.6.0 (unchanged) |
| io-classes | 1.8.0.1 | 1.8.0.1 (unchanged) |
| ouroboros-consensus | 0.30.0.1 | 1.0.0.0 |
| ouroboros-consensus-cardano | 0.26.0.3 | REMOVED (absorbed) |
| ouroboros-consensus-diffusion | 0.26.0.0 | REMOVED (absorbed) |
| ouroboros-consensus-protocol | 0.13.0.0 | REMOVED (absorbed) |
| ouroboros-network | 0.22.6.0 | 1.1.0.0 |
| ouroboros-network-api | 0.16.0.0 | REMOVED (absorbed) |
| ouroboros-network-framework | 0.19.3.0 | REMOVED (absorbed) |
| ouroboros-network-protocols | 0.15.2.0 | REMOVED (absorbed) |
| plutus-core | 1.57.0.0 | 1.59.0.0 |
| plutus-ledger-api | 1.57.0.0 | 1.59.0.0 |
| plutus-tx | 1.57.0.0 | 1.59.0.0 |

### Step 5: Run bump-constraints.sh

Feed the freeze file to `scripts/bump-constraints.sh` to update all `.cabal` files. The script handles packages it knows about; removed packages need manual cleanup.

### Step 6: Update .cabal files in topological order

One commit per sublibrary, bottom-up (leaves first):

1. `cardano-numeric` (`lib/numeric`)
2. `text-class` (`lib/text-class`)
3. `cardano-wallet-launcher` (`lib/launcher`)
4. `cardano-wallet-read` (`lib/cardano-wallet-read`)
5. `cardano-wallet-test-utils` (`lib/test-utils`)
6. `crypto-primitives` (`lib/crypto-primitives`)
7. `delta-types` (`lib/delta-types`)
8. `cardano-wallet-primitive` (`lib/primitive`)
9. `cardano-wallet-secrets` (`lib/secrets`)
10. `address-derivation-discovery` (`lib/address-derivation-discovery`)
11. `cardano-api-extra` (`lib/cardano-api-extra`)
12. `iohk-monitoring-extra` (`lib/iohk-monitoring-extra`)
13. `cardano-wallet-network-layer` (`lib/network-layer`)
14. `delta-store` (`lib/delta-store`)
15. `cardano-wallet` (`lib/wallet`)
16. `cardano-wallet-api` (`lib/api`)
17. `wai-middleware-logging` (`lib/wai-middleware-logging`)
18. `cardano-wallet-application` (`lib/application`)
19. `cardano-wallet-ui` (`lib/ui`)
20. `cardano-wallet-application-extras` (`lib/application-extras`)
21. `cardano-wallet-application-tls` (`lib/application-tls`)
22. `faucet` (`lib/faucet`)
23. `temporary-extra` (`lib/temporary-extra`)
24. `local-cluster` (`lib/local-cluster`)
25. `cardano-wallet-benchmarks` (`lib/benchmarks`)
26. `cardano-wallet-unit` (`lib/unit`)
27. `cardano-wallet-blackbox-benchmarks` (`lib/wallet-benchmarks`)
28. `cardano-wallet-integration` (`lib/integration`)
29. `flaky-tests` (`lib/flaky-tests`)
30. `delta-chain` (`lib/delta-chain`)
31. `delta-table` (`lib/delta-table`)

### Step 7: Manual cleanup

- Remove constraints for absorbed ouroboros packages from `cabal.project`
- Remove absorbed ouroboros packages from `bump-constraints.sh` updates list
- Update `allow-newer` section if any entries reference removed packages
- Check source-repository-package pins (cardano-ledger-read, cardano-balance-tx) for compatibility with new ledger versions

### Step 8: Update flake.nix
Already done: `cardano-node-runtime.url` → 10.7.0

### Step 9: Validate
- `cabal build all -O0`
- `just ci`
- `just e2e`
