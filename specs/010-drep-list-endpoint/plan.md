# Implementation Plan: Add GET /v2/dreps Endpoint

**Branch**: `010-drep-list-endpoint` | **Date**: 2026-08-02 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/010-drep-list-endpoint/spec.md`

## Summary

Add `GET /v2/dreps` to the cardano-wallet Servant API. The handler queries two Conway-era ledger state queries (`GetDRepState` and `GetDRepStakeDistr`) via the existing local state query infrastructure, joins the results on the DRep credential, maps them to a new `ApiDRepInfo` response type, and returns `[]` for pre-Conway eras. The OpenAPI spec, the Link module, and the Shelley client module are updated in lockstep.

## Technical Context

**Language/Version**: Haskell (GHC 9.6+, as pinned in flake.nix)
**Primary Dependencies**: `ouroboros-consensus-cardano` (Shelley.GetDRepState tag 25, Shelley.GetDRepStakeDistr tag 26), `cardano-ledger-core` (DRepState, Credential DRepRole, Anchor), `servant` (API type), `aeson` (JSON serialisation)
**Storage**: N/A — read-only ledger query, no wallet DB involved
**Testing**: `cardano-wallet-integration` test suite (local Conway cluster), `cabal test` unit tests
**Target Platform**: Linux, Windows, macOS (same as the rest of the wallet)
**Project Type**: web-service (REST API library + application)
**Performance Goals**: Ledger LSQ dominates; wallet processing overhead < 10 ms for 1 000 DReps
**Constraints**: Must not break existing DRep endpoint (`PUT /v2/dreps/:id/wallets/:id`); must compile with -Wall

## Constitution Check

### I. Maintenance-First Stability
This change adds a read-only query endpoint with no wallet DB writes and no transaction signing. Risk is low. The only new code path is a ledger LSQ and a JSON serialiser.

### II. Era-Aware Design
The new `DReps.hs` LSQ module uses `onAnyEra` to return `Nothing` for pre-Conway eras and `Just result` for Conway and Dijkstra. The API handler maps `Nothing` to `[]`.

### III. Type Safety as Security
New types (`ApiDRepInfo`, `DRepRegistration`) are newtypes or records with explicit JSON instances. No stringly-typed shortcuts. Haddock on all exported functions.

### IV. Formal Specification
`specifications/api/swagger.yaml` is updated with the new endpoint before the implementation is merged. The Haskell types are derived from the spec, not the other way around.

### V. Reproducible Builds
No new Cabal dependencies; all required packages (`ouroboros-consensus`, `cardano-ledger-core`) are already in `cabal.project`. No `.cabal` file version pins are added.

### VI. Comprehensive Testing
Integration test in `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs` covering US1 (list DReps on Conway cluster) and US2 (empty list pre-Conway).

### VII. Code Quality Gates
Fourmolu, HLint, -Wall compilation, and integration tests must all pass before merge.

## Project Structure

### Documentation (this feature)

```text
specs/010-drep-list-endpoint/
├── plan.md              # This file
├── research.md          # LSQ query research
├── data-model.md        # ApiDRepInfo type design
├── contracts/
│   └── dreps-get.yaml   # OpenAPI fragment for GET /v2/dreps
└── tasks.md             # Implementation tasks
```

### Source Code (repository root)

```text
lib/
├── network-layer/src/Cardano/Wallet/Network/
│   ├── Network.hs                          # add listDReps field
│   ├── LocalStateQuery.hs                  # re-export DReps module
│   ├── LocalStateQuery/DReps.hs            # NEW: GetDRepState + GetDRepStakeDistr LSQs
│   └── Implementation.hs                   # implement listDReps
├── api/src/Cardano/Wallet/Api/
│   ├── Api.hs                              # add ListDReps type, extend DReps n
│   ├── Types.hs                            # add ApiDRepInfo, ApiDRepCredential
│   ├── Link.hs                             # add listDReps link
│   ├── Clients/Shelley.hs                  # add listDReps client function
│   ├── Http/Server.hs                      # wire listDReps into dreps server
│   └── Http/Shelley/Server.hs              # implement listDReps handler
└── integration/scenarios/Test/Integration/Scenario/API/
    └── Voting.hs                           # add list-dreps integration tests

specifications/api/swagger.yaml             # add GET /v2/dreps endpoint
```

**Structure Decision**: Single monorepo library layout (existing pattern). No new Cabal packages needed — the new LSQ module lives in `cardano-wallet-network-layer`, the new API types in `cardano-wallet-api`.
