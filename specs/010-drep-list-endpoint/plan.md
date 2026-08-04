# Implementation Plan: DRep Listing and Discovery Endpoints

**Branch**: `010-drep-list-endpoint` | **Date**: 2026-08-02 | **Updated**: 2026-08-03 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/010-drep-list-endpoint/spec.md`

## Summary

Add three DRep read endpoints to the cardano-wallet Servant API:

- `GET /v2/dreps` — list all registered DReps with on-chain fields + `name`
- `GET /v2/dreps/suggested` — random sample of active, identified DReps (excludes top 35 by voting power)
- `GET /v2/dreps/{drepId}` — full `ApiDRepInfo` with embedded CIP-0119 metadata for one DRep

All three endpoints share a `DRepLayer` facade that merges live Conway-era
LSQ results (cached with a 900-second TTL) with off-chain metadata persisted
in SQLite. A background worker fetches and hash-verifies metadata from anchor
URLs (including IPFS), writing results to the SQLite cache.

## Technical Context

**Language/Version**: Haskell (GHC 9.6+, as pinned in flake.nix)
**Primary Dependencies**: `ouroboros-consensus-cardano` (Shelley.GetDRepState tag 25, Shelley.GetDRepStakeDistr tag 26), `cardano-ledger-core` (DRepState, Credential DRepRole, Anchor), `servant` (API types), `aeson` (JSON serialisation), `random` (Fisher-Yates sampling)
**Storage**: SQLite (`drep_metadata` table keyed on `(drep_id, data_hash)`)
**Testing**: `cardano-wallet-integration` test suite (local Conway cluster), `cabal test` unit tests
**Target Platform**: Linux, Windows, macOS (same as the rest of the wallet)
**Project Type**: web-service (REST API library + application)
**Performance Goals**: Ledger LSQ dominates; wallet processing overhead < 10 ms for 1 000 DReps; LSQ cached for 900 s
**Constraints**: Must not break existing DRep endpoint (`PUT /v2/dreps/:id/wallets/:id`); must compile with -Wall

## Constitution Check

### I. Maintenance-First Stability
All three endpoints are read-only query paths with no wallet DB writes and no transaction signing. Risk is low.

### II. Era-Aware Design
The `DReps.hs` LSQ module uses `onAnyEra` to return `Nothing` for pre-Conway eras. All list handlers map `Nothing` to `[]`.

### III. Type Safety as Security
New types (`ApiDRepInfo`, `ApiDRepMetadata`, `DRepRegistration`) are records with explicit JSON instances. The `suggestedDReps` handler's Fisher-Yates implementation operates on typed lists, not raw indices. No stringly-typed shortcuts.

### IV. Formal Specification
`specifications/api/swagger.yaml` and the OpenAPI contract fragments in `specs/010-drep-list-endpoint/contracts/` are updated with all three endpoints. Haskell types are derived from the spec.

### V. Reproducible Builds
One new Cabal dependency (`random`) added to `cardano-wallet-api.cabal`. All other required packages are already in `cabal.project`.

### VI. Comprehensive Testing
Integration tests in `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs` covering:
- List DReps on Conway cluster (DREPS_01)
- Empty list pre-Conway (DREPS_02)
- Metadata populated after worker tick (DREPS_03)
- Metadata null on unreachable anchor (DREPS_04)
- `doNotList: true` entry still appears in list (DREPS_05)

### VII. Code Quality Gates
Fourmolu, HLint, -Wall compilation, and integration tests must all pass before merge.

## Project Structure

### Documentation (this feature)

```text
specs/010-drep-list-endpoint/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Design decisions
├── data-model.md        # Type design and wire format
├── contracts/
│   └── dreps-get.yaml   # OpenAPI fragments for all three endpoints
└── tasks.md             # Implementation tasks (T001–T041)
```

### Source Code (repository root)

```text
lib/
├── network-layer/src/Cardano/Wallet/Network/
│   ├── Network.hs                              # listDReps field on NetworkLayer
│   ├── LocalStateQuery.hs                      # re-export DReps module
│   ├── LocalStateQuery/DReps.hs                # NEW: GetDRepState + GetDRepStakeDistr LSQs
│   └── Implementation.hs                       # implement listDReps
├── primitive/lib/Cardano/Wallet/Primitive/Types/
│   └── DRep.hs                                 # DRepMetadata, DRepMetaReference primitives
├── wallet/src/Cardano/Wallet/
│   ├── DRep/
│   │   ├── Layer.hs                            # NEW: DRepLayer facade + LSQ cache (900s TTL)
│   │   ├── Metadata.hs                         # NEW: fetch + parse + verify + ipfs:// resolve
│   │   └── Worker.hs                           # NEW: background metadata fetch worker
│   ├── DB/Layer/DRep.hs                        # NEW: DRepMetadataDB SQLite interface + migration
│   └── Shelley.hs                              # wire DRepLayer + worker into serveWallet
├── api/src/Cardano/Wallet/Api/
│   ├── Api.hs                                  # ListDReps, SuggestedDReps, GetDRep types
│   ├── Types.hs                                # ApiDRepInfo (with name field), ApiDRepMetadata
│   ├── Link.hs                                 # listDReps link helper
│   ├── Clients/Shelley.hs                      # listDReps client function
│   ├── Http/Server.hs                          # wire all three handlers into dreps server
│   └── Http/Shelley/Server.hs                  # listDReps, suggestedDReps, getDRep
└── integration/scenarios/Test/Integration/Scenario/API/
    └── Voting.hs                               # DREPS_01–DREPS_05 integration tests

specifications/api/swagger.yaml                 # GET /v2/dreps, /suggested, /{drepId}
```

**Structure Decision**: Single monorepo library layout (existing pattern). No new Cabal packages. The `DRepLayer` + worker modules are co-located with other service facades (`StakePoolLayer`) in `lib/wallet`.
