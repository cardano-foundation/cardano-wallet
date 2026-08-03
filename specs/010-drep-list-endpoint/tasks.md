# Tasks: Add GET /v2/dreps Endpoint

**Input**: Design documents from `/specs/010-drep-list-endpoint/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/dreps-get.yaml

## Phase 1: Network Layer — LSQ Queries

**Purpose**: Add DRep ledger state queries to the network layer.

- [x] T001 Create `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery/DReps.hs` with `getAllDRepStates` and `getAllDRepVotingPower` LSQ functions
- [x] T002 Export new module from `lib/network-layer/src/Cardano/Wallet/Network/LocalStateQuery.hs`
- [x] T003 Add `listDReps :: m (Maybe DRepQueryResult)` field to `NetworkLayer` in `lib/network-layer/src/Cardano/Wallet/Network.hs`
- [x] T004 Implement `listDReps` in `lib/network-layer/src/Cardano/Wallet/Network/Implementation.hs`

---

## Phase 2: API Types

**Purpose**: Define the `ApiDRepInfo` response type and JSON instances.

- [x] T005 Add `ApiDRepInfo`, `ApiDRepCredential`, `DRepStatus`, `ApiDRepAnchor` to `lib/api/src/Cardano/Wallet/Api/Types.hs` with ToJSON/FromJSON instances
- [x] T006 Export new types from `lib/api/src/Cardano/Wallet/Api/Types.hs` module header

---

## Phase 3: Servant API & Wiring

**Purpose**: Add the `ListDReps` endpoint type and wire up the handler.

- [x] T007 Add `ListDReps` type alias and extend `DReps n` in `lib/api/src/Cardano/Wallet/Api.hs`
- [x] T008 [P] Add `listDReps` link helper to `lib/api/src/Cardano/Wallet/Api/Link.hs`
- [x] T009 [P] Add `listDReps` client function to `lib/api/src/Cardano/Wallet/Api/Clients/Shelley.hs`
- [x] T010 Implement `listDReps` handler in `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
- [x] T011 Wire `listDReps` into `dreps` server in `lib/api/src/Cardano/Wallet/Api/Http/Server.hs`

---

## Phase 4: OpenAPI Spec

**Purpose**: Document the new endpoint in the swagger spec.

- [x] T012 Add `GET /v2/dreps` endpoint and `ApiDRepInfo` schema to `specifications/api/swagger.yaml`

---

## Phase 5: Integration Tests

**Purpose**: Verify the endpoint end-to-end against a real local cluster.

- [x] T013 Add `listDReps` integration tests to `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs`

---

## Phase 6: DRep Metadata Primitive Types

**Purpose**: Define the `DRepMetadata` Haskell type for CIP-0119 off-chain metadata.

- [x] T014 Add `DRepMetadata` and `DRepMetaReference` to `lib/primitive/lib/Cardano/Wallet/Primitive/Types/DRep.hs`

---

## Phase 7: DRep Metadata DB Layer

**Purpose**: Persist fetched metadata in SQLite so it survives wallet restarts.

- [x] T015 Create `lib/wallet/src/Cardano/Wallet/DB/Layer/DRep.hs` (or extend existing DB layer) with a `DRepMetadataDB` interface: `putDRepMetadata`, `getDRepMetadata`, `clearDRepMetadata`
- [x] T016 Add SQLite migration to create `drep_metadata` table keyed on `(drep_id, data_hash)`

---

## Phase 8: CIP-0119 HTTP Fetch + Hash Verification

**Purpose**: Fetch and verify DRep metadata documents from anchor URLs.

- [x] T017 Create `lib/wallet/src/Cardano/Wallet/DRep/Metadata.hs` with:
  - `fetchDRepMetadata :: Url -> IO (Either FetchError ByteString)` (HTTP GET)
  - `parseCip0119 :: ByteString -> Either ParseError DRepMetadata` (JSON decode)
  - `verifyHash :: ByteString -> AnchorDataHash -> Either HashMismatch DRepMetadata` (Blake2b-256 check)

---

## Phase 9: Background Metadata Monitor Worker

**Purpose**: Periodically fetch metadata for all DReps that have an anchor URL but no cached metadata.

- [x] T018 Create `lib/wallet/src/Cardano/Wallet/DRep/Worker.hs` with `monitorDRepMetadata`:
  - Accepts `NetworkLayer`, `DRepMetadataDB`, `Manager` (HTTP), retry interval
  - On each tick: calls `listDReps` via `NetworkLayer`, finds DReps with anchors but missing cached metadata, fetches + verifies + stores
  - Runs in a supervised thread inside `serveWallet`

---

## Phase 10: DRepLayer Service Facade

**Purpose**: Merge live LSQ data with cached metadata; replaces direct `NetworkLayer.listDReps` call in the handler.

- [x] T019 Create `lib/wallet/src/Cardano/Wallet/DRep/Layer.hs` with `DRepLayer`:
  ```haskell
  data DRepLayer m = DRepLayer
      { listDReps :: m [DRepInfo]  -- DRepRegistration + Maybe DRepMetadata merged
      }
  ```
  Implementation: call `NetworkLayer.listDReps`, look up each result in `DRepMetadataDB`, zip and return.
- [x] T020 Wire `DRepLayer` and `monitorDRepMetadata` into `serveWallet` in `lib/wallet/src/Cardano/Wallet/Shelley.hs` (or equivalent entrypoint)

---

## Phase 11: Update Handler and API Types

**Purpose**: Plug `DRepLayer` into the API handler and expose `metadata` in the response.

- [x] T021 Update `listDReps` handler in `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs` to accept `DRepLayer` instead of `NetworkLayer` directly, and map `Maybe DRepMetadata` to `Maybe ApiDRepMetadata`
- [x] T022 Add `ApiDRepMetadata` and `ApiDRepMetaReference` to `lib/api/src/Cardano/Wallet/Api/Types.hs` with ToJSON/FromJSON instances; extend `ApiDRepInfo` with `metadata :: Maybe ApiDRepMetadata`

---

## Phase 12: Update OpenAPI Spec and Contract

**Purpose**: Document `metadata` field in the public API spec.

- [x] T023 Update `specifications/api/swagger.yaml` and `specs/010-drep-list-endpoint/contracts/dreps-get.yaml` with `metadata` field on `ApiDRepInfo` and new `ApiDRepMetadata` / `ApiDRepMetaReference` schemas

---

## Phase 13: Update Integration Tests

**Purpose**: Cover metadata enrichment in end-to-end tests.

- [x] T024 Extend `lib/integration/scenarios/Test/Integration/Scenario/API/Voting.hs`:
  - DREPS_03: DRep with reachable CIP-0119 anchor URL → `metadata.name` populated after worker tick
  - DREPS_04: DRep with unreachable anchor URL → `metadata` is `null`, endpoint returns 200
  - DREPS_05: DRep with `doNotList: true` → entry appears with `metadata.do_not_list: true`

---

---

## Phase 14: Refactor — split metadata out of list response

**Purpose**: Reduce list response payload; expose full metadata on a separate endpoint.

- [x] T025 Change `ApiDRepInfo.metadata :: Maybe ApiDRepMetadata` to `ApiDRepInfo.name :: Maybe Text` in `lib/api/src/Cardano/Wallet/Api/Types.hs`
- [x] T026 Update `listDReps` handler to populate `name` from `drepMetaName` instead of the full `ApiDRepMetadata` object
- [x] T027 Update `ApiDRepInfo` schema in `specifications/api/swagger.yaml` (add `name`, remove embedded `metadata`)

---

## Phase 15: GET /v2/dreps/{drepId}/metadata endpoint

**Purpose**: Provide the full CIP-0119 metadata document for a single DRep on demand.

- [x] T028 Add `GetDRepMetadata` Servant type to `lib/api/src/Cardano/Wallet/Api.hs`; extend `DReps n`
- [x] T029 Implement `getDRepMetadata` handler in `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
- [x] T030 Wire `getDRepMetadata` in `lib/api/src/Cardano/Wallet/Api/Http/Server.hs`
- [x] T031 Add `GET /v2/dreps/{drepId}/metadata` to `specifications/api/swagger.yaml`

---

## Phase 16: IPFS anchor URL resolution

**Purpose**: Support DReps whose metadata is hosted on IPFS.

- [x] T032 Add `resolveUrl` to `lib/wallet/src/Cardano/Wallet/DRep/Metadata.hs`: rewrites `ipfs://<CID>` to `https://ipfs.blockfrost.dev/ipfs/<CID>` before the HTTP fetch
- [x] T033 Update `fetchDRepMetadata` to call `resolveUrl` so the worker transparently handles both `https://` and `ipfs://` anchor URLs

---

## Phase 17: LSQ result caching in DRepLayer

**Purpose**: Avoid redundant LSQ round-trips when multiple endpoints read the same DRep list.

- [x] T034 Add `IORef (Maybe (UTCTime, [DRepRegistration]))` cache to `DRepLayer` in `lib/wallet/src/Cardano/Wallet/DRep/Layer.hs`
- [x] T035 Implement `cachedLSQ` helper (TTL = 900 s) and pre-warm the cache with a `forkIO` at construction
- [x] T036 Use the cache in `listDRepInfos` so both `listDReps` and `suggestedDReps` share a single LSQ result

---

## Phase 18: GET /v2/dreps/suggested endpoint

**Purpose**: Provide a pre-filtered random sample of DReps suitable for delegation.

- [x] T037 Add `SuggestedDReps` Servant type to `lib/api/src/Cardano/Wallet/Api.hs`; extend `DReps n`
- [x] T038 Implement `suggestedDReps` handler in `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`:
  - rank by voting power descending, drop top 35
  - filter: active + metadata present + `doNotList = False`
  - partial Fisher-Yates sample of up to `count` (default 20, max 200)
- [x] T039 Wire `suggestedDReps` in `lib/api/src/Cardano/Wallet/Api/Http/Server.hs`
- [x] T040 Add `GET /v2/dreps/suggested` to `specifications/api/swagger.yaml`
- [x] T041 Add `random` dependency to `lib/api/cardano-wallet-api.cabal`

---

## Dependencies & Execution Order

### Phase 1–5 (complete)
- T001 → T002 → T003 → T004
- T004 + T005 in parallel after T003
- T007 → T008, T009 (parallel) → T010 → T011
- T012 independent; T013 after T011

### Phase 6–13 (metadata enrichment, complete)
- T014 before T015, T016, T017 (need primitive type)
- T015 + T016 together (DB interface + migration)
- T017 independent of DB work
- T018 after T015 + T016 + T017
- T019 after T018
- T020 after T019
- T021 after T019 + T020
- T022 after T021
- T023 after T022
- T024 after T020 + T022

### Phase 14–18 (split + new endpoints, complete)
- T025–T027 independent refactor (after T022)
- T028–T031 after T025 (need updated ApiDRepInfo)
- T032–T033 independent of API work
- T034–T036 after T019 (extend DRepLayer)
- T037–T041 after T034–T036 (suggestedDReps uses cache)
