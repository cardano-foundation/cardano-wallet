# Feature Specification: DRep Listing and Discovery Endpoints

**Feature Branch**: `010-drep-list-endpoint`
**Created**: 2026-08-02
**Updated**: 2026-08-03
**Status**: Implementation complete
**Input**: Expose DRep registry data through the wallet REST API so Daedalus can replace cardano-cli subprocess calls with a single wallet endpoint.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Browse All Registered DReps (Priority: P1)

A Daedalus user opens the governance section and sees a list of all registered DReps they can delegate their vote to. The list shows each DRep's identity, activity status, voting power, and anchored metadata URL.

**Why this priority**: This is the primary blocker for removing the cardano-cli subprocess from Daedalus. Without this endpoint, Daedalus cannot display the DRep directory at all.

**Independent Test**: Call `GET /v2/dreps` on a Conway-era node with at least one registered DRep and confirm a non-empty JSON array is returned with the correct fields.

**Acceptance Scenarios**:

1. **Given** a running wallet connected to a Conway-era (or later) node, **When** `GET /v2/dreps` is called, **Then** the response is a JSON array of DRep objects, each containing `id`, `credential`, `status`, `expiry_epoch`, `voting_power`, `deposit`, `anchor`, `name`, and `metadata` (always null in list responses; use `GET /v2/dreps/{id}` for the populated value).

2. **Given** a DRep registered with an anchor, **When** `GET /v2/dreps` is called, **Then** the matching entry contains a non-null `anchor` with `url` and `data_hash` fields.

3. **Given** a DRep registered without an anchor, **When** `GET /v2/dreps` is called, **Then** the matching entry has `"anchor": null` and `"metadata": null`.

4. **Given** a DRep whose `expiry_epoch` is less than the current epoch, **When** `GET /v2/dreps` is called, **Then** that DRep's `status` is `"inactive"`.

5. **Given** a DRep whose `expiry_epoch` is greater than or equal to the current epoch, **When** `GET /v2/dreps` is called, **Then** that DRep's `status` is `"active"`.

---

### User Story 2 - Pre-Conway Era Handling (Priority: P2)

When the wallet is connected to a node running a pre-Conway era, calling `GET /v2/dreps` returns an empty list rather than an error, so the Daedalus UI can render a "no DReps available" state gracefully.

**Why this priority**: The wallet must handle all eras without crashing. The empty-list response allows the UI to remain functional during the Babbage to Conway transition.

**Independent Test**: Connect the wallet to a Babbage-era or earlier cluster, call `GET /v2/dreps`, and confirm a `200 OK` with `[]` is returned.

**Acceptance Scenarios**:

1. **Given** a wallet connected to a pre-Conway node, **When** `GET /v2/dreps` is called, **Then** the response is `200 OK` with body `[]`.

---

### User Story 3 - Sentinel DReps Excluded (Priority: P3)

`drep-alwaysAbstain` and `drep-alwaysNoConfidence` are governance choices, not registered entries. They must not appear in the `GET /v2/dreps` response.

**Why this priority**: Including sentinels would break the Daedalus DRep directory renderer and violate the design intent.

**Independent Test**: Verify that no entry in the response has `credential.type` value other than `"key_hash"` or `"script_hash"`.

**Acceptance Scenarios**:

1. **Given** any Conway-era response from `GET /v2/dreps`, **When** each entry is inspected, **Then** every entry has `credential.type` equal to `"key_hash"` or `"script_hash"` and `id` is a valid CIP-129 bech32 DRep credential ID.

---

### User Story 4 - DRep Name in List Response (Priority: P2)

A Daedalus user browsing the DRep list sees each DRep's human-readable name
alongside their on-chain data, without needing to make a separate request per
DRep. The name comes from the CIP-0119 `givenName` field and is fetched
asynchronously.

**Why this priority**: The name is the minimal information needed to render a
DRep directory row. Full metadata (objectives, qualifications, etc.) is only
needed when a user drills into a specific DRep's detail view.

**Independent Test**: Register a DRep with a valid CIP-0119 anchor URL, wait
for the metadata worker to fetch it, then call `GET /v2/dreps` and confirm
the matching entry has a non-null `name` field set to `givenName`.

**Acceptance Scenarios**:

1. **Given** a DRep registered with a reachable anchor URL containing valid CIP-0119 JSON, **When** `GET /v2/dreps` is called after the metadata has been fetched, **Then** the matching entry has `name` set to the `givenName` from the metadata document.

2. **Given** a DRep whose metadata fetch failed (URL unreachable, hash mismatch), **When** `GET /v2/dreps` is called, **Then** the entry has `"name": null` and the endpoint still returns `200 OK`.

3. **Given** a DRep with an anchor URL whose metadata has not yet been fetched, **When** `GET /v2/dreps` is called, **Then** the entry has `"name": null` (the fetch happens asynchronously; the endpoint does not block).

4. **Given** a DRep metadata document with `doNotList: true`, **When** `GET /v2/dreps` is called, **Then** the entry still appears in the list with `"name"` populated (filtering is the client's responsibility; use `GET /v2/dreps/suggested` for pre-filtered results).

---

### User Story 5 - Full DRep Detail On Demand (Priority: P2)

A Daedalus user opens a DRep's detail page and sees their full governance
profile: objectives, motivations, qualifications, payment address, and
references, not just the name — alongside all on-chain fields.

**Why this priority**: Full metadata is only needed for one DRep at a time
(the one the user clicked on), so it should be a separate targeted request
rather than embedded in every list response.

**Independent Test**: Call `GET /v2/dreps/{drepId}` for a DRep with a
reachable anchor URL and confirm all on-chain fields are present and
`metadata` is populated with all CIP-0119 fields.

**Acceptance Scenarios**:

1. **Given** a DRep with successfully fetched CIP-0119 metadata, **When** `GET /v2/dreps/{drepId}` is called, **Then** the response is a full `ApiDRepInfo` object with a non-null `metadata` containing `name`, `do_not_list`, `references`, and any populated optional fields.

2. **Given** a DRep with no anchor or unfetched/failed metadata, **When** `GET /v2/dreps/{drepId}` is called, **Then** the response is a full `ApiDRepInfo` object with `metadata: null` (not an error).

3. **Given** an always-abstain or always-no-confidence DRep ID, **When** `GET /v2/dreps/{drepId}` is called, **Then** the response body is `null`.

---

### User Story 6 - Suggested DReps for Delegation (Priority: P2)

A Daedalus user who has not yet delegated their vote is shown a curated
random sample of DReps they can consider delegating to — biased toward
smaller DReps to encourage decentralisation.

**Why this priority**: Presenting users with "all N hundred DReps" is
overwhelming. A pre-filtered random shortlist of identified, opt-in DReps
lowers the barrier to participation in governance.

**Independent Test**: Call `GET /v2/dreps/suggested` and confirm all returned
entries are active, have a non-null `name`, and are not among the top 35 by
voting power.

**Acceptance Scenarios**:

1. **Given** a Conway-era node with registered DReps, **When** `GET /v2/dreps/suggested` is called, **Then** all returned entries have `status: active` and a non-null `name`.

2. **Given** a large DRep set, **When** `GET /v2/dreps/suggested` is called twice, **Then** the results are not necessarily identical (random sampling).

3. **Given** `?count=5`, **When** `GET /v2/dreps/suggested` is called, **Then** at most 5 entries are returned.

4. **Given** `?count=300`, **When** `GET /v2/dreps/suggested` is called, **Then** at most 200 entries are returned (server-side cap).

---

### User Story 7 - IPFS Anchor URLs (Priority: P3)

A DRep who published their CIP-0119 metadata document on IPFS using an
`ipfs://` anchor URL still has their name and metadata populated in wallet
responses.

**Why this priority**: IPFS is a popular hosting choice for DRep metadata.
Without support, DReps using IPFS anchors would permanently show `name: null`.

**Independent Test**: Register a DRep with an `ipfs://<CID>` anchor URL
pointing to valid CIP-0119 JSON. After the worker runs, confirm `name` is
populated in `GET /v2/dreps`.

**Acceptance Scenarios**:

1. **Given** a DRep with an `ipfs://` anchor URL, **When** the metadata worker runs, **Then** the wallet fetches the document via the Blockfrost IPFS gateway and populates `name` on success.

2. **Given** an `ipfs://` URL whose CID does not resolve, **When** the metadata worker runs, **Then** the entry has `"name": null` (same as any other failed fetch).

---

### Edge Cases

- What happens when `voting_power` exceeds JavaScript's `Number.MAX_SAFE_INTEGER`? The field MUST be a JSON string (not a number) to preserve precision.
- What happens when no DReps are registered on a Conway node? The response is `200 OK` with `[]`.
- What happens when the metadata anchor URL returns non-JSON or invalid CIP-0119? The metadata is discarded and `name` is `null` for that entry.
- What happens when the Blake2b-256 hash of fetched metadata does not match `anchor.data_hash`? The metadata is discarded (hash mismatch) and `name` is `null`.
- What happens when the metadata worker has not yet run since wallet startup? `name` is `null` for all entries; it will be populated after the first background fetch cycle.
- What happens when `GET /v2/dreps/suggested` is called on a pre-Conway node? Returns `200 OK` with `[]`.
- What happens when fewer eligible DReps exist than `count`? All eligible DReps are returned.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST expose `GET /v2/dreps` returning a JSON array of all registered DReps known to the connected node.
- **FR-002**: Each DRep list entry MUST include `id`, `credential`, `status`, `expiry_epoch`, `voting_power`, `deposit`, `anchor`, and `name`.
- **FR-003**: `voting_power.quantity` MUST be serialised as a JSON string.
- **FR-004**: System MUST return `200 OK` with `[]` when the connected node is pre-Conway.
- **FR-005**: Sentinel DReps (always-abstain, always-no-confidence) MUST NOT appear in the response.
- **FR-006**: All new endpoints MUST be documented in `specifications/api/swagger.yaml` under the `DReps` tag.
- **FR-007**: System MUST derive `status` from `expiry_epoch` vs the current epoch from the node.
- **FR-008**: System MUST fetch DRep metadata from anchor URLs in a background worker and cache results locally.
- **FR-009**: Metadata MUST be verified against `anchor.data_hash` (Blake2b-256) before being stored; mismatches MUST be discarded.
- **FR-010**: The list endpoints MUST NOT block on metadata fetches; they serve whatever is cached.
- **FR-011**: Metadata fetching MUST be re-attempted on a configurable interval for DReps whose fetch has not yet succeeded.
- **FR-012**: System MUST expose `GET /v2/dreps/{drepId}` returning the full `ApiDRepInfo` record with an embedded `metadata :: Maybe ApiDRepMetadata` field for a single DRep (or null if the DRep ID is not found / is a sentinel).
- **FR-013**: System MUST expose `GET /v2/dreps/suggested` returning a random sample of active, identified, opt-in DReps, excluding the top 35 by voting power.
- **FR-014**: `GET /v2/dreps/suggested` MUST accept a `count` query parameter (default 20, max 200).
- **FR-015**: System MUST resolve `ipfs://` anchor URLs to the Blockfrost IPFS gateway (`https://ipfs.blockfrost.dev/ipfs/`) before fetching metadata.

### Key Entities

- **DRepInfo**: A registered DRep — identified by key-hash or script-hash credential, carrying expiry epoch, optional anchor, deposit, voting power, and optional cached metadata.
- **DRepCredential**: The on-chain identity of a DRep (key hash or script hash), distinct from sentinel types.
- **DRepMetadata**: Off-chain CIP-0119 metadata document fields: name, objectives, motivations, qualifications, payment address, do-not-list flag, references.
- **DRepLayer**: Service facade that merges live LSQ data with cached metadata, analogous to `StakePoolLayer`.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `GET /v2/dreps` returns in under 5 seconds on mainnet for up to 1 000 registered DReps.
- **SC-002**: Every on-chain field in the response matches data from `cardano-cli query drep-state --all-dreps` and `cardano-cli query drep-stake-distribution --all-dreps` on the same node at the same slot.
- **SC-003**: Daedalus can remove the `GovernanceQueryService` subprocess after adopting this endpoint.
- **SC-004**: All new endpoints pass the wallet integration test suite without regressions.
- **SC-005**: For a DRep with a reachable CIP-0119 anchor URL (https:// or ipfs://), `name` is populated within one metadata fetch cycle (configurable, default 15 minutes) of wallet startup.
- **SC-006**: `GET /v2/dreps/suggested` returns only active DReps with a non-null `name` and `do_not_list: false`, excluding the top 35 by voting power.
- **SC-007**: The LSQ result serving both list endpoints is cached for 900 seconds to avoid repeated node queries.

## Assumptions

- The wallet is connected to a Conway-era or later node for DRep data; pre-Conway returns empty list.
- `voting_power` is obtained from `GetDRepStakeDistr` (ledger LSQ, Conway+), reflecting the most recently completed epoch's distribution.
- `deposit` is taken from `DRepState.drepDeposit` in the ledger.
- `anchor` is `null` when the DRep registered with `SNothing` anchor.
- No authentication or wallet-scoping is required; this is a node-level read query.
- The optional `stake` query parameter for non-myopic rewards is out of scope for this iteration.
- Metadata is fetched directly from anchor URLs (no SMASH-equivalent registry for DReps yet); a registry integration can be added later without breaking the API contract.
- The `doNotList` field in CIP-0119 metadata is advisory; the wallet exposes it but does not enforce it.
