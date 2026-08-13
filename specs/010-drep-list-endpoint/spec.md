# Feature Specification: DRep Listing and Discovery Endpoints

**Feature Branch**: `010-drep-list-endpoint`
**Created**: 2026-08-02
**Updated**: 2026-08-07
**Status**: Implementation in progress
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

3. **Given** an always-abstain, always-no-confidence, or unknown DRep ID, **When** `GET /v2/dreps/{drepId}` is called, **Then** the response is `404 Not Found`.

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

3. **Given** the wallet is started with `--ipfs-gateway-url=https://my-gateway.example.com/ipfs/`, **When** the metadata worker processes a DRep with an `ipfs://` anchor URL, **Then** it fetches from `https://my-gateway.example.com/ipfs/<CID>` instead of the Blockfrost default.

---

### User Story 8 - Governance Dashboard Summary (Priority: P2)

A Daedalus governance dashboard screen shows headline numbers — total stake
under DRep management, how many DReps are active, how many are inactive —
without having to download and aggregate the full DRep list on the client.

**Why this priority**: A summary endpoint is cheap to compute server-side
(one pass over the cached LSQ result) and avoids forcing clients to fetch
potentially hundreds of DRep rows just to display three numbers. It is also
composable: new aggregate fields can be added in future without changing the
list or suggested endpoints.

**Independent Test**: Call `GET /v2/dreps/summary` and confirm the response
contains `total_drep_stake`, `active_drep_count`, and `inactive_drep_count`;
verify `active_drep_count + inactive_drep_count == total_drep_count` and
`total_drep_stake` equals the sum of all `voting_power.quantity` values from
`GET /v2/dreps`.

**Acceptance Scenarios**:

1. **Given** a Conway-era node with registered DReps, **When** `GET /v2/dreps/summary` is called, **Then** the response is a single JSON object with `total_drep_stake` (quantity string + unit), `active_drep_count`, `inactive_drep_count`, and `total_drep_count` fields.

2. **Given** a pre-Conway node, **When** `GET /v2/dreps/summary` is called, **Then** the response is `200 OK` with `total_drep_stake: { quantity: "0", unit: "lovelace" }`, and all counts set to `0`.

3. **Given** a Conway-era node, **When** `GET /v2/dreps/summary` and `GET /v2/dreps` are called within the same cache window, **Then** `active_drep_count + inactive_drep_count == total_drep_count` and `total_drep_stake.quantity` equals the arithmetic sum of all `voting_power.quantity` values from the list response.

4. **Given** a Conway-era node, **When** `GET /v2/dreps/summary` is called, **Then** `total_drep_stake` is serialised as a string quantity (not a number) to preserve precision for large values.

---

### Edge Cases

- What happens when `voting_power` exceeds JavaScript's `Number.MAX_SAFE_INTEGER`? The field MUST be a JSON string (not a number) to preserve precision.
- What happens when no DReps are registered on a Conway node? The response is `200 OK` with `[]`.
- What happens when the metadata anchor URL returns non-JSON or invalid CIP-0119? The metadata is discarded and `name` is `null` for that entry.
- What happens when the Blake2b-256 hash of fetched metadata does not match `anchor.data_hash`? The metadata is discarded (hash mismatch) and `name` is `null`.
- What happens when the metadata worker has not yet run since wallet startup? `name` is `null` for all entries; it will be populated after the first background fetch cycle.
- What happens when `GET /v2/dreps/suggested` is called on a pre-Conway node? Returns `200 OK` with `[]`.
- What happens when fewer eligible DReps exist than `count`? All eligible DReps are returned.
- What happens when a metadata fetch response body exceeds the size limit? The download is aborted and the entry treated as a failed fetch (`name: null`); the oversized document is never written to SQLite.
- What happens when a metadata anchor URL is unreachable or very slow? The per-request HTTP timeout fires after at most `workerFetchTimeoutMicros` (default 30 s); the fetch is recorded as a failure. Because the worker fetches anchors sequentially (`forM_`), a timed-out fetch delays subsequent fetches in the same cycle by up to one timeout interval. On a cold start with many uncached DReps this can extend the first cycle; subsequent cycles only fetch newly-registered or hash-changed anchors.
- What happens when `count=0` is passed to `GET /v2/dreps/suggested`? An empty array `[]` is returned; this is valid and distinct from "no eligible DReps."
- What happens when a malformed (non-bech32) DRep ID is passed to `GET /v2/dreps/{drepId}`? A `400 Bad Request` is returned, not `404`. The OpenAPI schema for `drepId` should not imply 404 for parse failures.
- What happens when `GET /v2/dreps/summary` is called on a pre-Conway node? Returns `200 OK` with all fields zeroed.
- What happens if `total_drep_stake` overflows a JS number? It is serialised as a string quantity, same as `voting_power.quantity` in the list endpoint.

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
- **FR-012**: System MUST expose `GET /v2/dreps/{drepId}` returning the full `ApiDRepInfo` record with an embedded `metadata :: Maybe ApiDRepMetadata` field for a single DRep, and `404 Not Found` if the DRep ID is unknown or is a sentinel.
- **FR-013**: System MUST expose `GET /v2/dreps/suggested` returning a random sample of active, identified, opt-in DReps, excluding the top 35 by voting power.
- **FR-014**: `GET /v2/dreps/suggested` MUST accept a `count` query parameter (default 20, max 200).
- **FR-015**: System MUST resolve `ipfs://` anchor URLs via a configurable IPFS gateway base URL before fetching metadata. The default gateway is `https://ipfs.blockfrost.dev/ipfs/`.
- **FR-016**: The IPFS gateway base URL MUST be configurable at startup via the `--ipfs-gateway-url` CLI flag (e.g. `--ipfs-gateway-url=https://my-gateway.example.com/ipfs/`). The flag MUST default to the Blockfrost public gateway so no configuration is required for typical deployments.
- **FR-017**: Metadata fetch responses MUST be capped at a configurable maximum body size (default 1 MiB). Downloads that exceed this limit MUST be aborted and treated as failed fetches; the oversized body MUST NOT be written to SQLite.
- **FR-018**: Each metadata HTTP fetch MUST be subject to a per-request timeout independent of other concurrent fetches. A single unresponsive anchor MUST NOT stall the rest of the worker cycle. Implementation: wrap each `fetchDRepMetadata` call in `System.Timeout.timeout` (default 30 s); a `Nothing` result is treated the same as any other fetch failure and records a `putDRepFetchAttempt` entry.
- **FR-019**: The `drep_metadata` SQLite table MUST be garbage-collected periodically, pruning rows whose content hash is no longer referenced by any active DRep's `anchor.data_hash`. Implementation mirrors the `lastMetadataGC` / `cleanPoolMetadata` pattern in `Cardano.Pool.DB`: store a `last_drep_metadata_gc` timestamp directly in `PoolDatabase` (not `InternalState`, which is reserved for pool-metadata GC state); after each worker cycle, if more than a configurable interval has elapsed (default 24 h), collect all current anchor hashes from `GetDRepState`, delete `drep_metadata` rows whose hash is absent from that set, and update the timestamp.
- **FR-020**: The SQLite and in-memory (Model) implementations of `putDRepMetadata` MUST maintain equivalent state: specifically, both MUST delete the corresponding `drepFetchAttempts` row on a successful store (the Model currently leaves this row in place, breaking the equivalence invariant relied on by property-based tests).
- **FR-021**: CIP-0119 `references` MUST be read from the top-level document object (alongside `body`/`authors`), not from inside the `body` object. The current implementation reads `references` from `body`, which means the field will be empty for most real-world DReps whose documents follow the canonical CIP-0119 layout.
- **FR-022**: The `drep_anchor` table, `DRepLayer.getDRepMetadata`, and the worker's `putDRepAnchorHash` write path are dead code following the consolidation of `GET /v2/dreps/{id}/metadata` into `GET /v2/dreps/{id}`. These MUST be removed to prevent confusion and maintenance burden.
- **FR-023**: System MUST expose `GET /v2/dreps/summary` returning a single `ApiDRepSummary` object with: `total_drep_stake` (sum of all DRep voting power, string quantity), `active_drep_count` (DReps with `status: active`), `inactive_drep_count` (DReps with `status: inactive`), and `total_drep_count` (= active + inactive). The response is served from the same LSQ cache as the list endpoint, so no additional node query is required. Returns zeroed values on a pre-Conway node.

### Key Entities

- **DRepInfo**: A registered DRep — identified by key-hash or script-hash credential, carrying expiry epoch, optional anchor, deposit, voting power, and optional cached metadata.
- **DRepCredential**: The on-chain identity of a DRep (key hash or script hash), distinct from sentinel types.
- **DRepMetadata**: Off-chain CIP-0119 metadata document fields: name, objectives, motivations, qualifications, payment address, do-not-list flag, references.
- **DRepLayer**: Service facade that merges live LSQ data with cached metadata, analogous to `StakePoolLayer`.
- **DRepSummary**: Aggregate view over the DRep registry — total stake, active count, inactive count.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: `GET /v2/dreps` returns in under 5 seconds on mainnet for up to 1 000 registered DReps.
- **SC-002**: Every on-chain field in the response matches data from `cardano-cli query drep-state --all-dreps` and `cardano-cli query drep-stake-distribution --all-dreps` on the same node at the same slot.
- **SC-003**: Daedalus can remove the `GovernanceQueryService` subprocess after adopting this endpoint.
- **SC-004**: All new endpoints pass the wallet integration test suite without regressions.
- **SC-005**: For a DRep with a reachable CIP-0119 anchor URL (https:// or ipfs://), `name` is populated within one metadata fetch cycle (configurable, default 15 minutes) of wallet startup.
- **SC-006**: `GET /v2/dreps/suggested` returns only active DReps with a non-null `name` and `do_not_list: false`, excluding the top 35 by voting power.
- **SC-007**: The LSQ result serving both list endpoints is cached for 900 seconds to avoid repeated node queries.
- **SC-008**: `GET /v2/dreps/summary` is consistent with `GET /v2/dreps` within a single cache window: `active_drep_count + inactive_drep_count == total_drep_count` and `total_drep_stake` equals the sum of all `voting_power.quantity` values from the list response.

## Assumptions

- The wallet is connected to a Conway-era or later node for DRep data; pre-Conway returns empty list.
- `voting_power` is obtained from `GetDRepStakeDistr` (ledger LSQ, Conway+), reflecting the most recently completed epoch's distribution.
- `deposit` is taken from `DRepState.drepDeposit` in the ledger.
- `anchor` is `null` when the DRep registered with `SNothing` anchor.
- No authentication or wallet-scoping is required; this is a node-level read query.
- The optional `stake` query parameter for non-myopic rewards is out of scope for this iteration.
- Metadata is fetched directly from anchor URLs (no SMASH-equivalent registry for DReps yet); a registry integration can be added later without breaking the API contract.
- The `doNotList` field is an informal extension to CIP-0119 (not part of the canonical spec); the wallet surfaces it as-is and defaults to `false` when absent. This should be documented in the swagger field description.
- CIP-0119 places `references` at the document top level (alongside `body` and `authors`), not inside `body`. The parser MUST handle both layouts: (a) top-level `references`, and (b) `references` inside `body` for non-conforming documents.
- List and suggested responses are served from a 15-minute (900 s) LSQ cache. Clients (e.g. Daedalus) must accept that a just-deregistered DRep may linger in responses for up to one cache interval.
- All attacker-controlled string fields (`name`, `objectives`, `references[].uri`, `payment_address`) are passed through verbatim to API consumers. Clients MUST treat these as untrusted user input and apply appropriate sanitisation before rendering.

## Security Considerations

- **SSRF mitigation**: The worker only follows `https://` URLs (after `ipfs://` rewriting); redirects are not followed; the fetched body is hash-verified against the on-chain anchor before storage. This limits SSRF to internal probing, since an attacker must know the blake2b-256 preimage to register a matching anchor hash.
- **Residual SSRF**: Private IP ranges and loopback addresses are not currently blocked at the HTTP level. An attacker-registered anchor pointing at an internal HTTPS endpoint could be used to probe the wallet host's internal network. Blocking RFC-1918 / loopback ranges in the HTTP manager, or routing fetches through a restricted egress proxy, would close this.
- **Response size**: Without a cap (FR-017), an anchor pointing at a very large document causes unbounded memory growth in the worker and oversized rows in SQLite. The 1 MiB default limit is sufficient for any real CIP-0119 document.
- **Untrusted content**: All string fields sourced from off-chain metadata are attacker-controlled. They must be treated as untrusted by API consumers.
