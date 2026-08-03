# Feature Specification: Add GET /v2/dreps Endpoint

**Feature Branch**: `010-drep-list-endpoint`
**Created**: 2026-08-02
**Status**: In Progress
**Input**: Expose DRep registry data through the wallet REST API so Daedalus can replace cardano-cli subprocess calls with a single wallet endpoint.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Browse All Registered DReps (Priority: P1)

A Daedalus user opens the governance section and sees a list of all registered DReps they can delegate their vote to. The list shows each DRep's identity, activity status, voting power, and anchored metadata URL.

**Why this priority**: This is the primary blocker for removing the cardano-cli subprocess from Daedalus. Without this endpoint, Daedalus cannot display the DRep directory at all.

**Independent Test**: Call `GET /v2/dreps` on a Conway-era node with at least one registered DRep and confirm a non-empty JSON array is returned with the correct fields.

**Acceptance Scenarios**:

1. **Given** a running wallet connected to a Conway-era (or later) node, **When** `GET /v2/dreps` is called, **Then** the response is a JSON array of DRep objects, each containing `id`, `credential`, `status`, `expiry_epoch`, `voting_power`, `deposit`, `anchor`, and `metadata`.

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

### User Story 4 - DRep Metadata Enrichment (Priority: P2)

A Daedalus user can see a DRep's human-readable name, stated objectives, and qualifications alongside their on-chain data. This information comes from the off-chain metadata document published at the DRep's anchor URL (CIP-0119 format).

**Why this priority**: Raw on-chain data (key hashes, epoch numbers) is not usable for humans choosing a DRep to delegate to. Names and descriptions are essential for the governance UX.

**Independent Test**: Register a DRep with a valid CIP-0119 anchor URL, wait for the metadata worker to fetch it, then call `GET /v2/dreps` and confirm the matching entry has a non-null `metadata` with the correct `name` field.

**Acceptance Scenarios**:

1. **Given** a DRep registered with a reachable anchor URL containing valid CIP-0119 JSON, **When** `GET /v2/dreps` is called after the metadata has been fetched, **Then** the matching entry has `metadata.name` set to the `givenName` from the metadata document.

2. **Given** a DRep whose metadata fetch failed (URL unreachable, hash mismatch), **When** `GET /v2/dreps` is called, **Then** the entry has `"metadata": null` and the endpoint still returns `200 OK`.

3. **Given** a DRep with an anchor URL whose metadata has not yet been fetched, **When** `GET /v2/dreps` is called, **Then** the entry has `"metadata": null` (the fetch happens asynchronously; the endpoint does not block).

4. **Given** a DRep metadata document with `doNotList: true`, **When** `GET /v2/dreps` is called, **Then** the entry still appears in the list but the `metadata.do_not_list` field is `true` (filtering is the client's responsibility).

---

### Edge Cases

- What happens when `voting_power` exceeds JavaScript's `Number.MAX_SAFE_INTEGER`? The field MUST be a JSON string (not a number) to preserve precision.
- What happens when no DReps are registered on a Conway node? The response is `200 OK` with `[]`.
- What happens when the metadata anchor URL returns non-JSON or invalid CIP-0119? The metadata is discarded and `metadata` is `null` for that entry.
- What happens when the Blake2b-256 hash of fetched metadata does not match `anchor.data_hash`? The metadata is discarded (hash mismatch) and `metadata` is `null`.
- What happens when the metadata worker has not yet run since wallet startup? `metadata` is `null` for all entries; it will be populated after the first background fetch cycle.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST expose `GET /v2/dreps` returning a JSON array of all registered DReps known to the connected node.
- **FR-002**: Each DRep entry MUST include `id`, `credential`, `status`, `expiry_epoch`, `voting_power`, `deposit`, `anchor`, and `metadata`.
- **FR-003**: `voting_power.quantity` MUST be serialised as a JSON string.
- **FR-004**: System MUST return `200 OK` with `[]` when the connected node is pre-Conway.
- **FR-005**: Sentinel DReps (always-abstain, always-no-confidence) MUST NOT appear in the response.
- **FR-006**: The endpoint MUST be documented in `specifications/api/swagger.yaml` under the `DReps` tag.
- **FR-007**: System MUST derive `status` from `expiry_epoch` vs the current epoch from the node.
- **FR-008**: System MUST fetch DRep metadata from anchor URLs in a background worker and cache results locally.
- **FR-009**: Metadata MUST be verified against `anchor.data_hash` (Blake2b-256) before being stored; mismatches MUST be discarded.
- **FR-010**: The `GET /v2/dreps` endpoint MUST NOT block on metadata fetches; it serves whatever is cached.
- **FR-011**: Metadata fetching MUST be re-attempted on a configurable interval for DReps whose fetch has not yet succeeded.

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
- **SC-004**: The new endpoint passes the wallet integration test suite without regressions.
- **SC-005**: For a DRep with a reachable CIP-0119 anchor URL, `metadata` is populated within one metadata fetch cycle (configurable, default 15 minutes) of wallet startup.

## Assumptions

- The wallet is connected to a Conway-era or later node for DRep data; pre-Conway returns empty list.
- `voting_power` is obtained from `GetDRepStakeDistr` (ledger LSQ, Conway+), reflecting the most recently completed epoch's distribution.
- `deposit` is taken from `DRepState.drepDeposit` in the ledger.
- `anchor` is `null` when the DRep registered with `SNothing` anchor.
- No authentication or wallet-scoping is required; this is a node-level read query.
- The optional `stake` query parameter for non-myopic rewards is out of scope for this iteration.
- Metadata is fetched directly from anchor URLs (no SMASH-equivalent registry for DReps yet); a registry integration can be added later without breaking the API contract.
- The `doNotList` field in CIP-0119 metadata is advisory; the wallet exposes it but does not enforce it.
