# Research: GET /v2/dreps Implementation

## Decision: Use GetDRepState + GetDRepStakeDistr LSQ queries directly

**Decision**: Implement a new `DReps.hs` LSQ module using `Shelley.GetDRepState mempty` (all DReps, tag 25) and `Shelley.GetDRepStakeDistr` (tag 26) from `ouroboros-consensus-cardano`, following the exact same pattern as `StakeDistribution.hs`.

**Rationale**: These are the lowest-level, most direct queries available. They avoid a dependency on `cardano-api` and are already supported by the existing LSQ infrastructure in the wallet. `GetDRepState` returns `Map (Credential 'DRepRole) DRepState` which is everything needed for the response except voting power. `GetDRepStakeDistr` gives `Map DRep Coin` keyed on the full `DRep` sum type; we filter to credential-based entries only.

**Alternatives considered**:
- Using `cardano-api`'s `queryDRepState` / `queryDRepStakeDistribution`: Rejected — adds a heavyweight dependency; the wallet already wraps these queries directly.
- Using `GetFilteredVoteDelegatees` to derive voting power from delegations: Rejected — this is O(stake credentials) and less direct than `GetDRepStakeDistr`.

## LSQ Query Details

### GetDRepState (tag 25, Conway+)
```
Input:  Set (Credential 'DRepRole StandardCrypto)  -- empty = return all
Output: Map (Credential 'DRepRole StandardCrypto) (DRepState StandardCrypto)

DRepState:
  drepExpiry  :: EpochNo
  drepAnchor  :: StrictMaybe Anchor
  drepDeposit :: CompactForm Coin
  drepDelegs  :: Set (Credential 'Staking StandardCrypto)  -- not needed for our response
```

### GetDRepStakeDistr (tag 26, Conway+)
```
Input:  Set (DRep StandardCrypto)  -- empty = return all (including sentinels)
Output: Map (DRep StandardCrypto) Coin

DRep:
  DRepAlwaysAbstain                              -- sentinel, excluded
  DRepAlwaysNoConfidence                         -- sentinel, excluded
  DRepCredential (Credential 'DRepRole cryp)    -- the ones we want
```

## Decision: Derive status server-side using GetEpochNo

**Decision**: Use `LSQry Shelley.GetEpochNo` (already available via `onAnyEra`) to fetch the current epoch and compute `status` in the handler rather than returning `expiry_epoch` only and delegating computation to the client.

**Rationale**: Mirrors the pattern used by `getWallet` which computes delegation status server-side. The client still gets `expiry_epoch` so it can recompute if desired.

## Decision: voting_power.quantity is a String

**Decision**: Serialise `voting_power.quantity` as a JSON `String`, consistent with how the wallet serialises large coin values elsewhere (e.g., balance fields use `ApiT Coin` which renders as a quoted integer string).

**Rationale**: Lovelace values on mainnet exceed `Number.MAX_SAFE_INTEGER` (2^53 - 1 = ~9 petalovelace). The network's total supply is ~45 billion ADA = 45 × 10^15 lovelace, well above the JavaScript safe integer limit.

## Decision: anchor.data_hash as lowercase hex

**Decision**: Encode `Anchor`'s `anchorDataHash` as a lowercase hex string without a `0x` prefix, matching the format used by `cardano-cli` and the wallet's other hash fields.

**Rationale**: Consistent with how the wallet already serialises `AnchorDataHash` in transaction outputs.

## Existing code to reuse

| Component | File | Reuse |
|---|---|---|
| LSQ infrastructure | `Network/Implementation.hs`, `LocalStateQuery/Extra.hs` | `onAnyEra`, `LSQry`, `shelleyBased` patterns |
| Bech32 DRep ID | `Primitive/Types/DRep.hs:84` | `encodeDRepIDBech32` |
| Coin serialisation | `Api/Types.hs` | `ApiT Coin` JSON instance (string quantity) |
| Anchor type | `cardano-ledger-core` | `Anchor { anchorUrl, anchorDataHash }` |
| GetEpochNo | `Shelley.GetEpochNo` (tag 1) | Already available in LSQ |

---

## Decision: Background metadata worker + DRepLayer facade (analogous to StakePoolLayer)

**Decision**: Add a background HTTP metadata fetching worker and a `DRepLayer` service facade that merges live LSQ data with SQLite-cached metadata. The `GET /v2/dreps` handler queries `DRepLayer`, not `NetworkLayer` directly.

**Rationale**: DRep metadata (CIP-0119 JSON: name, objectives, motivations, qualifications) lives off-chain at anchor URLs. Fetching it inline would block the endpoint and make latency unbounded. The SPO subsystem (`monitorMetadata`, `StakePoolLayer`) solves the identical problem: background fetch, hash verification, local cache, facade that merges on read. We reuse that architecture directly.

**Key differences from SPO**:
- DRep registrations are first-class ledger state (`GetDRepState`), so no chain-following is needed. The background worker only needs to HTTP-fetch metadata, not replay certificates.
- No SMASH registry equivalent exists for DReps yet; fetching is always direct from anchor URLs.
- `doNotList` (CIP-0119) is advisory — the wallet exposes it but does not enforce it.

**Alternatives considered**:
- Inline fetch per request: Rejected — unbounded latency, blocks the endpoint, failures cause 5xx.
- No metadata (on-chain fields only): Rejected — raw hashes and epoch numbers are not useful to humans choosing a DRep; names/descriptions are essential for the Daedalus governance UX.

## Decision: CIP-0119 metadata fields to expose

**Decision**: Expose the following CIP-0119 fields in `ApiDRepMetadata`:

| CIP-0119 field | API field | Type |
|---|---|---|
| `givenName` | `name` | `Text` |
| `objectives` | `objectives` | `Maybe Text` |
| `motivations` | `motivations` | `Maybe Text` |
| `qualifications` | `qualifications` | `Maybe Text` |
| `paymentAddress` | `payment_address` | `Maybe Text` |
| `doNotList` | `do_not_list` | `Bool` (default false) |
| `references` | `references` | `[ApiDRepReference]` |

**Rationale**: These are the fields Daedalus needs to present a DRep profile. The `image` field is excluded from v1 (large, not needed for directory listing).

## Decision: Blake2b-256 hash verification before storage

**Decision**: Before caching any fetched metadata document, verify that `blake2b-256(document) == anchorDataHash`. Documents failing verification are discarded and the entry's `metadata` remains `null`.

**Rationale**: FR-009. Matching the pool metadata worker behaviour. Prevents spoofed or corrupted metadata from appearing in the wallet UI.

## Decision: Configurable retry interval, default 15 minutes

**Decision**: The metadata background worker retries all DReps whose metadata has not yet been successfully fetched (or whose anchor URL has changed) on a configurable interval, defaulting to 15 minutes.

**Rationale**: FR-011, SC-005. Matches the existing pool metadata fetch interval. 15 minutes is conservative enough to avoid hammering anchor URLs while ensuring metadata is populated within a reasonable time after wallet startup.

---

## Decision: Split metadata out of list response; expose via separate endpoint

**Decision**: `GET /v2/dreps` and `GET /v2/dreps/suggested` return only `name`
(the CIP-0119 `givenName`) from the off-chain document; the `metadata` field
is always `null` in list responses. The full DRep record with embedded
metadata is available via `GET /v2/dreps/{drepId}`, which returns an
`ApiDRepInfo` object with the `metadata :: Maybe ApiDRepMetadata` field
populated (or null if unavailable).

**Rationale**: The list endpoints may return hundreds of DReps. Embedding the
full metadata object (objectives, motivations, qualifications, references) in
every list entry balloons the response payload and is rarely needed by a
directory-style UI. Clients that need full metadata for a specific DRep make
one additional targeted request to `GET /v2/dreps/{drepId}`. Returning the
full `ApiDRepInfo` (rather than metadata alone) means the client does not need
to merge data from two sources to render the detail view. This pattern also
lets the list response remain stable even if the metadata schema grows.

**Alternatives considered**:
- Full metadata on list: Rejected — payload size and schema-coupling concerns.
- No metadata on list at all: Rejected — clients need at least the name to
  render a DRep directory without a second round-trip per entry.
- Metadata-only detail endpoint (original design): Superseded — returning the
  full `ApiDRepInfo` with embedded metadata is simpler for clients and avoids
  a separate merge step in the UI.

---

## Decision: Resolve ipfs:// anchor URLs via Blockfrost IPFS gateway

**Decision**: When the DRep's anchor URL has the `ipfs://` scheme, the
metadata worker rewrites it to
`https://ipfs.blockfrost.dev/ipfs/<CID>` before fetching.
The rewrite is performed in `lib/wallet/src/Cardano/Wallet/DRep/Metadata.hs`.

**Rationale**: Many DRep metadata documents are published on IPFS. Direct
IPFS node access is not available in the wallet environment. The Blockfrost
IPFS gateway is a well-known, publicly reachable HTTP gateway that serves
IPFS content by CID. The `ipfs://` → `https://ipfs.blockfrost.dev/ipfs/`
rewrite is a single-line transformation that works for any CIDv0 or CIDv1.

**Alternatives considered**:
- Reject `ipfs://` URLs and return `metadata: null`: Rejected — a large
  fraction of DReps use IPFS for their anchor and would never have metadata
  populated.
- Allow the user to configure a custom IPFS gateway: Deferred to a later
  iteration; the Blockfrost gateway covers the common case.

---

## Decision: Cache LSQ result in DRepLayer with a 900-second TTL

**Decision**: `DRepLayer` stores the `GetDRepState` + `GetDRepStakeDistr`
result in an `IORef (Maybe (UTCTime, [DRepRegistration]))`. On each
`listDRepInfos` call the cache is returned if the entry is younger than
900 seconds; otherwise a fresh LSQ round-trip is made. The cache is
pre-warmed by a background `forkIO` at `DRepLayer` construction so the
first API request does not pay the LSQ latency.

**Rationale**: Both `GET /v2/dreps` and `GET /v2/dreps/suggested` call
`listDRepInfos`. Without caching, concurrent or back-to-back requests would
each issue two LSQ queries to the node. Ledger DRep state changes only on
epoch boundaries (roughly every 5 days on mainnet, 1 hour on preview), so
a 15-minute TTL is safe. The 900-second value matches `lsqCacheTtl` in
`DRep.Layer`.

**Alternatives considered**:
- No cache, fresh LSQ per request: Rejected — adds ~100–500 ms per request
  with no benefit for data that changes once per epoch.
- Cache indefinitely until a new block arrives: Deferred — requires hooking
  into the chain-follow infrastructure; TTL is simpler and sufficient.

---

## Decision: Suggested DReps selection algorithm

**Decision**: `GET /v2/dreps/suggested` ranks all DReps by voting power
descending, drops the top 35, then filters to `active` DReps whose metadata
is present and whose `doNotList` flag is `false`. From the resulting pool a
uniform random sample of up to `count` entries (default 20, max 200) is
drawn without replacement using partial Fisher-Yates.

**Rationale**: Excluding the largest 35 DReps avoids further concentrating
stake on already-dominant participants, which is a stated governance goal of
Cardano's delegated voting model. The metadata + `doNotList` filters ensure
only DReps that are both identifiable and willing to be listed are shown.
Uniform random sampling (rather than ranking) gives smaller but eligible
DReps a fair chance of being surfaced.

**Alternatives considered**:
- Return top-N by voting power: Rejected — reinforces stake concentration.
- Return the bottom-N: Rejected — surfaces dormant or low-quality DReps.
- Deterministic random (seeded by request params): Rejected — adds complexity
  with no user-facing benefit; different users should see different samples.

**Rationale**: FR-011, SC-005. Matches the existing pool metadata fetch interval. 15 minutes is conservative enough to avoid hammering anchor URLs while ensuring metadata is populated within a reasonable time after wallet startup.
