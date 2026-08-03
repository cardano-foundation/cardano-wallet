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
