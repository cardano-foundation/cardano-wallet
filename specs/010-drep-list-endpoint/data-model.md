# Data Model: GET /v2/dreps

## New Haskell Types

### DRepMetadata (primitive, cached off-chain document)

```haskell
data DRepMetadata = DRepMetadata
    { drepMetaName           :: !Text
    , drepMetaObjectives     :: !(Maybe Text)
    , drepMetaMotivations    :: !(Maybe Text)
    , drepMetaQualifications :: !(Maybe Text)
    , drepMetaPaymentAddress :: !(Maybe Text)
    , drepMetaDoNotList      :: !Bool
    , drepMetaReferences     :: ![DRepMetaReference]
    } deriving (Generic, Eq, Show)
      deriving anyclass NFData

data DRepMetaReference = DRepMetaReference
    { drepRefLabel :: !Text
    , drepRefUri   :: !Text
    } deriving (Generic, Eq, Show)
      deriving anyclass NFData
```

Maps from CIP-0119 fields: `givenName → name`, `objectives`, `motivations`, `qualifications`, `paymentAddress → payment_address`, `doNotList → do_not_list`, `references`.

**Parsing note**: CIP-0119 places `references` at the document top level (alongside `body`/`authors`), not inside `body`. The parser must read it from the top level. `doNotList` is an informal extension (not canonical CIP-0119); it defaults to `false` when absent.

### ApiDRepMetadata (embedded in ApiDRepInfo for GET /v2/dreps/{drepId})

```haskell
data ApiDRepMetadata = ApiDRepMetadata
    { apiDRepMetaName           :: !Text
    , apiDRepMetaObjectives     :: !(Maybe Text)
    , apiDRepMetaMotivations    :: !(Maybe Text)
    , apiDRepMetaQualifications :: !(Maybe Text)
    , apiDRepMetaPaymentAddress :: !(Maybe Text)
    , apiDRepMetaDoNotList      :: !Bool
    , apiDRepMetaReferences     :: ![ApiDRepMetaReference]
    } deriving (Generic, Eq, Show)

data ApiDRepMetaReference = ApiDRepMetaReference
    { apiDRepRefLabel :: !Text
    , apiDRepRefUri   :: !Text
    } deriving (Generic, Eq, Show)
```

JSON field names: `name`, `objectives`, `motivations`, `qualifications`, `payment_address`, `do_not_list`, `references`.

### ApiDRepInfo (response element for list endpoints and GET /v2/dreps/{drepId})

```haskell
data ApiDRepInfo = ApiDRepInfo
    { id          :: Text               -- CIP-129 bech32 DRep credential ID
    , credential  :: ApiDRepCredential
    , status      :: DRepStatus
    , expiryEpoch :: Word64             -- serialised as "expiry_epoch"
    , votingPower :: ApiT Coin          -- serialised as string quantity
    , deposit     :: ApiT Coin          -- serialised as integer quantity (< MAX_SAFE_INTEGER)
    , anchor      :: Maybe ApiAnchor
    , name        :: Maybe Text         -- CIP-0119 givenName; null until fetched and verified
    , metadata    :: Maybe ApiDRepMetadata  -- null in list/suggested; populated in GET /v2/dreps/{drepId}
    }
```

Only the DRep's name (CIP-0119 `givenName`) is inlined in the list response;
`metadata` is always `null` there. The full `ApiDRepInfo` with embedded
`metadata` is returned by `GET /v2/dreps/{drepId}`.

### ApiDRepCredential

```haskell
data ApiDRepCredential = ApiDRepCredential
    { credType :: DRepCredentialType   -- "key_hash" | "script_hash"
    , hash     :: ApiT (Hash "DRepKey") -- hex-encoded 28-byte hash
    }

data DRepCredentialType = KeyHash | ScriptHash
```

### DRepStatus

```haskell
data DRepStatus = Active | Inactive
-- JSON: "active" | "inactive"
```

### ApiAnchor

```haskell
data ApiAnchor = ApiAnchor
    { url      :: Text
    , dataHash :: Text   -- lowercase hex, field name "data_hash"
    }
```

## JSON Wire Format

### GET /v2/dreps and GET /v2/dreps/suggested

The `metadata` field is always `null` in list and suggested responses.

```json
[
  {
    "id": "drep1y4yzl4zeuh3r0l8t0j8k4r9p3wz2m9qv3k7xn...",
    "credential": {
      "type": "key_hash",
      "hash": "deadbeef..."
    },
    "status": "active",
    "expiry_epoch": 520,
    "voting_power": { "quantity": "123456789012345", "unit": "lovelace" },
    "deposit": { "quantity": 500000000, "unit": "lovelace" },
    "anchor": {
      "url": "https://example.com/drep.jsonld",
      "data_hash": "a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de"
    },
    "name": "Alice the DRep",
    "metadata": null
  },
  {
    "id": "drep_script1rl5lq4n9t4zm7...",
    "credential": { "type": "script_hash", "hash": "ff8d9e..." },
    "status": "inactive",
    "expiry_epoch": 315,
    "voting_power": { "quantity": "0", "unit": "lovelace" },
    "deposit": { "quantity": 500000000, "unit": "lovelace" },
    "anchor": null,
    "name": null,
    "metadata": null
  }
]
```

### GET /v2/dreps/{drepId}

Returns the full `ApiDRepInfo` with the `metadata` field populated when
available. Unknown DRep IDs and the abstain/no-confidence sentinels return
`404 Not Found`.

```json
{
  "id": "drep1y4yzl4zeuh3r0l8t0j8k4r9p3wz2m9qv3k7xn...",
  "credential": {
    "type": "key_hash",
    "hash": "deadbeef..."
  },
  "status": "active",
  "expiry_epoch": 520,
  "voting_power": { "quantity": "123456789012345", "unit": "lovelace" },
  "deposit": { "quantity": 500000000, "unit": "lovelace" },
  "anchor": {
    "url": "https://example.com/drep.jsonld",
    "data_hash": "a14a5ad4f36bddc00f92ddb39fd9ac633c0fd43f8bfa57758f9163d10ef916de"
  },
  "name": "Alice the DRep",
  "metadata": {
    "name": "Alice the DRep",
    "objectives": "Promote decentralisation and fair governance.",
    "motivations": "Long-time Cardano community member.",
    "qualifications": "10 years in distributed systems.",
    "payment_address": "addr1...",
    "do_not_list": false,
    "references": [
      { "label": "Website", "uri": "https://alice.example.com" }
    ]
  }
}
```

When the DRep exists but has no anchor, or fetch/verification failed:

```json
{
  "id": "drep_script1rl5lq4n9t4zm7...",
  ...
  "name": null,
  "metadata": null
}
```

## Ledger → API Mapping

| API field | Source | Notes |
|---|---|---|
| `id` | `Credential 'DRepRole` → `encodeDRepIDBech32` | CIP-129 "drep1..." bech32 |
| `credential.type` | `KeyHashObj` → `"key_hash"`, `ScriptHashObj` → `"script_hash"` | |
| `credential.hash` | `KeyHash bytes` or `ScriptHash bytes` | 28 bytes → hex |
| `status` | compare `drepExpiry` to `GetEpochNo` result | `>=` current → "active" |
| `expiry_epoch` | `drepExpiry :: EpochNo` | Word64 |
| `voting_power` | `Map DRep Coin` from `GetDRepStakeDistr`, lookup `DRepCredential cred` | String quantity |
| `deposit` | `drepDeposit :: CompactForm Coin` → `fromCompact` | Integer quantity |
| `anchor` | `drepAnchor :: StrictMaybe Anchor` | null if SNothing |
| `anchor.url` | `anchorUrl :: Url` | Text; may be `ipfs://` scheme |
| `anchor.data_hash` | `anchorDataHash :: SafeHash Blake2b_256 AnchorData` | hex |
| `name` | `drepMetaName` from SQLite cache | null until fetched & verified |
| `metadata` | SQLite cache keyed on `(DRepID, anchorDataHash)` | null in list/suggested; full CIP-0119 document in GET /v2/dreps/{drepId} |

## State Transitions

DRep status is a derived field, not stored:
- `Active`: `drepExpiry >= currentEpoch`
- `Inactive`: `drepExpiry < currentEpoch`

DReps are only removed from the ledger state via a de-registration certificate. An expired DRep remains in the registry with `status: "inactive"` until explicitly de-registered.

## LSQ Caching

`DRepLayer` caches the `GetDRepState` + `GetDRepStakeDistr` LSQ result in an
`IORef` with a 900-second TTL. The cache is pre-warmed on wallet startup so
the first API request is fast. Both `GET /v2/dreps` and `GET /v2/dreps/suggested`
read from the same cache.

## Metadata Cache (drep_metadata table)

The `drep_metadata` SQLite table is keyed by content hash (Blake2b-256) and
grows unboundedly as DReps update their anchor. It must be garbage-collected
periodically, pruning rows whose hash is no longer referenced by any active
DRep's anchor. This mirrors the `lastMetadataGC` mechanism used by pool
metadata (see `Cardano.Pool.DB.Layer`).

The `drepFetchAttempts` table tracks failed/in-progress fetch attempts per
hash. On a successful `putDRepMetadata`, both the SQLite layer and the
in-memory Model layer MUST delete the corresponding `drepFetchAttempts` row
to maintain pool-DB property-test equivalence.

## Dead Code (post-consolidation)

The `drep_anchor` table, `DRepLayer.getDRepMetadata`, and the worker's
`putDRepAnchorHash` write are write-only after the `GET /v2/dreps/{id}/metadata`
endpoint was consolidated into `GET /v2/dreps/{id}`. These should be removed.
