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

### ApiDRepMetadata (API response element)

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

### ApiDRepInfo (response element)

```haskell
data ApiDRepInfo = ApiDRepInfo
    { id          :: Text               -- CIP-129 bech32 DRep credential ID
    , credential  :: ApiDRepCredential
    , status      :: DRepStatus
    , expiryEpoch :: Word64             -- serialised as "expiry_epoch"
    , votingPower :: ApiT Coin          -- serialised as string quantity
    , deposit     :: ApiT Coin          -- serialised as integer quantity (< MAX_SAFE_INTEGER)
    , anchor      :: Maybe ApiAnchor
    , metadata    :: Maybe ApiDRepMetadata  -- null until fetched and verified
    }
```

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
  },
  {
    "id": "drep_script1rl5lq4n9t4zm7...",
    "credential": { "type": "script_hash", "hash": "ff8d9e..." },
    "status": "inactive",
    "expiry_epoch": 315,
    "voting_power": { "quantity": "0", "unit": "lovelace" },
    "deposit": { "quantity": 500000000, "unit": "lovelace" },
    "anchor": null,
    "metadata": null
  }
]
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
| `anchor.url` | `anchorUrl :: Url` | Text |
| `anchor.data_hash` | `anchorDataHash :: SafeHash Blake2b_256 AnchorData` | hex |
| `metadata` | SQLite cache keyed on `(DRepID, anchorDataHash)` | null until fetched & verified |
| `metadata.name` | CIP-0119 `givenName` | |
| `metadata.objectives` | CIP-0119 `objectives` | optional |
| `metadata.motivations` | CIP-0119 `motivations` | optional |
| `metadata.qualifications` | CIP-0119 `qualifications` | optional |
| `metadata.payment_address` | CIP-0119 `paymentAddress` | optional |
| `metadata.do_not_list` | CIP-0119 `doNotList` | default false |
| `metadata.references` | CIP-0119 `references[].{label,uri}` | |

## State Transitions

DRep status is a derived field, not stored:
- `Active`: `drepExpiry >= currentEpoch`
- `Inactive`: `drepExpiry < currentEpoch`

DReps are only removed from the ledger state via a de-registration certificate. An expired DRep remains in the registry with `status: "inactive"` until explicitly de-registered.
