# Data Model: Remove cardano-api Dependency

**Feature Branch**: `001-drop-cardano-api`
**Date**: 2026-04-09

## Entities

### SealedTx (modified)

**Location**: `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs`

**Current representation**:
```
SealedTx { valid :: Bool, unsafeCardanoTx :: InAnyCardanoEra Cardano.Tx, serialisedTx :: ByteString }
```

**Target representation**:
```
SealedTx { valid :: Bool, unsafeLedgerTx :: EraValue LedgerTx, serialisedTx :: ByteString }
```

Where `LedgerTx era` wraps `Ledger.Tx (CardanoLedgerEra era)` using the wallet-read era system.

**Validation rules**: 
- `serialisedTx` must round-trip through CBOR encode/decode
- `valid` is True for all properly-constructed instances
- Database-stored CBOR from previous versions must deserialize into the new representation

**State transitions**: None — SealedTx is immutable once constructed.

### TxMetadata (new wallet-owned type)

**Location**: `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/TxMetadata.hs` (new module)

**Fields**:
- `TxMetadata`: `Map Word64 TxMetadataValue`
- `TxMetadataValue`: ADT with constructors `TxMetaMap`, `TxMetaList`, `TxMetaNumber`, `TxMetaBytes`, `TxMetaText`

**Validation rules**:
- Byte strings limited to 64 bytes per chunk
- Text strings limited to 64 bytes UTF-8 encoded per chunk
- JSON serialization must match `TxMetadataJsonDetailedSchema` format exactly

**Relationships**: Used by SealedTx (embedded), REST API response types, SQLite storage layer.

### NetworkId (simplified)

**Location**: `lib/primitive/lib/Cardano/Wallet/Primitive/NetworkId.hs`

**Change**: Remove `networkIdVal :: SNetworkId n -> Cardano.NetworkId` conversion. Add `networkIdToLedger :: SNetworkId n -> Ledger.Network` for the few places that need a ledger-level network value.

### Era types (unchanged, bridge removed)

**Location**: `lib/cardano-wallet-read/haskell/Cardano/Wallet/Read/Eras.hs`

**Change**: No modification to the era GADT itself. The bridge module `Cardano.Wallet.Primitive.Ledger.Read.Eras` loses its `cardanoApiEraFromRead`/`readEraFromCardanoApi` functions.

## Relationship Map

```
REST API ──uses──> TxMetadata (JSON serialization)
REST API ──uses──> SealedTx (CBOR hex in responses)
SealedTx ──contains──> EraValue LedgerTx (decoded)
SealedTx ──contains──> ByteString (raw CBOR)
SealedTx ──submits-via──> GenTx (ouroboros-consensus)
Wallet DB ──stores──> SealedTx.serialisedTx (CBOR blob)
Wallet DB ──stores──> TxMetadata (JSON text)
NetworkId ──converts-to──> Ledger.Network (for ouroboros)
Era GADT ──indexes──> LedgerTx, certificates, all era-dependent types
```

## Migration Impact

No database schema changes. Stored CBOR and JSON remain wire-compatible. The representation changes are internal to the Haskell types — serialization format is preserved exactly.
