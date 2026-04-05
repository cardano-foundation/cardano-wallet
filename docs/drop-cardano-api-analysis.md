# Feasibility: Dropping cardano-api from cardano-wallet

## Current State

9 packages depend on `cardano-api`, with ~60 import statements across ~35 files.

| Package | Role |
|---------|------|
| `cardano-wallet-primitive` | Core types (`SealedTx`, `NetworkId`, `TxMetadata`) |
| `cardano-wallet` | Transaction construction, delegation, voting |
| `cardano-wallet-api` | REST API, error handling |
| `cardano-wallet-network-layer` | Node client, tx submission |
| `cardano-wallet-unit` | Tests |
| `cardano-wallet-integration` | Integration tests |
| `cardano-wallet-benchmarks` | Benchmarks |
| `cardano-api-extra` | Extensions to cardano-api (generators, certificate helpers) |
| `local-cluster` | Test cluster setup |

## Usage Classification

### Easy — Ledger re-exports (scattered, many files)

Types cardano-api just re-exports from `cardano-ledger-*` / `ouroboros-*`:
`SlotNo`, `ShelleyGenesis`, `NodeToClientVersion`, era types.

Mechanical import swaps.

### Medium — Serialization (~29 occurrences, 8 files)

`serialiseToCBOR`, `deserialiseFromCBOR`, `serialiseToRawBytes`,
`deserialiseFromRawBytes`, `serialiseToBech32`, `deserialiseFromBech32`.

Wrappers around `Cardano.Ledger.Binary` with type-safe `AsType` dispatch.
Replaceable with direct ledger CBOR calls — more verbose but straightforward.

### Medium — NetworkId / StakeAddress (~29 occurrences, 6 files)

`NetworkId`, `NetworkMagic`, `StakeAddress`, `makeStakeAddress`.

The wallet already defines its own `NetworkId` in
`Cardano.Wallet.Primitive.NetworkId`. The cardano-api one coexists via
conversion functions — can be unified.

### Medium — Certificate builders (~25 occurrences, 3 files)

`makeStakeAddressDelegationCertificate`,
`makeStakeAddressRegistrationCertificate`, etc.

Handle pre-Conway vs Conway certificate format differences. Replaceable
with direct ledger certificate construction + era branching.

### Medium-High — TxMetadata + JSON (~32 occurrences, 4+ files)

`TxMetadata`, `TxMetadataValue`, `metadataFromJson`, `metadataToJson`,
`TxMetadataJsonSchema`.

Exposed in the wallet's public REST API types and DB serialization.
Would need to inline the type + JSON conversion from cardano-api source,
or find a ledger equivalent.

### Hard — Era GADTs (~37 occurrences, 10 files)

`AnyCardanoEra`, `CardanoEra`, `InAnyCardanoEra`, `InAnyShelleyBasedEra`,
`IsCardanoEra`, `ShelleyBasedEra`.

Deeply woven era GADT system. The wallet has its own era system in
`cardano-wallet-read` but bridges between them (`fromAnyCardanoEra`).
Would need full migration to the wallet's own era representation.

### Hard — Tx / SealedTx (~38 occurrences, 8 files)

`Tx era`, `ShelleyTx`, `InAnyCardanoEra Tx`, `TxBody`, `TxBodyContent`,
`createTransactionBody`, `makeSignedTransaction`.

`SealedTx` stores `InAnyCardanoEra Tx` internally — it's the wallet's
canonical serialized transaction type. Transaction construction in
`Shelley/Transaction.hs` uses `TxBodyContent` as a high-level builder.

### Hard — Transaction submission (1 file, critical path)

`toConsensusGenTx` in `Network/Implementation.hs` converts `Cardano.Api.Tx`
to Ouroboros `GenTx` for node submission. Non-trivial multi-era hard fork
combinator dispatch.

## Blockers

1. **`SealedTx` / `InAnyCardanoEra Tx`** — load-bearing transaction representation
2. **`toConsensusGenTx`** — the bridge to Ouroboros for tx submission
3. **`TxBodyContent` / `createTransactionBody`** — high-level tx builder abstraction
4. **`TxMetadata`** — exposed in public REST API

## Proposed Phases

| Phase | Scope | Difficulty |
|-------|-------|-----------|
| 1 | Replace re-exported ledger types, eliminate trivial imports (8-10 quick-win files) | Easy |
| 2 | Inline `TxMetadata` type + JSON conversion | Medium |
| 3 | Replace `NetworkId`, `StakeAddress`, address construction with ledger-native | Medium |
| 4 | Replace certificate construction with direct ledger calls | Medium |
| 5 | Replace `SealedTx` internals — use ledger `Tx` instead of `Cardano.Api.Tx` | Hard |
| 6 | Replace `toConsensusGenTx` and tx construction — eliminate last cardano-api usage | Hard |

Phase 1-4 can proceed incrementally. Phase 5-6 are the structural changes
that require careful design — particularly `SealedTx` since it's the central
transaction representation used across the entire codebase.

`cardano-api-extra` is eliminated entirely once the migration completes.

## Quick Wins

Files with minimal cardano-api usage (1-2 imports each):

- `Cardano/Wallet/Primitive/Ledger/Read/Eras.hs` — era conversion only
- `Cardano/Api/Extra.hs` — 10-line helper, trivially replaceable
- `Cardano/Wallet/Network.hs` — `AnyCardanoEra` in type signature only
- `Cardano/Wallet/Api/Http/Server.hs` — `NetworkId` only
- `Cardano/Wallet/Api/Types/Era.hs` — era mapping only
- `Cardano/Wallet/Transaction.hs` — `AnyCardanoEra` in type signatures only
- `Cardano/Ledger/Credential/Safe.hs` — `SlotNo` only (already from ouroboros)
- `Cardano/Wallet/DB/Sqlite/Types.hs` — `TxMetadata` JSON only
- `Cardano/Wallet/Primitive/NetworkId.hs` — bridge to own `NetworkId` type
- `Cardano/Wallet/Orphans.hs` — orphan instances for `TxMetadata`
