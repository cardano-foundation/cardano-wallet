# Research: Remove cardano-api Dependency

**Feature Branch**: `001-drop-cardano-api`
**Date**: 2026-04-09

## Decision: Upstream pin strategy

**Decision**: Pin `cardano-balance-transaction` main at `964e8a2` (current HEAD, post-merge of #32 and #33).

**Rationale**: Both PRs (#32 — library/test removal, #33 — generator cleanup) merged to main. The branch `002-remove-cardano-api` is stale; main is ahead. Pins must target main commits per project policy.

**Alternatives considered**: Pinning the branch tip (`dd21596`) — rejected because branch is behind main and project policy requires main-only pins.

## Decision: SealedTx replacement representation

**Decision**: Replace `InAnyCardanoEra Cardano.Api.Tx` with `EraValue (Tx era)` using `cardano-wallet-read`'s existing era GADT system, plus raw CBOR `ByteString`.

**Rationale**: The wallet already has `EraValue` as an era-existential wrapper in `cardano-wallet-read`. Ledger `Tx` types are era-indexed. This is a natural fit — no new abstractions needed. The raw CBOR bytes remain the serialization format for database and submission.

**Alternatives considered**: 
- Wrapping `InAnyShelleyBasedEra` from ouroboros-consensus — rejected, still pulls in cardano-api types.
- Storing only raw CBOR without decoded tx — rejected, too many call sites need decoded access.

## Decision: TxMetadata type ownership

**Decision**: Inline `TxMetadata` and `TxMetadataValue` as wallet-owned types in `Cardano.Wallet.Primitive.Types.Tx.TxMetadata`. Port the JSON schema conversion functions (`metadataFromJson`, `metadataToJson`, `TxMetadataJsonSchema`) from cardano-api source.

**Rationale**: The types are simple ADTs (map of Word64 to values). The JSON conversion is ~200 lines of pure Haskell. The ledger has `Cardano.Ledger.Metadata.Metadatum`, but with different JSON serialization and constructor names. Inlining preserves wire compatibility exactly while conversion functions bridge to ledger metadata at transaction construction and decoding boundaries.

**Alternatives considered**:
- Using ledger `Metadata` directly — rejected, different JSON format would break REST API contract.
- Depending on a cardano-api sub-library — rejected, defeats the purpose.

## Decision: Era GADT unification

**Decision**: Replace all `Cardano.Api.CardanoEra`, `AnyCardanoEra`, `InAnyCardanoEra`, `ShelleyBasedEra` usage with `cardano-wallet-read`'s `Era`, `EraValue`, `IsEra`.

**Rationale**: `cardano-wallet-read` already provides a complete era system covering Byron through Dijkstra. The bridge module `Cardano.Wallet.Primitive.Ledger.Read.Eras` currently converts between the two — this bridge becomes unnecessary.

**Alternatives considered**: None serious — the wallet's own era system was designed for this purpose.

## Decision: Transaction submission path

**Decision**: Replace `toConsensusGenTx` (cardano-api) with direct `mkShelleyTx` + hard fork combinator injection using ouroboros-consensus APIs.

**Rationale**: `toConsensusGenTx` is a thin wrapper over `mkShelleyTx` from `ouroboros-consensus-cardano`. The wallet already depends on ouroboros-consensus. The multi-era dispatch via `HardForkGenTx` is available directly.

**Alternatives considered**: Vendoring `toConsensusGenTx` — rejected, unnecessary indirection.

## Decision: Certificate construction

**Decision**: Use `cardano-ledger-api` certificate constructors directly. Handle Conway vs pre-Conway split using the wallet's existing era-indexed pattern.

**Rationale**: `cardano-ledger-api` exposes `mkRegTxCert`, `mkDelegTxCert`, `mkRegDRepTxCert` etc. The Conway era split (new cert types vs legacy) is already handled by the ledger's era-indexed types. `cardano-api`'s `Certificate` type was just a wrapper.

**Alternatives considered**: None — direct ledger usage is the obvious path.

## Decision: Serialization functions

**Decision**: Replace `serialiseToCBOR`/`deserialiseFromCBOR` with direct `serialize`/`decodeFull` from `cardano-ledger-binary`. Replace bech32 functions with `bech32`/`bech32-th` library directly.

**Rationale**: cardano-api's serialization functions are thin wrappers over `cardano-ledger-binary` (for CBOR) and the `bech32` package (for bech32). The wallet already transitively depends on both.

**Alternatives considered**: None — these are direct replacements with no semantic difference.

## Decision: NetworkId unification

**Decision**: Remove `networkIdVal :: SNetworkId n -> Cardano.NetworkId` conversion once all remaining cardano-api consumers migrate. All call sites switch to `Cardano.Wallet.Primitive.NetworkId.SNetworkId` or the value-level `NetworkId` directly. Where ouroboros/ledger need a magic-free `Network` value, convert via `networkIdToLedger` / `sNetworkIdToLedger`.

**Rationale**: The wallet's `NetworkId` still carries testnet magic, which `Ledger.Network` intentionally does not. `Ledger.Network` is the right type for ledger-facing code paths that only need the mainnet/testnet bit; wallet configuration, network information, and Byron witness construction still need the magic-bearing wallet type. The conversion to `Cardano.Api.NetworkId` was only needed because other cardano-api functions required it. Once those functions are replaced, the conversion is dead code.

**Alternatives considered**: None.

## Decision: Test generator migration

**Decision**: Replace `Cardano.Api.Gen` generators with `Arbitrary` instances from `cardano-ledger-conway:testlib` and wallet-native generators. Delete `lib/cardano-api-extra`.

**Rationale**: The ledger testlib provides comprehensive `Arbitrary` instances for all ledger types. The `cardano-api-extra` package was a shim to get generators for cardano-api wrapper types — once the wrapper types are gone, the generators follow.

**Alternatives considered**: Keeping generators as wallet-internal utilities — partially adopted, some wallet-specific generators (e.g., for SealedTx) will remain but produce ledger-native types.

## Decision: Migration ordering

**Decision**: Six phases, each independently mergeable:

1. **Pin bump** — Update `cardano-balance-transaction` pin, fix compilation against new API
2. **Ledger re-exports** — Replace `Cardano.Api` re-exports (SlotNo, EpochNo, etc.) with direct ledger imports
3. **Era GADTs** — Remove cardano-api era types, unify on cardano-wallet-read
4. **Core types** — SealedTx, TxMetadata, NetworkId, certificates
5. **Transaction construction** — Replace TxBodyContent/createTransactionBody, submission path
6. **Cleanup** — Remove cardano-api-extra, delete bridge modules, verify SC-001 through SC-007

**Rationale**: Each phase targets a distinct layer of the dependency. Earlier phases unlock later ones. The ordering minimizes merge conflicts between phases.

**Alternatives considered**: Single atomic removal — rejected, too risky for a production wallet. Would be a 5000+ line change impossible to review.
