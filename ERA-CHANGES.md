# cardano-node 10.6.2 Bump — DijkstraEra Changes

## Overview

This document describes all changes required to bump cardano-wallet from
cardano-node 10.5.x to 10.6.2, which introduces **DijkstraEra** (the 8th
Cardano era, protocol version 12).

**Branch**: `chore/bump-node-10.6.2`
**PR**: #5197

---

## 1. Build System & Dependencies

### 1.1 `flake.nix` / `flake.lock`

- **haskell.nix** pinned to a newer rev supporting GHC and Dijkstra ledger.
- **CHaP** advanced to `2026-02-09`; Hackage rolled back to `2025-09-29` to
  avoid breakage from newer upstream packages while CHaP carries the node
  10.6.2 ecosystem.
- Old legacy inputs removed (`ghc-8.6.5-iohk`, `ghc910X`, `ghc911`, `hydra`,
  `nix`, `lowdown-src`, `nixpkgs-regression`, `em`).

### 1.2 `nix/haskell.nix`

- **Removed `-Wno-deriving-typeable`**: GHC 9.12 warns on redundant `deriving
  Typeable`. Source code cleaned up instead (see §6).
- **streaming-commons Windows patch**: Reverted the `mkForce []` override —
  haskell.nix's built-in patch is still needed until `index-state` resolves
  `streaming-commons >= 0.2.3.1`.

### 1.3 `cabal.project`

Key dependency bumps:

| Package | Old | New |
|---|---|---|
| `cardano-api` | 10.16 | **10.23.0.0** |
| `cardano-ledger-core` | 1.17 | **1.18.0.0** |
| `cardano-ledger-alonzo` | 1.13 | **1.14.0.0** |
| `cardano-ledger-babbage` | 1.11 | **1.12.0.0** |
| `cardano-ledger-conway` | 1.19 | **1.20.0.0** |
| `cardano-ledger-shelley` | 1.16 | **1.17.0.0** |
| `ouroboros-consensus` | 0.27 | **0.30.0.1** |
| `ouroboros-consensus-cardano` | 0.25 | **0.26.0.3** |
| `ouroboros-network` | — | **0.22.6.0** |
| `plutus-core/ledger-api/tx` | — | **1.57.0.0** |
| `io-classes` | — | **1.8.0.1** |

Other changes:
- All `.cabal` files drop per-package version bounds for Cardano ecosystem deps;
  pinning is now centralised in `cabal.project` `constraints:`.
- `allow-newer` for `strict-stm:io-classes` / `si-timers:io-classes` removed
  (io-classes 1.8 is compatible).
- `source-repository-package` for cardano-cli removed (resolved via CHaP).
- Package renames: `crypton-asn1-encoding` → `asn1-encoding`,
  `time-hourglass` → `hourglass`.

---

## 2. `cardano-wallet-read` (Read Layer)

The lowest-level era-indexed layer. Every type family and era-dispatch function
gains a `Dijkstra` branch.

### 2.1 Era Definition — `KnownEras.hs`

DijkstraEra is the 8th member (index 7):

```haskell
type KnownEras =
    '[ ByronEra, ShelleyEra, AllegraEra, MaryEra
     , AlonzoEra, BabbageEra, ConwayEra, DijkstraEra ]

indexOfEra Dijkstra = 7
```

`Era` GADT, `IsEra` instance, and `Dijkstra` type alias all added.

### 2.2 Era-Indexed Existentials — `EraValue.hs`

`parseEraIndex 7 = Just $ EraValue Dijkstra`. All instances (`Show`, `Eq`,
`Ord`, `NFData`) gain Dijkstra cases.

### 2.3 Transaction Type Family — `Tx.hs`

**Significant refactor.** Previously:
- Shelley–Mary used `ShelleyTx` directly
- Alonzo–Conway used `AlonzoTx` directly

Now **all post-Byron eras use `Core.Tx era`** (the associated type from
`EraTx`):

```haskell
type family TxT era where
    TxT Byron    = Byron.ATxAux ()
    TxT Shelley  = Core.Tx Shelley
    TxT Allegra  = Core.Tx Allegra
    ...
    TxT Dijkstra = Core.Tx Dijkstra
```

**Why**: The ledger now wraps each era's internal tx in a newtype, e.g.
`MkDijkstraTx (AlonzoTx DijkstraEra)`. `Core.Tx` is the abstract unified
type.

### 2.4 Block Type Family — `Block.hs`

```haskell
BlockT Dijkstra = O.ShelleyBlock (Praos O.StandardCrypto) Dijkstra
```

`fromConsensusBlock` gains:
```haskell
O.BlockDijkstra block -> EraValue (Block block :: Block Dijkstra)
```

### 2.5 Block Transaction Extraction — `Txs.hs`

Constraint simplified:
- Old: `EraSegWits era, EncCBOR (ShelleyProtocolHeader proto)`
- New: `EraBlockBody era`

Body extraction changes from `toList (fromTxSeq txs)` to
`toList (txs ^. txSeqBlockBodyL)` (new lens-based API).

### 2.6 Header Hash Extraction — `HeaderHash.hs`

Constraints `EncCBOR`, `EncCBORGroup`, `Core.Era`,
`ShelleyProtocolHeader` all removed. Simplified to:

```haskell
getHeaderHashShelley
    :: ProtocolHeaderSupportsEnvelope (praos StandardCrypto)
    => O.ShelleyBlock (praos StandardCrypto) era -> ShelleyHash
```

### 2.7 Transaction Feature Type Families

Every feature (Inputs, Outputs, Fee, Mint, Certificates, Withdrawals,
CollateralInputs, CollateralOutputs, ReferenceInputs, ExtraSigs, Validity,
Integrity, Metadata, Witnesses) gains `Dijkstra` case. Pattern: Dijkstra uses
same underlying ledger type as Conway, with these exceptions:

- **Certificates**: `CertificatesType Dijkstra = StrictSeq (DijkstraTxCert
  Dijkstra)` — Dijkstra has its own certificate type.
- **ExtraSigs**: uses `reqSignerHashesTxBodyG` (getter) instead of
  `reqSignerHashesTxBodyL` (lens).

### 2.8 Withdrawals — `Withdrawals.hs`

Refactored from standalone `shelleyWithdrawals` function with `view` to inline
`tx ^. bodyTxL . withdrawalsTxBodyL`, adding an explicit `EraTx` constraint.

### 2.9 Transaction Generator — `Dijkstra.hs` (NEW)

New module `Cardano.Wallet.Read.Tx.Gen.Dijkstra` providing `mkDijkstraTx`.
Key observations:

- Constructs via `MkDijkstraTx $ AlonzoTx body wits valid aux`
- `DijkstraTxBody` has two new fields vs Conway:
  - `guards :: OSet (Credential 'Guard)` — a new credential role
  - An additional empty field
- Uses `natVersion @12` (protocol version 12, Conway was 10)

### 2.10 Other Generator Updates

All existing era generators updated to use era-specific `Tx` newtype
constructors (`MkAllegraTx`, `MkAlonzoTx`, `MkBabbageTx`, `MkConwayTx`) and
`Core.Tx era` / `L.TxBody era` instead of concrete types.

---

## 3. `cardano-wallet-primitive` (Primitive Layer)

### 3.1 Certificates — `Certificates.hs`

New `mkDijkstraCertsK` handles `DijkstraTxCert`:

```haskell
data DijkstraTxCert era
    = DijkstraTxCertDeleg (DijkstraDelegCert era)
    | DijkstraTxCertPool PoolCert
    | DijkstraTxCertGov (ConwayGovCert era)
```

`DijkstraDelegCert` subtypes:
- `DijkstraRegCert` — stake key registration (with deposit)
- `DijkstraUnRegCert` — deregistration (with deposit return)
- `DijkstraDelegCert` — delegation to pool/DRep
- `DijkstraRegDelegCert` — combined registration + delegation

Pool certs reuse `PoolCert` (`RegPool`/`RetirePool`). Gov certs reuse
`ConwayGovCert`.

### 3.2 Outputs — `Outputs.hs`

New `fromDijkstraTxOut`, identical structure to `fromConwayTxOut`:

```haskell
fromDijkstraTxOut
    :: Babbage.BabbageTxOut DijkstraEra
    -> (W.TxOut, Maybe (AlonzoScript DijkstraEra))
```

### 3.3 Scripts — `Scripts.hs`

New `dijkstraAnyExplicitScript`. All era functions updated:
`Alonzo.TimelockScript` → `Alonzo.NativeScript`.

### 3.4 Witness Count — `WitnessCount.hs`

New `dijkstraWitnessCount` following same pattern as `conwayWitnessCount`,
using `fromDijkstraTxOut` and `dijkstraAnyExplicitScript`.

### 3.5 Protocol Parameters & Block Processing — `Shelley.hs`

1. **`fromDijkstraPParams`**: New function, identical structure to
   `fromConwayPParams`. Decentralisation fixed at 0%.

2. **`nodeToClientVersions`**: Extended from `[V_16, V_17]` to
   `[V_16 .. V_22]` for Dijkstra support.

3. **Constraint simplifications**: `getProducer`, `getBabbageProducer`,
   `getConwayProducer` drop `Era era`, `EncCBORGroup (TxSeq era)` constraints.

4. **`fromPoolDistr`**: Changed from `Consensus.PoolDistr crypto` to
   `Ledger.PoolDistr` (monomorphic). Types moved to `Cardano.Ledger.State`.

5. **Certificate witness handling**: `cardanoCertKeysForWitnesses` now uses
   `Cardano.Compat.selectStakeCredentialWitness` (from
   `Cardano.Api.Compatible.Certificate`).

### 3.6 Script Conversion — `Convert.hs`

- `toWalletScript`: catch-all for new timelock constructors
- `toPlutusScriptInfo`: new `Ledger.PlutusV4 -> PlutusVersionV4`

### 3.7 Metadata — `Metadata.hs`

Gains Dijkstra case in era dispatch (same pattern as Conway).

### 3.8 Mint — `Mint.hs`

Gains Dijkstra case for minting/burning policy extraction.

### 3.9 Collateral Outputs — `CollateralOutputs.hs`

Gains Dijkstra case using `fromDijkstraTxOut`.

---

## 4. `cardano-balance-tx` (Balance Transaction Engine)

### 4.1 `RecentEraConstraints` — `Eras.hs`

Drops the constraint `Core.Tx era ~ Babbage.AlonzoTx era`. Now that `Core.Tx`
is a newtype per era (not a type synonym for `AlonzoTx`), this equality no
longer holds. `fromAnyCardanoEra` gains `_ -> Nothing` for Dijkstra.

### 4.2 Script Downgrade — `Tx.hs`

`downgradeScript` updated: `TimelockScript` → `Alonzo.NativeScript`.

### 4.3 Transaction Update — `Balance.hs`

`toLedgerScript` in `updateTx`: `TimelockScript` → `NativeScript` for both
Babbage and Conway.

### 4.4 Script Integrity Hash — `Redeemers.hs`

**Substantial rewrite** of `addScriptIntegrityHash`:

1. `Alonzo.txdats'` renamed to `Alonzo.txdats`
2. `hashScriptIntegrity` now takes a single `ScriptIntegrity` record instead
   of three arguments
3. **Critical fix**: When all components (redeemers, datums, language views)
   are empty, sets `SNothing` instead of computing an empty hash:

```haskell
integrityHash
    | Map.null (view Alonzo.unRedeemersL rdmrs)
    , Map.null (view Alonzo.unTxDatsL dats)
    , Set.null langViews = SNothing
    | otherwise =
        SJust $ Alonzo.hashScriptIntegrity
              $ Alonzo.ScriptIntegrity rdmrs dats langViews
```

This fix (commit `ffda1e6fa3`) was needed because the ledger now rejects
transactions with a non-empty integrity hash but no Plutus scripts.

### 4.5 Witness Estimation — `Sign.hs`

`estimateDelegSigningKeys` refactored from pattern-matching on
`CardanoApi.ShelleyRelatedCertificate` / `ConwayCertificate` to using
`Exp.Certificate` (experimental API):

```haskell
estimateDelegSigningKeys (Exp.Certificate txCert) =
    case txCert of
        Shelley.RegTxCert _          -> 0
        Shelley.DelegStakeTxCert c _ -> estimateWitNumForCred c
        Shelley.UnRegTxCert c        -> estimateWitNumForCred c
        _                            -> 1
```

`toTimelockScript`: `Alonzo.TimelockScript` → `Alonzo.NativeScript`.

---

## 5. Network Layer

### 5.1 Node Protocol Client — `Implementation.hs`

- **`codecConfig`**: 8th `ShelleyCodecConfig` for Dijkstra.
- **Mux tracer**: `nctMuxTracer = nullTracer` → `nctMuxTracers = nullTracers`
  (mux library API change to a record of tracers).
- **New constraints**: `MonadThrow m` / `MonadMask m` on client functions
  (ouroboros-network-framework API change).
- **`_getUTxOByTxIn`**: `error` stub for Dijkstra.

### 5.2 Era Dispatch — `LocalStateQuery/Extra.hs`

`onAnyEra` and `onAnyEra'` gain an 8th `onDijkstra` parameter.
`QueryIfCurrentDijkstra` from consensus. `eraIndexToAnyCardanoEra` maps
index 7 to `DijkstraEra`.

### 5.3 Local State Queries

- **`StakeDistribution.hs`**: Uses `Shelley.GetStakeDistribution2` instead of
  `GetStakeDistribution`.
- **`UTxO.hs`**: `error` stub for Dijkstra with TODO.
- **`PParams.hs`**, **`RewardAccount.hs`**: Pass same Shelley query for
  Dijkstra.

---

## 6. API Layer

### 6.1 Era Mapping — `Api/Types/Era.hs`

Both `fromReadEra` and `fromAnyCardanoEra` gain `error` stubs for Dijkstra.

### 6.2 API Types — `Api/Types.hs`

`Aeson.Value` type family instances moved from `Client.hs` to `Types.hs`,
eliminating orphan instances.

### 6.3 Typeable Cleanup — `Api/Types/Error.hs`

All API error types drop `deriving Typeable` (redundant in GHC 9.12).

---

## 7. Wallet Core

### 7.1 `Wallet.hs`

`pparamsInRecentEra`: `error` stub for Dijkstra.

### 7.2 `Pools.hs`

`forAllBlocks`: `error` stub for `BlockDijkstra`.

### 7.3 `Transaction.hs`

- `withRecentEraLedgerTx`: `error` stub for Dijkstra.
- New `certToLedger` bridges deprecated `Cardano.Api.Certificate` to
  `Cardano.Api.Experimental.Certificate`.
- `{-# OPTIONS_GHC -Wno-deprecations #-}` added (temporary).

### 7.4 `Delegation.hs` / `Voting.hs`

`-Wno-deprecations` added. `Cardano.Api.Shelley` → `Cardano.Api`.

---

## 8. DijkstraEra Wiring Pattern

The systematic pattern for adding each era:

1. **Type family**: `TypeFamily Dijkstra = <ledger type>` (usually same as
   Conway)
2. **Era dispatch**: `Dijkstra -> <implementation>` case in `getEra*`
   functions
3. **`fromConsensusBlock`**: Map `O.BlockDijkstra` to `EraValue`
4. **`onAnyEra`**: Add 8th parameter
5. **Local state queries**: Add Dijkstra query (or `error` stub)
6. **API mapping**: `error` stubs until `ApiDijkstra` exists

### DijkstraEra vs Conway

Structurally very similar — Praos consensus, `BabbageTxOut`, `AlonzoScript`,
same integrity/validity/fee mechanisms. Differences:

| Aspect | Conway | Dijkstra |
|---|---|---|
| Protocol version | 10 | **12** |
| Certificate type | `ConwayTxCert` | **`DijkstraTxCert`** |
| Required signers | lens (`reqSignerHashesTxBodyL`) | **getter** (`reqSignerHashesTxBodyG`) |
| New credential role | — | **`Guard`** |
| TxBody fields | — | **2 additional empty fields** |
| Plutus | V1–V3 | V1–**V4** |

---

## 9. TODOs & Error Stubs

`TODO.md` catalogs 16 `error` stubs across 14 files that must be implemented
when DijkstraEra is promoted to `RecentEra`:

| File | Function |
|---|---|
| `Api/Types/Era.hs` | `fromReadEra`, `fromAnyCardanoEra` |
| `Write/Tx.hs` | `upgradeToOutputConway` |
| `Network/Implementation.hs` | `_getUTxOByTxIn` |
| `LocalStateQuery/UTxO.hs` | `getUTxOByTxIn` |
| `Convert.hs` | `toWalletScript` |
| `Read/Eras.hs` | `fromAnyCardanoEra` |
| `Outputs.hs` | `txOutFromOutput` |
| `Sealed.hs` | `fromCardanoApiTx` |
| `TxExtended.hs` | `fromCardanoTx` |
| `Shelley.hs` | `toCardanoEra` |
| `Wallet.hs` | `pparamsInRecentEra` |
| `Pools.hs` | `forAllBlocks` |
| `Transaction.hs` | `withRecentEraLedgerTx` |

Plus 3 files needing certificate API migration (`-Wno-deprecations`):
`Transaction.hs`, `Delegation.hs`, `Voting.hs`.

---

## 10. Upstream Breaking Changes Reference

### cardano-ledger

- **Tx newtype per era**: `MkDijkstraTx (AlonzoTx era)` etc. Use `Core.Tx era`.
- **`TimelockScript` → `NativeScript`**
- **`PoolParams`/`PoolMetadata`/`PoolDistr`** → `Cardano.Ledger.State`
- **`txdats'` → `txdats`**
- **`hashScriptIntegrity`**: single `ScriptIntegrity` record, not 3 args
- **`PlutusV4`**: new language version
- **Constraint simplification**: `EraBlockBody` replaces `EraSegWits` +
  `EncCBOR` combos
- **`fromTxSeq` → `txSeqBlockBodyL`** (lens)

### cardano-api

- **`Cardano.Api.Shelley`** deprecated → merged into `Cardano.Api`
- **Certificate API**: `Cardano.Api.Certificate` deprecated →
  `Cardano.Api.Experimental.Certificate`
- **`selectStakeCredentialWitness`** → `Cardano.Api.Compatible.Certificate`

### ouroboros-network / ouroboros-consensus

- **`BlockDijkstra`**, **`QueryIfCurrentDijkstra`**: new constructors
- **`GetStakeDistribution2`**: replaces/augments `GetStakeDistribution`
- **`NodeToClientV_18..V_22`**: new protocol versions
- **`nctMuxTracer` → `nctMuxTracers`** (record of tracers)
- **`MonadThrow`/`MonadMask`**: new constraints on client functions
