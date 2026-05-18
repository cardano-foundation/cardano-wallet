# Data Model: Ledger Mint Translation

**Feature**: `007-ledger-minting`

## Entities

### `WalletMintSet`

The wallet's existing internal representation of mints requested for a transaction. Already populated by upstream code paths.

- **Mint half**: `TokenMap` — flat map of `(TokenPolicyId, TokenAssetName) → TokenQuantity` where `TokenQuantity` is `Natural` (unsigned). Each entry means: "increase the on-chain supply of this asset by this much".
- **Burn half**: `TokenMap` — same shape. Each entry means: "decrease the on-chain supply of this asset by this much".

Source-of-truth fields in the existing code:

- `Cardano.Wallet.Transaction.TransactionCtx`
  - `txAssetsToMint :: (TokenMap, Map AssetId ScriptSource)`
  - `txAssetsToBurn :: (TokenMap, Map AssetId ScriptSource)`
- `Cardano.Wallet.Transaction.SelectionOf change`
  - `assetsToMint :: !TokenMap`
  - `assetsToBurn :: !TokenMap`

The `ScriptSource` half of the `TransactionCtx` pair is **not consumed by this feature** — script witnesses are explicitly out of scope.

### `LedgerMintValue`

The ledger's mint field of a transaction body.

- **Type**: `Cardano.Ledger.Mary.Value.MultiAsset`, structurally `Map PolicyID (Map AssetName Integer)`.
- **Signed quantity**: positive integers are mints, negative integers are burns, zero entries are admissible but redundant.

### `MintTranslation`

The new total function from the wallet representation to the ledger representation, introduced by this feature.

```text
toLedgerMintValue
    :: TokenMap   -- mints (unsigned)
    -> TokenMap   -- burns (unsigned)
    -> MultiAsset -- signed
```

Lives in `Cardano.Wallet.Primitive.Ledger.Convert`, next to `toLedgerTokenBundle`.

## Translation rules

For each `(policy, asset)` key that appears in either input:

1. Let `m = mint[(policy, asset)]` (default `0` if absent), interpreted as `Natural`.
2. Let `b = burn[(policy, asset)]` (default `0` if absent), interpreted as `Natural`.
3. Let `q = (toInteger m) - (toInteger b)` — the net signed quantity.
4. If `q == 0`, **drop** the entry (do not include it in the output).
5. Otherwise, emit `(policy, asset) ↦ q` in the output `MultiAsset`, with `policy` and `asset` translated by the existing `toLedgerTokenPolicyId` / `toLedgerAssetName` helpers.

The output's `PolicyID` map only contains policies that have at least one non-zero asset after netting (no empty inner maps).

## Invariants

These are the property statements the test suite must encode (see `contracts/ledger-mint-translation.md` for the formal versions).

- **I1 — Totality**: `toLedgerMintValue` terminates for every input pair of `TokenMap`s.
- **I2 — Identity on empty**: `toLedgerMintValue mempty mempty == mempty`.
- **I3 — Pure-mint correctness**: for every `(p, a, q)` in `mint` with `burn[(p, a)] == 0` and `q > 0`, the output has the corresponding ledger entry with quantity `+q`.
- **I4 — Pure-burn correctness**: for every `(p, a, q)` in `burn` with `mint[(p, a)] == 0` and `q > 0`, the output has the corresponding ledger entry with quantity `-q`.
- **I5 — Mint–burn netting**: when `(p, a)` is in both, the output entry's quantity equals `mint[(p, a)] - burn[(p, a)]`. If that difference is zero, the entry is absent.
- **I6 — No phantom keys**: no policy or asset appears in the output that didn't appear in at least one input.
- **I7 — No empty policy buckets**: every `PolicyID` in the output's outer map has a non-empty inner map.
- **I8 — Round-trip via `fromLedgerMintValue`**: for every input pair where mint and burn have no overlapping keys, applying `fromLedgerMintValue` to the output reconstructs the original `(mint, burn)` pair (modulo empty-bucket trimming). (Reciprocal of the existing read-side function in `Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Mint`.)

## Use in callers

- `mkTransaction` (Ledger.hs:266–349): extract `txAssetsToMint` and `txAssetsToBurn` from its `TransactionCtx`; call `toLedgerMintValue mint burn`; pass the result as the new explicit mint argument to `buildLedgerTx`.
- `constructUnsignedTxLedger` (Ledger.hs:376–408): grow an explicit `(TokenMap, TokenMap)` parameter (mints, burns); call `toLedgerMintValue` internally; pass the result to `buildLedgerTxRaw`.
- Wallet-level call site at `Cardano.Wallet.hs:~2746`: pass `txAssetsToMint`'s first component and `txAssetsToBurn`'s first component (the `TokenMap` halves) into the updated `constructUnsignedTxLedger`.

## What this feature does **not** introduce

- No new wallet-domain type. `TokenMap` and the existing field shapes are reused as-is.
- No script-source threading. The `Map AssetId ScriptSource` halves of `TransactionCtx`'s mint/burn pairs are ignored by this feature.
- No change to `mkLedgerTx`'s signature. Its `MultiAsset` parameter already exists.
