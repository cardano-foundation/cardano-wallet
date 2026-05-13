# Phase 0 Research: Ledger Body Builder Minting Support

**Feature**: `007-ledger-minting`
**Date**: 2026-05-13
**Source**: Explore sub-agent run on `/code/cardano-wallet-5243` at base commit `0821bddb2c` (`origin/master`).

## File index

| Purpose | File | Key lines |
|---|---|---|
| Ledger body builder | `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` | 411–423 (`buildLedgerTx`), 465–477 (`buildLedgerTxRaw`), 433 + 496 (`mempty -- TODO`) |
| Transaction assembler | `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Build.hs` | 88–108 (`mkLedgerTx`), 104 (`MultiAsset` param), 131 (`mintTxBodyL .~ mint`) |
| Transaction context types | `lib/wallet/src/Cardano/Wallet/Transaction.hs` | 218–244 (`TransactionCtx`), 252–271 (`SelectionOf`) |
| `TokenMap` | `lib/primitive/lib/Cardano/Wallet/Primitive/Types/TokenMap.hs` | 19–79 |
| Existing wallet→ledger converters | `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Convert.hs` | 183–192 (class), 216–251 (`toLedgerTokenBundle`) |
| Existing ledger→wallet mint read path | `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Mint.hs` | 226–246 (`fromLedgerMintValue`) |
| Tests | `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` | spec entry around line 408 |
| Indirect caller of `constructUnsignedTxLedger` | `lib/wallet/src/Cardano/Wallet.hs` | ~line 2746 |

## What's there today

### `buildLedgerTx` (Ledger.hs:411–423)

```haskell
buildLedgerTx
    :: forall era . IsRecentEra era
    => RecentEra era
    -> (Maybe SlotNo, SlotNo)
    -> Network
    -> Withdrawal
    -> W.Coin
    -> Maybe TxMetadata
    -> [TxCert era]
    -> [TxOut]
    -> SelectionOf TxOut
    -> Write.Tx era
```

Line 433 inside its `where`-bound call to `mkLedgerTx` passes `mempty -- TODO: minting support` as the mint argument.

### `buildLedgerTxRaw` (Ledger.hs:465–477)

Same shape, but accepts `Either PreSelection (SelectionOf TxOut)` as its final argument. Line 496 has the same `mempty -- TODO: minting support`.

### `mkLedgerTx` (Build.hs:88–108)

Already accepts `MultiAsset` (`Cardano.Ledger.Mary.Value`) as its 8th parameter (line 104). Applies it via `mintTxBodyL .~ mint` (line 131). No change needed here.

### Wallet's mint representation

- `TokenMap` from `Cardano.Wallet.Primitive.Types.TokenMap` — flat map `(TokenPolicyId, TokenAssetName) → TokenQuantity` where `TokenQuantity = Natural` (unsigned).
- `TransactionCtx.txAssetsToMint :: (TokenMap, Map AssetId ScriptSource)` and `txAssetsToBurn` analogue.
- `SelectionOf.assetsToMint :: !TokenMap` and `assetsToBurn :: !TokenMap`.
- Mints and burns are **two distinct unsigned maps**, not one signed map.

### Ledger's mint representation

- `Cardano.Ledger.Mary.Value.MultiAsset` — `Map PolicyID (Map AssetName Integer)`. Quantity is **signed**: positive = mint, negative = burn.
- Lens at the body level: `mintTxBodyL :: Lens' (TxBody era) MultiAsset`.

### Existing convert layer

- `toLedgerTokenBundle :: TokenBundle -> MaryValue` (Convert.hs:220) — operates on `TokenBundle` (ada + tokens), not bare `TokenMap`, so not directly reusable for mints.
- Sub-helpers it relies on (`toLedgerTokenPolicyId`, `toLedgerAssetName`, `toLedgerTokenQuantity`) are reusable for the mint translation.
- Read-side counterpart `fromLedgerMintValue` (Mint.hs:226) splits a `MultiAsset` into `(mint, burn) :: (TokenMap, TokenMap)`; the new write-side function is its inverse.

## Risks / surprises

1. **`TokenMap → MultiAsset` does not yet exist.** Must be added. Natural home: `Cardano.Wallet.Primitive.Ledger.Convert`, next to `toLedgerTokenBundle`.

2. **Signed / unsigned asymmetry.** `TokenMap` quantities are `Natural`; `MultiAsset` quantities are `Integer`. The new translation must take **two** `TokenMap`s (mint and burn) and produce a single signed `MultiAsset`.

3. **Mint–burn overlap is not impossible.** The same `(policy, asset)` can appear in both `assetsToMint` and `assetsToBurn`. Decision (documented in `data-model.md`): net them (`mint − burn`); drop zero entries.

4. **`PreSelection` does not carry mint fields.** Therefore `buildLedgerTxRaw` cannot recover mints by inspecting its `Either PreSelection (SelectionOf TxOut)` argument. The plan adds an explicit mint parameter — the only design that is symmetric across both builders.

5. **Only `RecentEraConway` is implemented in `mkLedgerTx`.** `Dijkstra` continues to error. The mint feature inherits this; it does not unblock Dijkstra.

6. **No existing minting test on this path.** All current tests pass `mempty`. There is no positive coverage that would regress, but also no fixture to extend — the new property and unit tests come from this feature.

## Decisions (Phase 0 → Phase 1)

- **D1: Pass `MultiAsset` (pre-translated) as an explicit parameter to both builders.** Rationale: uniform across both surfaces, independent of whether the selection happens to carry mint fields.
- **D2: Add `toLedgerMintValue :: TokenMap -> TokenMap -> MultiAsset` in `Convert.hs`.** Rationale: same module as `toLedgerTokenBundle`; consistent with read-side `fromLedgerMintValue`.
- **D3: Net overlapping `(policy, asset)` entries and drop zeros.** Rationale: deterministic body output; matches what callers already arrange to be non-overlapping in practice; documented so reviewers don't have to derive it.
- **D4: Do not touch `mkLedgerTx`.** Its `MultiAsset` parameter is already there.
- **D5: Do not change the `cardano-api` path.** This feature is strictly the ledger path's plumbing; the migration is the next feature.
- **D6: No script-witness work in this slice.** Explicit non-goal; revisit when minting actually becomes reachable through a non-experimental code path.

## Alternatives considered

- **A1: Extract mint from `SelectionOf` inside `buildLedgerTx`** instead of taking a parameter. Rejected — asymmetric with `buildLedgerTxRaw` (whose `PreSelection` branch has no mint), and silently hides the input from the caller-facing signature.
- **A2: Make the translation type `TokenBundle → MultiAsset` and ignore the ada coin.** Rejected — `TokenBundle` is the wrong domain (mint has no ada coin), and silently dropping the coin is an information-loss footgun if the input ever has one.
- **A3: Add a `TokenMapWithSign` type and feed that through.** Rejected — gratuitous new type; the existing two-`TokenMap` representation is already universally used in `TransactionCtx` and `SelectionOf`, and netting at the boundary is one line.
- **A4: Defer the translation and accept `MultiAsset` directly in callers.** Rejected — pushes the wallet→ledger conversion to many call sites instead of one; against the layering principle of the `Convert` module.
