# Contract: `toLedgerMintValue`

**Module (planned)**: `Cardano.Wallet.Primitive.Ledger.Convert`

**Sketch**:

```haskell
toLedgerMintValue
    :: TokenMap   -- ^ mints (unsigned)
    -> TokenMap   -- ^ burns (unsigned)
    -> Ledger.MultiAsset
```

The signature is the public contract. The body is private and may evolve as long as the properties below hold.

## Properties

These are stated as `QuickCheck` properties. They live in the wallet-side spec module — preferred location `lib/primitive/test/.../Ledger/ConvertSpec.hs` (or whichever existing convert spec is conventional in `lib/primitive/test`, to be confirmed by the first task).

### P1 — Totality

`prop_total :: TokenMap -> TokenMap -> Property`
`toLedgerMintValue m b` evaluates without exception for every `(m, b)` produced by the `Arbitrary TokenMap` generator.

### P2 — Empty preserved

`prop_empty :: Property`
`toLedgerMintValue mempty mempty === mempty`.

### P3 — Pure mint

`prop_mint_only :: TokenMap -> Property`
For every key `(p, a)` in a `TokenMap` `m` (disjoint from `mempty`), the result of `toLedgerMintValue m mempty` has the corresponding ledger entry with quantity equal to `+ toInteger (m ! (p, a))`. No other entries.

### P4 — Pure burn

`prop_burn_only :: TokenMap -> Property`
Same, but with `toLedgerMintValue mempty b`; entries are negative.

### P5 — Mint–burn netting

`prop_netting :: TokenMap -> TokenMap -> Property`
For every key `(p, a)` appearing in `mint` or `burn` (or both), the quantity in `toLedgerMintValue mint burn` at that key equals `toInteger (mint ! (p, a)) - toInteger (burn ! (p, a))`, **unless** that difference is `0`, in which case the key is absent from the output.

### P6 — No phantom keys

`prop_no_phantom_keys :: TokenMap -> TokenMap -> Property`
The set of `(p, a)` keys in the output is a subset of `keys mint ∪ keys burn`.

### P7 — No empty policy buckets

`prop_no_empty_buckets :: TokenMap -> TokenMap -> Property`
For every `policy` in the outer map of the output, the inner map is non-empty.

### P8 — Round-trip with `fromLedgerMintValue`

`prop_roundtrip_disjoint :: TokenMap -> TokenMap -> Property`
**Precondition**: `mint` and `burn` share no `(policy, asset)` key.
**Statement**: let `(mint', burn') = fromLedgerMintValue (toLedgerMintValue mint burn)`; then `(mint', burn') === (mint, burn)`.

(Without the disjointness precondition, netting drops information that `fromLedgerMintValue` cannot recover. That is by design; document the precondition explicitly in the property.)

## Builder contracts

These belong in `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`.

### B1 — Mint plumbed through `buildLedgerTx`

For every `RecentEra era` supported by `mkLedgerTx` (currently `RecentEraConway`), for every `(mint, burn)` produced by the wallet generators, with all other inputs fixed:

```text
view mintTxBodyL (txBody (buildLedgerTx era ttl net wdrl fee md certs outs sel mintVal))
  ===
toLedgerMintValue mint burn
```

where `mintVal = toLedgerMintValue mint burn`.

### B2 — Mint plumbed through `buildLedgerTxRaw`

Same statement, for `buildLedgerTxRaw`, on both branches of its `Either PreSelection (SelectionOf TxOut)` argument.

### B3 — Empty-path stability

When `(mint, burn) == (mempty, mempty)`, the produced transaction body is structurally equal (under `(==)` on `Tx era`) to the body the current `mempty -- TODO`-using builder produces: same mint field (empty), every other field unchanged. Byte-identity of any wire serialisation is not part of this contract — that would depend on serialiser determinism this feature does not own. The regression guarantee is: existing non-minting tests continue to pass without modification.

## Out of contract

- Quantity overflow: `Natural` is unbounded, `Integer` is unbounded; no overflow path exists. The contract has no clamping.
- Script witnesses: the contract does not constrain script witnesses. A body produced by these builders with a non-empty mint and no witnesses is not on-chain-valid; this is acceptable because no production caller exercises it until a later feature lands.
- Era beyond `RecentEraConway`: `mkLedgerTx` errors on `RecentEraDijkstra`; the contract for B1/B2 inherits that and is therefore restricted to `RecentEraConway` until the era support gap is closed independently.
