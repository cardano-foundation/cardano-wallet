# References

## Blog posts

- **Self Organisation in Coin Selection**
  ([IOHK blog](https://iohk.io/blog/self-organisation-in-coin-selection/))
  -- describes how random coin selection leads to a self-organising UTxO
  distribution that mirrors the user's payment patterns.

- **The Challenges of Optimizing Unspent Output Selection**
  ([Jameson Lopp](https://medium.com/@lopp/the-challenges-of-optimizing-unspent-output-selection-a3e5d05d13ef))
  -- a survey of coin selection approaches for Bitcoin, many of which apply
  to Cardano's UTxO model.

## Cardano documentation

- **Cardano Wallet User Guide**
  ([GitHub Pages](https://cardano-foundation.github.io/cardano-wallet/))
  -- the full documentation for the cardano-wallet project, which uses
  this coin selection library.

- **Cardano Ledger Specification**
  ([GitHub](https://github.com/intersectmbo/cardano-ledger))
  -- the formal specification of the Cardano ledger, including transaction
  validation rules that constrain coin selection.

## Related libraries

- **`cardano-numeric`** -- provides `partitionNatural`, `equipartitionNatural`,
  and `padCoalesce` used by the coin selection algorithm.

- **`cardano-wallet-primitive`** -- provides `TokenMap`, `TokenBundle`, `Coin`,
  and related types used throughout the library.

## Haddock documentation

The most detailed documentation is in the Haddock comments within the source
code itself:

| Module | Key documentation |
|--------|-------------------|
| `Cardano.CoinSelection.Balance` | Round-robin algorithm, change generation, minting/burning |
| `Cardano.CoinSelection.Collateral` | Dual-strategy collateral selection |
| `Cardano.CoinSelection.UTxOIndex.Internal` | Index invariants, selection filters |
| `Cardano.CoinSelection.UTxOSelection` | State machine semantics |
| `Cardano.Numeric.Util` | padCoalesce, partitionNatural proofs |
| `Cardano.Wallet.Primitive.Types.TokenMap` | Partial ordering rationale |
