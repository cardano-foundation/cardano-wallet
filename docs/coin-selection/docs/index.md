# cardano-coin-selection

Coin selection algorithms for the Cardano blockchain.

This library provides a complete solution for selecting UTxO entries to fund
transactions on Cardano, including multi-asset support, change generation, fee
estimation, minting/burning, and collateral selection.

## Modules

| Module | Purpose |
|--------|---------|
| `Cardano.CoinSelection` | High-level entry point combining balance and collateral selection |
| `Cardano.CoinSelection.Balance` | Random-Round-Robin algorithm for multi-asset UTxO sets |
| `Cardano.CoinSelection.Collateral` | Dual-strategy collateral selection |
| `Cardano.CoinSelection.UTxOIndex` | Asset-indexed UTxO set with O(1) asset lookup |
| `Cardano.CoinSelection.UTxOSelection` | Selected/leftover state machine |
| `Cardano.CoinSelection.Context` | Type class for selection contexts |
| `Cardano.CoinSelection.Size` | Token bundle size assessment |

## Quick links

- [Getting Started](getting-started.md) -- installation and basic usage
- [Tutorial](tutorial.md) -- step-by-step coin selection walkthrough
- [Selection Strategies](concepts/selection-strategies.md) -- Optimal vs Minimal
- [UTxO Index](data-structures/utxo-index.md) -- the core data structure
- [References](references.md) -- papers and blog posts

## Design principles

The coin selection algorithm is designed around several key principles:

1. **Self-organisation**: the UTxO set should evolve over time to resemble the
   typical distribution of payments made by the wallet owner, increasing the
   likelihood that future selections succeed.

2. **Privacy**: change outputs should be roughly the same size and shape as
   user-specified outputs, making it harder for an observer to distinguish
   change from payments.

3. **Efficiency**: the algorithm must terminate in bounded time and produce
   selections that minimise fees.

4. **Correctness**: all selections satisfy a set of formally verified
   properties -- checked by the test suite via QuickCheck.
