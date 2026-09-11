# Dijkstra converter stubs — specification

Issue: #5429. Parent epic: #5209 (census items 13–17). Base: #5422.

## The problem

Five code paths abort with `error` the moment a Dijkstra block or a Dijkstra
transaction reaches them:

| site | function |
|---|---|
| `lib/wallet/src/Cardano/Wallet.hs` | `utxoIndexFromWalletUTxO` |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` | `mkLedgerTxOut` |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs` | `toLedgerTxOut` |
| `lib/local-cluster/test/unit/…/Http/ServiceSpec.hs` | `txOutFromOutput` |
| `lib/wallet/src/Cardano/Wallet/Pools.hs` | `forAllBlocks` |

None is blocked upstream. `cardano-ledger-dijkstra` defines
`TxOut DijkstraEra = BabbageTxOut DijkstraEra`, structurally identical to
Conway's, and a Dijkstra block is a Praos `ShelleyBlock` like Babbage's and
Conway's.

## Requirements

**R1 — the Dijkstra arm produces a value.** Each of the five paths returns a
result for Dijkstra. No `error`, no partial function, no exception.

**R2 — output conversion preserves what it converts.** Converting a wallet
output to a Dijkstra ledger output preserves the address bytes and the token
bundle, and converting back returns the original output. On any input where
both are defined, the Dijkstra conversion agrees with the Conway conversion
modulo the era index.

**R3 — the UTxO conversion preserves the map.** Converting a wallet UTxO to a
Dijkstra ledger UTxO preserves the key set and converts each output by R2.

**R4 — the block-producer path agrees across eras.** A Praos block header
yields the same pool identifier whether it is reached through the Babbage,
Conway or Dijkstra arm. The accessor is era-polymorphic, so this is a property
of one definition rather than of three.

**R5 — the census falls by five and the ratchet follows.**
`scripts/ci/dijkstra-stub-gate.sh` run on this tree reports a total five lower
than the same script reports on the merge base, and `DIJKSTRA_STUB_MAX` inside
that script is lowered by the same five in this pull request. The number comes
from the script, never from a hand grep.

**R6 — no era-specific duplicate is added.** The count of era-specialised
output-converter and block-producer definitions in the touched modules does not
rise. Where one definition can serve every era the site already handles, it is
written once.

## Rejection behaviour

These do **not** satisfy R1 and must be rejected:

- widening a catch-all so the Dijkstra case falls into an existing branch;
- rewording or relocating the `error` message;
- replacing the failure with a skipped, pending or otherwise silent test;
- returning a placeholder value the caller cannot distinguish from a real one.

Each converts a loud failure into a silent one, which is worse than the stub.

## Observable success

- Every Dijkstra arm is exercised by a test that runs it. A test showing Conway
  still works has not touched the new arm.
- Where a property carries the proof, its generated population is shown to
  contain the Dijkstra case by an in-suite coverage assertion. A property whose
  generator cannot produce the feature is blind to it at any number of runs.
- The census script's own positive and negative controls still pass, so the
  count that fell is a count the instrument could still have raised.

## Out of scope

Census items 12 and 18–44. `Delegation.hs`. In `Unsigned.hs`, every function
other than `toLedgerTxOut`. Family-wide consolidation of the output-converter
duplicates outside the five sites.
