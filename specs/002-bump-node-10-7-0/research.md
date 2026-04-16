# Research: Bump dependencies to cardano-node 10.7.0

## Freeze File Analysis

**Source**: `/tmp/cardano-node-10.7.0/cabal.project.freeze`
**Method**: `nix develop -c cabal freeze` in cloned cardano-node 10.7.0

## Ouroboros Package Consolidation

### Sublibrary Syntax

The absorbed packages become sublibraries referenced with curly brace syntax:

```cabal
ouroboros-consensus:{ouroboros-consensus, cardano, protocol}
ouroboros-network:{api, ouroboros-network, framework, protocols}
```

Must use braces: `ouroboros-consensus:{cardano}`, not `ouroboros-consensus:cardano`.

### Mapping

| Old package | New sublibrary reference |
|---|---|
| ouroboros-consensus-cardano | ouroboros-consensus:{cardano} |
| ouroboros-consensus-diffusion | ouroboros-consensus:{diffusion} |
| ouroboros-consensus-protocol | ouroboros-consensus:{protocol} |
| ouroboros-network-api | ouroboros-network:{api} |
| ouroboros-network-framework | ouroboros-network:{framework} |
| ouroboros-network-protocols | ouroboros-network:{protocols} |

### Confirmed from working downstream consumers

- cardano-ledger-read: `ouroboros-consensus:{ouroboros-consensus, cardano, protocol}`
- cardano-node: `ouroboros-consensus:{ouroboros-consensus, lmdb, lsm, cardano, diffusion, protocol}`

### Import Paths

**No changes.** Module paths remain identical (e.g. `Ouroboros.Consensus.Cardano.Block`). Sublibraries are organizational, not namespacing.

### haskell.nix

Handles `package:{sublib}` syntax natively via Cabal 3.0+ support.

## Missing Transitive Constraints in cabal.project

These packages were transitive deps of the absorbed packages and now need explicit constraints:

- typed-protocols ==1.2.0.0
- network-mux ==0.10.1.0
- cardano-diffusion ==1.0.0.0
- fs-api ==0.4.0.0

## Wallet .cabal Files Affected

Six files reference absorbed packages:

1. lib/primitive/cardano-wallet-primitive.cabal
2. lib/address-derivation-discovery/address-derivation-discovery.cabal
3. lib/network-layer/cardano-wallet-network-layer.cabal
4. lib/wallet/cardano-wallet.cabal
5. lib/unit/cardano-wallet-unit.cabal
6. lib/local-cluster/local-cluster.cabal

## Source Repository Packages

Both bumped and merged:
- cardano-ledger-read: pin `34d0767` (cardano-foundation/cardano-ledger-read#12)
- cardano-balance-tx: pin `ae2087d`
- cardano-coin-selection: no ledger/ouroboros deps, no bump needed

## CHaP Alignment

CHaP rev `887d73ce`, index-state `2026-03-23T18:21:55Z`. Matches cardano-node 10.7.0.

## Local Cluster Fix (merged in #5249)

Node 10.7.0 sets `cardanoProtocolVersion = ProtVer 10 8`, exceeding Conway-era max from shelley genesis `ProtVer 8 0`. Fix: bump genesis ProtVer to 10.0, enable ExperimentalHardForks, add dijkstra genesis.
