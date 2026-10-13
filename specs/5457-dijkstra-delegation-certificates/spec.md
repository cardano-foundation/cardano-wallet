# 5457 — Dijkstra: close the five delegation, voting and certificate stubs

## Problem

Five `error` stubs reject the Dijkstra era in the delegation, voting and
certificate path. Each is a runtime crash the moment the wallet is asked to
build a delegation or vote transaction after the hard fork.

| function | file |
|---|---|
| `joinStakePoolDelegationAction` | `lib/wallet/src/Cardano/Wallet/Delegation.hs` |
| `guardJoin` | same |
| `guardEraIsConway` | same |
| `certificateFromDelegationActionLedger` | `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs` |
| `certificateFromVotingActionLedger` | same |

## What Dijkstra changes about certificates

Dijkstra expunges the deposit-free stake registration and de-registration
certificates. `cardano-ledger-dijkstra-0.3.0.0`,
`Cardano/Ledger/Dijkstra/TxCert.hs`:

- `DijkstraTxCertUpgradeError = RegTxCertExpunged | UnRegTxCertExpunged`;
- `upgradeTxCert` returns `Left RegTxCertExpunged` for `RegTxCert {}` and
  `Left UnRegTxCertExpunged` for `UnRegTxCert {}`.

The package declares **no `ShelleyEraTxCert` instance**, and
`class EraTxCert era => ConwayEraTxCert era` does not carry `ShelleyEraTxCert`
as a superclass. For `DijkstraEra` the deposit-free constructors therefore do
not typecheck; unrepresentability is a compile error, not a runtime one.

`instance ConwayEraTxCert DijkstraEra` supplies `mkRegDepositTxCert`,
`mkUnRegDepositTxCert` and `mkDelegTxCert`. DRep and committee certificates
carry over unchanged.

## Why the change is small

Both certificate functions already build only with those three constructors,
and already reject a missing deposit on the Conway path with `error`. Their
Conway bodies are expressible in Dijkstra as written. Nothing here needs a
lossy mapping or a widened tolerance.

## Acceptance

1. The five sites accept Dijkstra and the census falls by five, with the
   ratchet tightened in the same change.
2. Every retired site is executed for Dijkstra by a test. A point mutant
   restoring `error` at any one of the five fails the suite.
3. Where a property carries the proof, its generated population is shown to
   contain the Dijkstra case, with `checkCoverage` so the claim can fail rather
   than warn.
4. A missing deposit remains a loud, era-named failure. No site is closed by
   widening a catch-all, rewording a message, or making a test pending.

## Out of scope

- `installScriptWitnesses` in `Unsigned.hs` and the script-witness parity spec
  that covers it. A previous sweep found it may be deletable rather than needing
  an era arm; its era shape is that slice's decision.
- `Cardano/Api/Extra.hs`, removed with the shim module.
