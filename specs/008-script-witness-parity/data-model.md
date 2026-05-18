# Data Model: Script-witness parity in `Transaction.Ledger`

**Branch**: `5288-script-witnesses` | **Spec**: [spec.md](./spec.md) | **Research**: [research.md](./research.md)

## Overview

The parity surface introduces no new domain entities. It plumbs
existing wallet-level types into ledger-typed builder calls. This
document records the carrier shape that crosses the builder boundary
and the body/witness-set fields it lands on.

## Carrier type

`ScriptWitnesses` — a builder argument record at module scope in
`Cardano.Wallet.Shelley.Transaction.Ledger`. All fields default to
"no witness" through `noScriptWitnesses`.

```haskell
data ScriptWitnesses = ScriptWitnesses
    { swNativeInputs    :: Map TxIn (Script KeyHash)
      -- ^ Mirrors TransactionCtx.txNativeScriptInputs.
    , swStakingScript   :: Maybe (Script KeyHash)
      -- ^ Mirrors the resolved TransactionCtx.txStakingCredentialScriptTemplate.
    , swMintingSources  :: Map AssetId ScriptSource
      -- ^ Mirrors the snd component of TransactionCtx.txAssetsToMint and
      --   TransactionCtx.txAssetsToBurn (a flattened union of both).
    , swReferenceScript :: Maybe (Script KeyHash)
      -- ^ Mirrors TransactionCtx.txReferenceScript.
    }

noScriptWitnesses :: ScriptWitnesses
noScriptWitnesses = ScriptWitnesses
    { swNativeInputs    = Map.empty
    , swStakingScript   = Nothing
    , swMintingSources  = Map.empty
    , swReferenceScript = Nothing
    }
```

`ScriptSource = Either (Script KeyHash) ReferenceInput` is reused
from `Cardano.Wallet.Transaction` (no change).

The minting-source field at the wallet layer is two maps (one for
mint, one for burn). The legacy `mkUnsignedTx` accepts a single
union via the `Map AssetId ScriptSource` parameter named
`mintingSource`. We mirror that union at the builder boundary;
callers that have separate mint and burn maps are responsible for
unioning them. This matches the existing `mkUnsignedTransaction`
behavior on the legacy side.

## Body-field map (informative)

For each populated `ScriptWitnesses` field, the builder must touch
exactly the ledger lenses below. Anything else is a regression
against parity.

| Source field | Body lens(es) touched | Witness-set lens touched |
|---|---|---|
| `swNativeInputs` | `inputsTxBodyL` (input set membership; inputs are already added from the selection) | `witsTxL . scriptTxWitsL` ← script with `hashScript` as key |
| `swStakingScript` | `withdrawalsTxBodyL` (script-hash credential) AND `certsTxBodyL` (script-hash stake credential on each cert that has one) | `witsTxL . scriptTxWitsL` ← script with `hashScript` as key, once |
| `swMintingSources` `Left script` | `mintTxBodyL` already covers the value via `toLedgerMintValue`; no body-shape contribution from the script itself | `witsTxL . scriptTxWitsL` ← script with `hashScript` as key |
| `swMintingSources` `Right (ReferenceInput txin)` | `referenceInputsTxBodyL` ← txin; `mintTxBodyL` carries the policy via `toLedgerMintValue` | (none; the script is on-chain at `txin`) |
| `swReferenceScript` | `outputsTxBodyL` first element's reference-script field | (none; the script ships in the output, not the witness set) |

Deduplication invariants:

- Reference inputs collected from `swMintingSources` (`Right` branch)
  go through `Set` membership semantics — multiple policies that
  share a `ReferenceInput` produce one entry in
  `referenceInputsTxBodyL`.
- Scripts in `witsTxL . scriptTxWitsL` are keyed by `hashScript` of
  the ledger `NativeScript`, so multiple references to the same
  script (e.g., the same staking script used by both a withdrawal
  and a cert) produce one entry.

## Test-fixture vocabulary

The parity tests construct hand-made values for the carrier and the
selection. To keep the spec auditable, each enumerated test case
uses a fixed `Script KeyHash` literal whose hash is shown in the
test name. The legacy `mkUnsignedTx` and the new
`buildLedgerTx` consume the same literal, so the comparison's
input space is fully under test control.

## Out-of-scope

This document does not describe:

- Plutus script witnesses (explicitly excluded by the legacy
  builder).
- Signing key resolution (handled separately by `signTransaction`).
- Dijkstra-era native-script differences (no behavior change
  in Conway-only PR).
