# Data Model: Unsigned Shelley ledger builder migration

## Builder Flow

```text
mkUnsignedTransaction
  -> ledger certificate helpers
  -> mkUnsignedTx
       -> shared ledger body builder
       -> toCardanoApiTx / body extraction for existing API return type
```

## Type Migration

| Current shape | Migrated shape | Notes |
|---|---|---|
| `[Cardano.Certificate (CardanoApiEra era)]` | `[TxCert era]` | Enables `certificateFrom*ActionLedger` and deletion of old helpers. |
| `Cardano.TxBodyContent` assembly | `Write.Tx era` via ledger builder | Existing return type may extract `Cardano.TxBody` after ledger construction. |
| `Cardano.ScriptWitness` / `SimpleScriptWitness` body fields | `ScriptWitnesses` + ledger lenses | Already introduced and tested by #5288. |
| `certToLedger` local conversion | not needed | Certs are ledger-native before body construction. |
| `dummyInput` + `removeDummyInput` API hack | ledger equivalent or eliminated by `buildLedgerTxRaw` preselection support | Must preserve preselection behavior without `createTransactionBody`. |

## Modules

| Module | Desired role |
|---|---|
| `Cardano.Wallet.Shelley.Transaction` | Public unsigned facade, signing still present until #5289. |
| `Cardano.Wallet.Shelley.Transaction.Ledger` | Ledger transaction API and signing/witness exports; reexports or delegates shared unsigned builder pieces. |
| `Cardano.Wallet.Shelley.Transaction.Build` or new sibling module | Acyclic home for shared unsigned ledger construction, `ScriptWitnesses`, and ledger certificate helpers. |
| `Cardano.Wallet.Transaction.Delegation` | Deleted. |
| `Cardano.Wallet.Transaction.Voting` | Deleted. |

## Invariants

- No module under `lib/integration/**` is changed.
- The shared builder module must not import `Cardano.Wallet.Shelley.Transaction`.
- `mkUnsignedTx` must thread all #5288 script-witness fields to the ledger builder.
- Certificate construction happens as ledger `TxCert era` values before body construction.
- Existing signing functions may still use cardano-api until #5289.
