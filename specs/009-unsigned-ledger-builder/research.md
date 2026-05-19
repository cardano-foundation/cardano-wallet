# Research: Unsigned Shelley ledger builder migration

**Issue**: [#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285)  
**Parent**: [#5237](https://github.com/cardano-foundation/cardano-wallet/issues/5237)  
**Tracker**: [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243)

## Current-state inventory

`lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` still owns the exported unsigned builder facade:

- `mkUnsignedTransaction`
- `mkUnsignedTx`
- local cardano-api body builder helper `constructUnsignedTx`
- local API certificate conversion helper `certToLedger`
- imports of `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting`

The remaining direct body-construction surface is concentrated in `mkUnsignedTx`: `Cardano.createTransactionBody`, `Cardano.TxBodyContent`, `BuildTxWith`, `TxCertificates`, `TxMintValue`, validity fields, metadata fields, script witness fields, and reference input fields.

`lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` already has the replacement pieces:

- `constructUnsignedTxLedger`
- `buildLedgerTx`
- `buildLedgerTxRaw`
- `ScriptWitnesses`
- `noScriptWitnesses`
- `certificateFromDelegationActionLedger`
- `certificateFromVotingActionLedger`

Those replacements cannot simply be imported by `Shelley.Transaction`, because `Transaction.Ledger` imports `Shelley.Transaction` today for signing, constraints, witness tags, and reward-withdrawal cost helpers. A direct import would create a cycle.

## Decisions

| Decision | Rationale | Alternative Rejected |
|---|---|---|
| Put shared unsigned-body construction behind an acyclic module boundary. | Both `Shelley.Transaction` and `Transaction.Ledger` need the same builder after this ticket. Moving the body/cert builder code to a module that does not import `Shelley.Transaction` breaks the cycle cleanly. | Importing `Transaction.Ledger` from `Shelley.Transaction` would create a compile-time cycle. Copying code would duplicate sensitive transaction construction logic. |
| Keep `mkUnsignedTransaction`'s public return type as `Cardano.TxBody` for this child. | Wallet construction APIs still consume that type. #5289 owns signing removal; #5290 owns final dependency removal. | Changing outward types now would expand the slice into signing/API callers and make the PR too large. |
| Change the internal cert flow to `[TxCert era]`. | This is what unblocks deleting the obsolete cardano-api helper modules. | Keeping `[Cardano.Certificate]` would leave the helper modules alive and fail #5285. |
| Reuse #5288's `ScriptWitnesses` model instead of introducing another script carrier. | #5288 already proved parity for exactly the script-witness surface needed by this ticket. | A second carrier would create mapping drift and duplicate tests. |
| Treat the helper-module deletion as part of the same behavior commit. | The deletion only compiles after the body builder accepts ledger certs; splitting would create a non-buildable intermediate state. | A separate PR or commit would add review overhead without a bisect-safe boundary. |

## Test strategy

RED is a compile/test failure from tests written against the desired ledger-cert `mkUnsignedTx` shape and the wrapper-vs-builder parity expectations. GREEN is the migrated implementation plus existing golden/binary tests still passing.

Required local proof:

```sh
./gate.sh
```

Focused development proof:

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Wallet.Shelley.Transaction"'
```

No integration tests are edited. Running unchanged integration tests is allowed as extra signal only.

## Risks

- **Import cycle**: mitigated by the acyclic shared module.
- **Wrapper loses script witnesses**: mitigated by keeping/updating the #5288 `TransactionLedgerSpec` parity cases so `mkUnsignedTx` exercises the same `ScriptWitnesses`.
- **Byte drift in non-script cases**: mitigated by existing `TransactionSpec` binary/golden tests.
- **Over-broad scope**: signing rewrite, final dependency removal, and Dijkstra are explicitly deferred to #5289/#5290/#5209.
