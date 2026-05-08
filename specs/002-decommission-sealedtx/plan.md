# Decommission `SealedTx.hs` cardano-api surface

Follow-up to PR #5236. Removes the remaining three `Cardano.Api*`
imports from `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs`
by eliminating the bridge functions they support.

## Current cardano-api surface in `SealedTx.hs`

| Symbol | Uses cardano-api for | External callers |
|---|---|---|
| `cardanoTxIdeallyNoLaterThan`, `cardanoTxInExactEra` | `deserialiseFromCBOR` of `InAnyCardanoEra Cardano.Tx` | `Shelley/Transaction.hs`, `Ledger/Shelley.hs`, `Server.hs`, integration tests |
| `sealedTxFromCardano`, `sealedTxFromCardano'` | accept `Cardano.Tx` / `InAnyCardanoEra Cardano.Tx` | `Wallet.hs`, `Transaction.Ledger.hs`, `Shelley.Transaction.hs`, API server, tests |
| `sealedTxFromCardanoBody` | constructor from `Cardano.TxBody` | old tx path in `Shelley.Transaction.hs` |
| `getSealedTxBody`, `getSealedTxWitnesses` | re-extract `Cardano.TxBody` / `Cardano.KeyWitness` | signing in `Shelley.Transaction.hs`, API redeemer accounting, tests |
| `cardanoApiTxToReadTx` | `ShelleyTx`-dispatched projection to `EraValue Read.Tx` | only called from `sealedTxFromCardano` / `sealedTxFromCardano'` |

## Plan

Each step is a standalone bisect-safe commit.

1. **Caller-side reader swap.** Replace `cardanoTxIdeallyNoLaterThan` and
   `cardanoTxInExactEra` call sites with
   `Cardano.Read.Ledger.Tx.CBOR.deserializeTx`. The helpers remain in
   `SealedTx.hs` but lose external callers.

2. **Delete `sealedTxFromCardano` / `sealedTxFromCardano'` /
   `sealedTxFromCardanoBody`.** Every producer of a `Cardano.Tx` /
   `Cardano.TxBody` today already has the CBOR bytes in hand. Replace
   with `sealedTxFromBytes`. This also kills the only caller of
   `cardanoApiTxToReadTx`.

3. **Delete `getSealedTxBody` / `getSealedTxWitnesses`.** Gated on
   deletion of the old `Shelley.Transaction.hs` signing path. API /
   tests read the same information via `EraValue Read.Tx` accessors.

4. **Delete `cardanoApiTxToReadTx` + `cardanoTxFromBytes` private
   helpers.** They have no callers after steps 1–3.

5. **Remove `Cardano.Api` + `Cardano.Api.Tx` imports from `SealedTx.hs`.**
   The module's only remaining external types are `Cardano.Ledger.Binary`,
   `Cardano.Read.Ledger.Tx.CBOR`, and `Cardano.Wallet.Read`. Note:
   `cardano-api` stays in `lib/primitive/cardano-wallet-primitive.cabal`
   until the other 7 primitive-layer files also shed their imports —
   out of scope for this PR.

## Not in this PR

- Deleting `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` and
  the other legacy modules (`Delegation.hs`, `Voting.hs`). That's the
  parallel transaction-layer swap — tracked in #5243.
- Removing `cardano-api` from the `cardano-wallet-primitive` cabal
  dependency list. Blocked on steps above plus the other 7 primitive
  modules.
- Removing `lib/cardano-api-extra/`. Phase D of #5237.

## Acceptance criteria

- [ ] `grep -n "^import.*Cardano\.Api" lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs` returns 0 results.
- [ ] Every commit builds `nix build .#cardano-wallet .#unit-cardano-wallet-unit`.
- [ ] Every commit passes `fourmolu --mode check` and `hlint lib`.
- [ ] Conway integration tests green on final tip.
