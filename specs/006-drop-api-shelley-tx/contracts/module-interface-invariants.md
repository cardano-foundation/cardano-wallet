# Module Interface Invariants

**Branch**: `006-drop-api-shelley-tx`

The cardano-api removal is behaviour-preserving. The exports listed below MUST keep identical names AND signatures across every slice of this feature. These are the public surfaces that other packages (`lib/api`, `lib/unit`, `lib/integration`, `lib/benchmarks`) link against.

If a slice forces a signature change on any item below, the slice is mis-scoped and must be split — or the new ticket scope must be extended explicitly.

## `Cardano.Wallet.Transaction` (interface module — **out of scope**)

The full export list is unchanged. The single dead `Cardano.Api.Extra ()` import is **not** removed in this feature.

Notable invariants:

- `TransactionLayer` type and its constructor.
- `DelegationAction`, `VotingAction` ADTs.
- `TxValidityInterval`, `TransactionCtx`, `Withdrawal`, `WitnessCount*`, `AnyScript`.
- The `Err*` family of error types.

## `Cardano.Wallet.Shelley.Transaction`

The exports remain stable across all three stories. Internal call sites change; the public API does not.

Concretely, the export list is whichever symbols are currently re-exported from `Shelley.Transaction` and consumed by `Cardano.Wallet`, `lib/api`, `lib/unit`, and `lib/integration` — including:

- `newTransactionLayer` (the `TransactionLayer` factory).
- `mkUnsignedTransaction` (entry into body construction).
- `signTransaction`.
- `mkShelleyWitness`, `mkByronWitness` (witness builders, used by the integration suite).
- `txConstraints`, `_txRewardWithdrawalCost`, `_txRewardWithdrawalSize`.

Each must keep its exact type signature.

## `Cardano.Wallet.Shelley.Transaction.Ledger`

The exports remain stable. New exports MAY be added (this is where prerequisite work for Stories 2 and 3 lands) but existing ones are invariant.

Current invariant set (Phase-0 inventory):

```text
mkTransactionLedger
constructUnsignedTxLedger
sealWriteTx
signTransaction              -- re-export
mkShelleyWitnessLedger
mkByronWitnessLedger
TxPayload (..)
txConstraints                -- re-export
TxWitnessTag (..)
txWitnessTagForKey           -- re-export
_txRewardWithdrawalCost      -- re-export
_txRewardWithdrawalSize      -- re-export
certificateFromDelegationActionLedger
certificateFromVotingActionLedger
```

## `Cardano.Wallet.Transaction.Voting`, `Cardano.Wallet.Transaction.Delegation`

**To be deleted when Story 2 lands** (folded into Story 2's commit per `plan.md` §"Vertical Slice Contract"). Until then, both modules and their `exposed-modules` entries stay. No invariant on their public surface for the lifetime of this feature.

When Story 2 deletes them, the `exposed-modules` block in `lib/wallet/cardano-wallet.cabal` MUST drop these two entries in the same commit; otherwise `cabal check` fails.

## `Cardano.Wallet` (consumer of the ledger cert builders)

The exports remain stable. `Cardano/Wallet.hs` already calls the `*Ledger` cert builders (via `constructUnsignedTxLedger`) and is unchanged by this feature.

## Cabal `build-depends`

- `lib/wallet/cardano-wallet.cabal` keeps `cardano-api` in `build-depends` until **no** remaining module under `lib/wallet/src/` imports it.
- The entry is removed in whichever slice closes the last importer. Story 1 does NOT remove it — `Shelley/Transaction.hs` is still a consumer.

## Compatibility audit checklist (per slice)

Before pushing a slice:

1. `git grep -E "^import.*Cardano\.Api" lib/wallet/src/` — confirm no new importers introduced.
2. `git grep -lE "Cardano\.Wallet\.Transaction\.(Voting|Delegation)\b" lib/` — after Story 1, must return empty.
3. `cabal build cardano-wallet` — green.
4. `cabal test cardano-wallet-unit:unit --test-options="--match Sign\|cert"` — green.
5. `git show HEAD -- lib/wallet/cardano-wallet.cabal` — exposed-modules diff matches the slice's scope and nothing else.
