# data-model — #5411

**No data model change. This section is deliberately not empty prose: the
absence is the design decision.**

- No type is added, removed, or renamed.
- No field is added, removed, renamed, or retyped.
- No relationship, validation rule, or state invariant changes.
- No serialised form changes, on the wire or on disk.

## Why "no change" is the specification here

The ticket's central fence is **no new type modelling the transaction-body
boundary**. A previous attempt introduced a local `BodyContent` record; it
compiled and went in the right direction and was still wrong, because it
reconstructed `Cardano.TxBodyContent` locally as a parallel abstraction.

The four values — inputs, outputs, collateral, withdrawals — are bound
individually at their point of use. They have no carrier before this change
(they are a record pattern on a `cardano-api` type, immediately destructured)
and must have none after.

**So an empty data model is the acceptance criterion, not an omission.** If the
implementation produces a diff that belongs in this file, the fence has been
crossed — stop and escalate rather than documenting it here.

## The values, for reference only

These are read, not modelled. Named so the differential check (INV-3) has an
explicit subject:

| value | read from |
|---|---|
| transaction inputs | the ledger transaction's inputs |
| transaction outputs | the ledger transaction's outputs |
| collateral inputs | the ledger transaction's collateral |
| withdrawals | the ledger transaction's withdrawals |

Their *representation* after conversion to wallet primitives is unchanged; INV-3
requires it, field by field, against the values the `cardano-api` path produced.
