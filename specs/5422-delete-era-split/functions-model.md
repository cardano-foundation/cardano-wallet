# Functions model — 5422

## Changed signatures

**None.** `mkLedgerTx` keeps its exported signature exactly:

```
mkLedgerTx
    :: forall era
     . IsRecentEra era
    => RecentEra era
    -> Set TxIn -> StrictSeq (TxOut era) -> Coin -> ValidityInterval
    -> Withdrawals -> StrictSeq (TxCert era) -> MultiAsset
    -> Map Word64 Metadatum
    -> Tx era
```

The first argument is retained and becomes an unused binder, so no call site
changes. No constraint is added. **Adding a constraint to make a site compile is
an era arm by another name and is forbidden** — at the deletion site it would
mean the deletion failed, which is a finding that changes the ticket rather than
something absorbed into it.

## New functions

| Name | Arguments | Result | Constraints / effects |
|---|---|---|---|
| the Dijkstra-exercising property | one `RecentEra era` drawn from the quantified extent, plus the transaction components | `Property` | pure; asserts the components handed to `mkLedgerTx` are recoverable from the resulting `Tx era` |
| the extent guard | none | `Expectation` | pure; asserts `allRecentEras` is non-empty and contains `RecentEraDijkstra` |

Names, module placement within the existing spec module, helper decomposition,
and the exact assertion set are the implementer's, inside these constraints.

## Functions inspected but NOT changed

`joinStakePoolDelegationAction`, `guardJoin`, `guardEraIsConway`,
`installScriptWitnesses`, `certificateFromDelegationActionLedger`,
`certificateFromVotingActionLedger`. Leg A's compile experiment is performed
against each and then **reverted**. None of their signatures or bodies is
changed by this slice.
