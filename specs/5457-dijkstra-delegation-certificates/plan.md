# 5457 — plan

## Shape: one era-polymorphic entry point, dispatch inside

Keep each function's existing signature taking `RecentEra era` and put the era
dispatch inside it. Do not split into era-named entry points.

This is not style. `applyEraFun` and `applyEraFunValue` take arguments whose
type is literally `forall era. IsEra era => f era -> g`, so an era-named
function cannot be passed to either. Measured on the same question in
`cardano-ledger-read`: adding Dijkstra to the polymorphic shape cost 3 added
lines and 0 removed; adding it to the era-named shape cost 46 added and 9
removed, plus an `UndecidableInstances` pragma.

`2adfbcd05c` fixes the precedent in this repository for the certificate half:
`genTxCertificate` dispatches on the era, the legacy certificate through Conway,
the deposit-carrying `mkUnRegDepositTxCert` at Dijkstra.

## Why not delete the split entirely

The tempting shape is to drop the `RecentEra` match and let both functions be
era-polymorphic over the constructor class. That needs `ConwayEraTxCert era` in
`RecentEraConstraints`, which supplies `Core.EraTxCert era` and not the Conway
class — and that type lives in `cardano-balance-transaction`, outside this
repository. The per-era arm needs nothing outside: at a concrete `Dijkstra` the
instance resolves.

## Per-site intent

| site | Dijkstra behaviour |
|---|---|
| `certificateFromDelegationActionLedger` | same three constructors as Conway; missing deposit stays fatal |
| `certificateFromVotingActionLedger` | same; the vote half is unchanged by the expunge |
| `joinStakePoolDelegationAction` | the vote coupling is unaffected — the surviving certificate space still carries `mkDelegTxCert` with a DRep delegatee |
| `guardJoin` | duplicate-vote policy unchanged for the same reason |
| `guardEraIsConway` | voting is representable in Dijkstra, so the guard accepts it; the name no longer describes the function and changes with it |

For each of the last three, the Dijkstra arm is justified by what the era can
represent, not by copying the Conway arm because it compiles. If a case turns
out unrepresentable, the outcome is a loud Dijkstra-named error that stays
counted — not a lossy mapping — and it is escalated rather than papered over.

## Proof

Current coverage reaches none of the five for Dijkstra:

- `certificateFromVotingActionLedger`, `joinStakePoolDelegationAction` and
  `guardEraIsConway` have no test references under `lib/`;
- `guardJoin`'s tests in `DelegationSpec.hs` all pass `Write.RecentEraConway`,
  through a helper named `guardJoinConway`;
- the two references to `certificateFromDelegationActionLedger` sit in helpers
  typed `RecentEra Write.Conway`, one of which passes the era as a literal.

So the tests are **written**, not un-suppressed. Flipping an existing
`pendingWith` would produce a Dijkstra-named test that still calls the function
at Conway.

`DelegationSpec.hs` already uses `checkCoverage`; follow that pattern rather
than inventing one.

## Order

1. RED: Dijkstra tests for all five sites, failing against the unfixed tree
   because the production arms still call `error`.
2. GREEN: the five era arms.
3. Tighten the ratchet in the same change.
