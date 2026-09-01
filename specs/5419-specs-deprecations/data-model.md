# data-model.md — #5419

Ceiling: 35 lines.

## New or changed data

**None.** No field is added, removed, retyped, or revalidated; no relationship
changes; no persisted or wire representation changes. Stated explicitly because
an empty model is a claim to be checked, not a section left blank.

## The boundary this ticket reads across

Unchanged in shape, only in which side is read from:

| Concept | Deprecated source being retired | Ledger-native source |
|---|---|---|
| inputs | `TxBodyContent` record field | ledger transaction body |
| outputs | `TxBodyContent` record field | ledger transaction body |
| collateral | `TxBodyContent` record field | ledger transaction body |
| withdrawals | `TxBodyContent` record field | ledger transaction body |

These four are the surface #5413 established the direction for. The deprecated
side is dominated by **record fields**, which is why causes come from GHC
diagnostics and not from grepping type names.

## State invariants

The only state invariant this ticket may affect is the one it must **preserve**:
for every era these suites actually exercise, the four fields read through the
ledger accessors carry the same wallet-level meaning as the deprecated
projections they replace. Enforcement is INV-3 NON-VACUITY, because the failure
mode is not a wrong value — it is an assertion that can no longer tell a wrong
value from a right one.

## Era reachability — a fact that governs how the above is read

Both spec files declare `arbitrary = return (AnyRecentEra RecentEraConway)`, in
the base as well as the candidate. The Dijkstra arm of these suites is
unreachable and always was. So "every era these suites exercise" means Conway.
Dijkstra arms in these files are **static stub surface**, counted by INV-4, not
executed coverage.
