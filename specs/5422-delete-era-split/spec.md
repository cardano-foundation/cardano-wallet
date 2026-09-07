# 5422 — Delete the spurious era split in `mkLedgerTx`, and sweep six sites

Issue: cardano-foundation/cardano-wallet#5422 · child of #5209 · milestone M6 (#118)
Base: `master` = `6b42c36b586c495a294eb11331984f90a3609470`

## Why this ticket exists

`mkLedgerTx` branches on the era and dies on Dijkstra:

```haskell
case era of
    RecentEraConway -> go
    RecentEraDijkstra -> error "mkLedgerTx: Dijkstra era not yet supported"
```

`go` is written entirely against era-polymorphic ledger lenses under an
`IsRecentEra era` constraint. If nothing in it is Conway-specific, the branch is
not an unimplemented era — it is a split that never had a reason to exist, and
the stub is retired by **deleting code**, not by adding an era arm.

That makes this the highest-value shape in #5209's census, and it is worth one
ticket on its own precisely because the *next* five sites look identical from a
distance and are not.

## User stories

**US-1 — a wallet operator building a transaction in the Dijkstra era.**
`mkLedgerTx` returns a transaction instead of throwing. Today the call is a
crash; after this ticket the Dijkstra path is exercised by a test that would
fail if the crash returned.

**US-2 — the next engineer who opens `Delegation.hs` or `Unsigned.hs` intending
to "do the same thing here".** They find a recorded, evidenced verdict for each
of the six suspected sites, and do not have to re-derive why five of them are
different — or, worse, fail to derive it and ship a change that compiles, passes
every currently-green test, and is wrong.

**US-3 — #5209's owner.** The census falls by exactly the number of stubs this
ticket retires and the ratchet moves with it in the same PR, so the gate keeps
no slack it could later hide a regression behind.

## Requirements

| ID | Requirement |
|---|---|
| R-1 | The `case era of` in `Cardano.Wallet.Shelley.Transaction.Build.mkLedgerTx` is deleted and the body returned directly. The function stays era-polymorphic under its existing `IsRecentEra era` constraint. |
| R-2 | `mkLedgerTx` is exercised **for Dijkstra** by an executable test that fails when the deleted `error` is restored. |
| R-3 | Six sites — 22, 23, 24 (`Delegation.hs`), 27, 28, 29 (`Unsigned.hs`) — each carry a recorded verdict, *same shape* or *not same shape*, each with the evidence that decides it. |
| R-4 | The Dijkstra stub census falls to **38** and the ratchet `MAX` in `scripts/ci/dijkstra-stub-gate.sh` moves to **38** in this same PR. |
| R-5 | No stub is closed by suppression: no widened catch-all, no renamed message, no loud failure turned silent, no rename that merely hides a stub from the census. |

## Rejection behaviour — what this ticket must NOT do

| ID | Forbidden | Why |
|---|---|---|
| X-1 | Write a Dijkstra **era arm** at `Build.hs`. | R-1 is a deletion. If an arm turns out to be required, that is a **finding** that changes the ticket, escalated, not absorbed. |
| X-2 | "Generalize the Conway arm" at any swept site. | Sites 22–24 and 28–29 are semantic or constraint-bearing splits. Deleting three of them **compiles**. |
| X-3 | Implement any swept site. | Item 2 is a sweep. A verdict is a recorded decision, not a change. A site found deletable is a finding, and it belongs to a follow-up ticket. |
| X-4 | Touch #5421, the genesis-checkpoint path, #5413, #5420, #5423, #5424, or `Cardano/Api/Extra.hs`. | Other desks' scope, and `Api/Extra.hs`'s five stubs die with #5290. |
| X-5 | Raise `MAX`, or leave it at 39. | The ratchet only ever goes down, and CI *cannot* catch a missing `MAX` change — it prints the remedy and exits 0. |

## Observable success

1. `scripts/ci/dijkstra-stub-gate.sh .` prints `total = 38` and `(ratchet MAX=38)`
   and does not print `RATCHET SLACK`, and the same script under
   `DIJKSTRA_STUB_STRICT=1` still exits 0.
2. `scripts/ci/dijkstra-census-negative-control.sh .` exits 0 with `delta=1`
   and `gate_exit=1` — the census can still go red.
3. A unit test reaches `mkLedgerTx` at `RecentEraDijkstra` and asserts a
   property of its result; restoring the deleted `error` makes it fail.
4. `specs/5422-delete-era-split/sweep.md` carries six verdicts, each with both
   evidence legs required by `plan.md`.
5. The repository builds with `-Werror` and passes `hlint` and the format check
   with no regression against the base.

## Out of scope, but not out of the goal

The five stubs in `lib/wallet/src/Cardano/Api/Extra.hs`. They belong to the shim
module's deletion (#5290). #5209 still owes a *verification* that they died with
their owner — an implementation here would be work that dies with its subject.
