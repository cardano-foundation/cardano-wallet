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

## Who this is for

**A wallet operator building a transaction in the Dijkstra era.**
`mkLedgerTx` returns a transaction instead of throwing. Today the call is a
crash; after this ticket the Dijkstra path is exercised by a test that would
fail if the crash returned.

**The next engineer who opens `Delegation.hs` or `Unsigned.hs` intending to "do
the same thing here".** They find a recorded, evidenced verdict for each of the
six suspected sites, and do not have to re-derive why five of them are
different — or, worse, fail to derive it and ship a change that compiles, passes
every currently-green test, and is wrong.

**#5209's owner.** The census falls by exactly the number of stubs this ticket
retires and the ratchet moves with it in the same PR, so the gate keeps no slack
it could later hide a regression behind.

## Requirements

**The deletion.** The `case era of` in
`Cardano.Wallet.Shelley.Transaction.Build.mkLedgerTx` is deleted and the body
returned directly. The function stays era-polymorphic under its existing
`IsRecentEra era` constraint.

**The path is exercised.** `mkLedgerTx` is exercised **for Dijkstra** by an
executable test that fails when the deleted failure is restored.

**Six verdicts.** Six sites — `joinStakePoolDelegationAction`, `guardJoin` and
`guardEraIsConway` in `Delegation.hs`; `installScriptWitnesses`,
`certificateFromDelegationActionLedger` and `certificateFromVotingActionLedger`
in `Unsigned.hs` — each carry a recorded verdict, *same shape* or *not same
shape*, each with the evidence that decides it.

**The census and the ratchet move together.** The Dijkstra stub census falls to
**38** and the ratchet maximum in `scripts/ci/dijkstra-stub-gate.sh` moves to
**38** in this same PR.

**Nothing is closed by suppression.** No widened catch-all, no renamed message,
no loud failure turned silent, no rename that merely hides a stub from the
census.

## What this ticket must NOT do

**No Dijkstra era arm at `Build.hs`.** The change is a deletion. If an arm turns
out to be required, that is a **finding** that changes the ticket, escalated,
not absorbed.

**No "generalizing the Conway arm"** at any swept site. Five of the six are
semantic or constraint-bearing splits, and deleting three of them **compiles**.

**No implementing a swept site.** The second half of this ticket is a sweep. A
verdict is a recorded decision, not a change. A site found deletable is a
finding, and it belongs to a follow-up ticket.

**No touching** #5421, the genesis-checkpoint path, #5413, #5420, #5423, #5424,
or `Cardano/Api/Extra.hs` — other desks' scope, and `Api/Extra.hs`'s five stubs
die with #5290.

**The ratchet is never raised and never left where it was.** It only ever goes
down. The gate script itself exits 0 on slack — it prints the remedy and passes —
but the workflow runs the census negative control as its own step, and slack
makes that control unfalsifiable, so the job does go red. A ratchet carrying
slack is a gate that cannot fail, and the control is what notices.

## Observable success

1. `scripts/ci/dijkstra-stub-gate.sh .` prints `total = 38` and `(ratchet MAX=38)`
   and does not print `RATCHET SLACK`, and the same script under
   `DIJKSTRA_STUB_STRICT=1` still exits 0.
2. `scripts/ci/dijkstra-census-negative-control.sh .` exits 0 with `delta=1`
   and `gate_exit=1` — the census can still go red.
3. A unit test reaches `mkLedgerTx` at `RecentEraDijkstra` and asserts a
   property of its result; restoring the deleted failure makes it fail.
4. `specs/5422-delete-era-split/sweep.md` carries six verdicts, each with both
   evidence legs required by `plan.md`.
5. The repository builds with `-Werror` and passes `hlint` and the format check
   with no regression against the base.

## Out of scope, but not out of the goal

The five stubs in `lib/wallet/src/Cardano/Api/Extra.hs`. They belong to the shim
module's deletion (#5290). #5209 still owes a *verification* that they died with
their owner — an implementation here would be work that dies with its subject.
