# Plan — 5422

Base: `master` = `6b42c36b586c495a294eb11331984f90a3609470`
Branch: `refactor/5422-delete-era-split` · worktree `/code/cardano-wallet-5422`

## What must hold

| Property | Fails when | Succeeds when |
|---|---|---|
| **The census falls to 38 and the ratchet moves with it, in this PR.** *(blocking)* | the gate prints any `total` other than 38, or prints `RATCHET SLACK`, or exits non-zero under `DIJKSTRA_STUB_STRICT=1` | the gate's own stdout shows `total = 38 across <n> files   (ratchet MAX=38)` and exit 0 under strict |
| **No stub is closed by suppression.** *(blocking)* | a stub disappears from the census while its failure still exists in some form — a widened catch-all, a renamed message, a loud failure made silent, or a rename that hides it from the counter | every retired stub is retired because the code that threw it is **gone**, and the census delta equals the number of deleted failure literals |
| **The Dijkstra path is exercised.** *(blocking)* | restoring the deleted failure leaves the suite green | restoring the deleted failure makes a named test fail, with that failure captured |
| **The generated population actually contains the Dijkstra case.** | the property's generated/enumerated population does not actually contain the Dijkstra case, or the extent it quantifies over can be empty or truncated without the check noticing | the extent is read out of `allRecentEras` rather than listed, an assertion proves that extent non-empty and containing `RecentEraDijkstra`, and **that assertion is itself shown able to fail** |
| **Six sites, six verdicts, each with evidence. Silence is not a pass.** | any of the six sites lacks a verdict, or carries a verdict without both evidence legs | `sweep.md` has six entries, each with leg A and leg B below |

## The sweep method — two legs, and a site is deletable only if both pass

This is the whole intellectual content of the ticket, and it exists because the
obvious instrument gives the wrong answer at three of the six sites.

**Leg A — type.** Delete the era match at the site, compile, record GHC's
verbatim answer (or verbatim success). Then revert. This is an *experiment*, not
a change: the tree returns to its prior state and `git status --porcelain` is
empty afterwards.

**Leg B — semantics.** State in one sentence what the Conway arm *decides*, then
cite the ledger or protocol fact that settles whether Dijkstra decides it the
same way. The citation names a file, a line and the **resolved package version**,
taken from the build this worktree actually produces — never from
`dist-newstyle/cache/plan.json` in another checkout, which is stale.

A site is **same shape (deletable)** only when leg A compiles **and** leg B shows
Dijkstra decides identically. Anything else is **not same shape**, and the
verdict records which leg failed.

### Why both legs are mandatory

Leg A alone is the trap the ticket was written around. At
`joinStakePoolDelegationAction`, `guardJoin` and `guardEraIsConway` the arms
produce era-free types — `Maybe Tx.VotingAction`, `Either ErrCannotJoin ()`,
`Either ErrCannotVote ()` — so deleting the era match **compiles**, the suite
stays green, and the change is wrong. A sweep whose only instrument is "does it
still build?" returns *deletable* three times.

Leg B alone cannot see a missing class constraint. `RecentEraConstraints` in
`Cardano.Balance.Tx.Eras` supplies `Core.EraTx`, `Core.EraTxCert`,
`Alonzo.AlonzoEraTxBody`, `Babbage.BabbageEraTxBody` and more, but it does not
supply everything a Conway-only arm may be using. Only the compiler knows.

`guardEraIsConway` has a third character worth naming: it exists **in order to
reject** non-Conway. Deleting its split does not generalize it, it makes it
return `Right ()` unconditionally — that is suppression wearing a deletion's
clothes.

## Strategy for the deletion

`mkLedgerTx`'s body uses `mkBasicTxBody`, `inputsTxBodyL`, `outputsTxBodyL`,
`feeTxBodyL`, `vldtTxBodyL`, `withdrawalsTxBodyL`, `certsTxBodyL`,
`mintTxBodyL`, `auxDataHashTxBodyL`, `mkBasicTxAuxData`, `metadataTxAuxDataL`,
`hashTxAuxData`, `mkBasicTx`, `auxDataTxL`. The hypothesis is that every one of
them is available from `RecentEraConstraints`; leg A at the deletion site is
what settles it. If it does not compile, the deletion needed an era arm after
all — that is the finding, and the ticket stops.

### The era parameter stays

After the deletion the `era` argument has no use in the body. Two options:

- **chosen:** keep the parameter and bind it `_era`. Minimal diff, no call-site
  churn, and `Cardano.Balance.Tx.Eras`' own convention keeps a `RecentEra era`
  argument for disambiguation.
- rejected: drop the parameter and update every call site. It changes an
  exported signature and touches four call sites in
  `TransactionLedgerSpec.hs` and `Unsigned.hs` for no behavioural gain — scope
  this ticket was explicitly cut to avoid.

### Deletion orphans things, and `-Werror` will say so

Deleting the `case era of` removes the only uses of the `RecentEraConway` and
`RecentEraDijkstra` **constructors** in `Build.hs`, while the `RecentEra`
**type** is still used in the signature. `-Wall -Werror` reports unused import
items, so `RecentEra (..)` and possibly the `GADTs` pragma become live warnings.
Cleaning up exactly what the deletion orphaned is part of the deletion (swap
rule); it is not adjacent cleanup, and it is not licence to touch anything the
deletion did not orphan.

This class — a lint regression created by removing code — is the one that got
past a ticket gate on #5419 and was caught by CI after push. The gate below runs
`hlint` and the format check, and the base is measured as a control so a
pre-existing hint is not attributed to this slice.

## Strategy for exercising the Dijkstra path

One property in `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`,
which already imports `mkLedgerTx`.

- Quantify over the **discovered extent**: `allRecentEras :: Set AnyRecentEra`
  from `Cardano.Balance.Tx.Eras`, not a hand-written two-element list. A future
  era is then covered without editing the test.
- Assert separately that this extent is non-empty **and** contains
  `RecentEraDijkstra`. Without that guard the property can range over an empty
  or truncated set and report success having tested nothing.
- Show that guard can fail. An extent guard that has never been red is a
  hypothesis.
- The property's own assertion must be about the built transaction — the fields
  handed in come back out — so it is not satisfied by the mere absence of an
  exception.

RED is not "the test does not exist yet". RED is: with the failure present, the
named test **executes and fails**, and that failure is captured with its exit
code read directly.

## Strategy for the ratchet

`scripts/ci/dijkstra-stub-gate.sh` carries `MAX=${DIJKSTRA_STUB_MAX:-39}`. The
default moves to 38. Measured on the merged script with exit codes read
directly and not through a pipe: adding a stub exits **1**; retiring one and
leaving the maximum alone prints `RATCHET SLACK` and exits **0**.

That is a fact about the **script**, and it does not settle what **CI** does —
the workflow runs more than the script. Measured on the workflow: the census
negative control is a separate step that seeds one stub and requires the gate to
exit 1. With the census at 38 and the maximum at 39, the seeded total is 39,
which is *at* the ratchet, so the gate exits 0 and the control cannot fire:
`negative-control: FAIL — gate-exit-0-not-1`. **CI does catch a ratchet left
carrying slack**, by way of the control rather than the census. Lowering the
maximum to 38 restores `gate_exit=1` and the control passes.

The ticket gate also closes it locally by running the same script under
`DIJKSTRA_STUB_STRICT=1`, where slack is a hard failure directly.

## Slices

One bisect-safe slice. The deletion, its test, the ratchet move and the sweep
record travel together: a commit with the deletion but not the ratchet leaves
the gate carrying slack, and a commit with the ratchet but not the deletion is
red.

## Live boundaries

None. Everything here is pure transaction construction and a shell census over
the source tree. No node, no database, no network.

## Rejected alternative — recording the verdicts only in source comments

Considered, because the next engineer opens `Delegation.hs`, not `specs/`.
Rejected as the *primary* record: a comment cannot carry the compiler transcript
that is leg A's evidence. Adopted as a *pointer*: one comment per non-deletable
site naming the operative reason in one line and pointing at `sweep.md`, so both
audiences are served.

**Mechanical constraint on those comments, and it is not optional:** the census
counts a failure call followed by a string literal mentioning Dijkstra, over raw
file bytes, with no comment stripping. A comment containing the word that names
that call, next to a Dijkstra string, would be counted as a new stub and turn CI
red. The comments must not contain that token at all.
