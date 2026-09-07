# Plan — 5422

Base: `master` = `6b42c36b586c495a294eb11331984f90a3609470`
Branch: `refactor/5422-delete-era-split` · worktree `/code/cardano-wallet-5422`

## Invariants

| ID | Statement | Fails when | Succeeds when |
|---|---|---|---|
| INV-1 | The census reads 38 and the ratchet `MAX` reads 38, in this PR. | the gate prints any `total` other than 38, or prints `RATCHET SLACK`, or exits non-zero under `DIJKSTRA_STUB_STRICT=1` | the gate's own stdout shows `total = 38 across <n> files   (ratchet MAX=38)` and exit 0 under strict |
| INV-2 | No stub is closed by suppression. | a stub disappears from the census while its failure still exists in some form — a widened catch-all, a renamed message, a loud failure made silent, or a rename that hides it from the counter | every retired stub is retired because the code that threw it is **gone**, and the census delta equals the number of deleted `error` literals |
| INV-3 | The Dijkstra path is exercised. | restoring the deleted `error` leaves the suite green | restoring the deleted `error` makes a named test fail, with that failure captured |
| INV-4 | The population contains the case. | the property's generated/enumerated population does not actually contain the Dijkstra case, or the extent it quantifies over can be empty or truncated without the check noticing | the extent is read out of `allRecentEras` rather than listed, an assertion proves that extent non-empty and containing `RecentEraDijkstra`, and **that assertion is itself shown able to fail** |
| INV-5 | Six sites, six verdicts, each with evidence. Silence is not a pass. | any of sites 22, 23, 24, 27, 28, 29 lacks a verdict, or carries a verdict without both evidence legs | `sweep.md` has six entries, each with leg A and leg B below |

INV-1, INV-2, INV-3 are **blocking**.

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

Leg A alone is the trap the ticket was written around. At sites 22, 23 and 24
the arms produce era-free types — `Maybe Tx.VotingAction`, `Either
ErrCannotJoin ()`, `Either ErrCannotVote ()` — so deleting the era match
**compiles**, the suite stays green, and the change is wrong. A sweep whose only
instrument is "does it still build?" returns *deletable* three times.

Leg B alone cannot see a missing class constraint. `RecentEraConstraints` in
`Cardano.Balance.Tx.Eras` supplies `Core.EraTx`, `Core.EraTxCert`,
`Alonzo.AlonzoEraTxBody`, `Babbage.BabbageEraTxBody` and more, but it does not
supply everything a Conway-only arm may be using. Only the compiler knows.

Site 24 has a third character worth naming: `guardEraIsConway` exists **in order
to reject** non-Conway. Deleting its split does not generalize it, it makes it
return `Right ()` unconditionally — that is INV-2 suppression wearing a deletion's
clothes.

## Strategy for R-1

`mkLedgerTx`'s body uses `mkBasicTxBody`, `inputsTxBodyL`, `outputsTxBodyL`,
`feeTxBodyL`, `vldtTxBodyL`, `withdrawalsTxBodyL`, `certsTxBodyL`,
`mintTxBodyL`, `auxDataHashTxBodyL`, `mkBasicTxAuxData`, `metadataTxAuxDataL`,
`hashTxAuxData`, `mkBasicTx`, `auxDataTxL`. The hypothesis is that every one of
them is available from `RecentEraConstraints`; leg A at site 12 is what settles
it. If it does not compile, that is X-1's finding and the ticket stops.

### Decision D-1 — the `RecentEra era` parameter stays

After the deletion the `era` argument has no use in the body. Two options:

- **chosen:** keep the parameter and bind it `_era`. Minimal diff, no call-site
  churn, and `Cardano.Balance.Tx.Eras`' own convention keeps a `RecentEra era`
  argument for disambiguation.
- rejected: drop the parameter and update every call site. It changes an
  exported signature and touches four call sites in
  `TransactionLedgerSpec.hs` and `Unsigned.hs` for no behavioural gain — scope
  this ticket was explicitly cut to avoid.

### Decision D-2 — deletion orphans things, and `-Werror` will say so

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

## Strategy for R-2 / INV-3 / INV-4

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

RED is not "the test does not exist yet". RED is: with the `error` present, the
named test **executes and fails**, and that failure is captured with its exit
code read directly.

## Strategy for R-4

`scripts/ci/dijkstra-stub-gate.sh` carries `MAX=${DIJKSTRA_STUB_MAX:-39}`. The
default moves to 38. Measured on the merged script with exit codes read
directly and not through a pipe: adding a stub exits **1**; retiring one and
leaving `MAX` alone prints `RATCHET SLACK` and exits **0**. So CI enforces the
census falling and does **not** enforce the ratchet moving. The ticket gate
closes that hole locally by running the same script with
`DIJKSTRA_STUB_STRICT=1`, under which slack is a hard failure.

## Slices

One bisect-safe slice, `S1`. The deletion, its test, the ratchet move and the
sweep record travel together: a commit with the deletion but not the ratchet
leaves the gate carrying slack, and a commit with the ratchet but not the
deletion is red.

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
counts `error` followed by a string literal mentioning Dijkstra, over raw file
bytes, with no comment stripping. A comment containing the word `error` next to
a Dijkstra string would be counted as a new stub and turn CI red. The comments
must not contain the token `error` at all.
