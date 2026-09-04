# M6 (#118) — disposition at go-dark

**Desk:** milestone owner `%4`, runtime root
`/tmp/projects/cardano-wallet/ms6-dijkstra`.
**Written:** 2026-09-01, on `RULING-2026-09-01-wallet-5420-then-dark.md`
(sha256 `090dfdbb…b4b5`, recomputed at this desk and matching).

**This desk is NOT yet dark.** The terminal event is the *condition*, not the
ruling: **#5420 green (every check `SUCCESS`) and open (out of draft)**, relayed
by `%107`. `PAUSED` gets written then, not before.

Every number below was measured, not remembered. The command is given so a
resurrector can re-run it rather than trust it.

## Reference point

I hold **no worktree**. There is no checkout under my runtime root — every git
object I track lives in `/code/cardano-wallet` (read-only bootstrap ground) or in
a lane's tree. **The SHA that identifies my state is `origin/master`.**

| | |
|---|---|
| `origin/master` | **`4a2227a725582dc611b10ec6f7338274a990938f`** — #5399's merge, 2026-09-01T15:08:25+01:00 |
| unchanged since | nothing has merged after it |
| `#5413` | **OPEN**, `REVIEW_REQUIRED`, head `f1dd799c`, based on `4a2227a725` — the only non-draft PR in the repo |
| `#5420` | **DRAFT**, OPEN, head `69480c325c9f` — M1's; not mine to touch |

## Owed unit 1 — remove the five suppressions #5399 added

**Owner:** M6 (`%4`). **Trigger:** the M1 front merging.
**Status: UNFIRED — correctly.** The trigger has not occurred.

The five are no longer a description; they are **enumerated**, derived by
differencing the two revisions rather than by reading the PR body:

```
git -C /code/cardano-wallet grep -E -- \
  '^\{-# +OPTIONS_GHC .*(-Wno-deprecations|-fno-warn-deprecations)' <rev> -- '*.hs'
```

| rev | meaning | pragmas |
|---|---|---|
| `99c3a7e88b` | master before #5399 (#5402's merge) | **7** |
| `4a2227a725` | master after #5399 | **12** |

Added by #5399: **5**. Removed: **0**. `7 + 5 = 12` closes.

1. `lib/cardano-api-extra/lib/Cardano/Api/Gen.hs`
2. `lib/integration/scenarios/Test/Integration/Scenario/API/Shelley/TransactionsNew.hs`
3. `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`
4. `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs`
5. `lib/wallet/src/Cardano/Wallet.hs`

**All five are already spoken for by other desks** and must not be
retired here without checking first — retiring one twice is a merge conflict, and
retiring one whose owner is mid-flight is a crossing:

| file | who retires it | on what |
|---|---|---|
| `Gen.hs` | **#5290** | it lives in the shim package #5290 deletes; ruled, `retires-with` |
| `TransactionsNew.hs` | **#5412** | integration slice |
| `TransactionLedgerSpec.hs` | **#5419** (M1) | now #5420 |
| `TransactionSpec.hs` | **#5419** (M1) | now #5420 |
| `Cardano/Wallet.hs` | **#5413** | verified: #5413 clears exactly this one, 12 -> 11 |

**All five are spoken for. The residue of this unit is therefore ZERO FILE EDITS
and one verification obligation:** confirm each of the five actually died with
its owner, and take back any that its owner drops. A resurrector that reads
"remove five suppressions" and removes five will collide with four desks.

This was checked, not assumed. The draft of this page asserted `Cardano/Wallet.hs`
was unclaimed; differencing #5413's head against its base showed it is exactly
the file #5413 retires, which is also the one of M1's three that `RULING 3`
recorded #5413 as clearing. **The unit I owe is a check, not a change.**

### Controls, so the counter is known to work

- negative: the same expression with `-Wno-zzzz` returns **0**
- positive: it finds `lib/wallet/src/Cardano/Wallet.hs` at `4a2227a725`
- **prose trap, hit and corrected while writing this page:** a looser expression
  matched `ERA-CHANGES.md` and `TODO.md`, which *quote* the pragma in prose, and
  inflated 12 → 14. Counts come from `^{-# OPTIONS_GHC` at line start, `*.hs`
  only. This is the census contract's own recorded failure and it recurs.

## Owed unit 2 — convert #5399's `MAX` declaration into a file edit

**Owner:** M6 (`%4`). **Trigger:** row 2 landing.
**Status: UNFIRED — correctly.**

**Named object:** #5399's PR body declares a `MAX` for `-Wno-deprecations` /
`-fno-warn-deprecations` occurrences. A declaration in a PR body is not
enforceable — nothing reads it. The unit converts it into a checked-in
ratchet file. The counting rule is settled and must survive the conversion:
**both spellings**, counted from the tree, never from prose.

Related instrument, **not mine**: #5407 (draft) enforces the *Dijkstra stub*
census ratchet in CI. Different counter, same shape.

## Release blockers — four, all now visible on a board

| # | ms | disposition |
|---|---|---|
| **#5408** | 118 | wallet requires node ≥ 11.1.0, explicit compatibility cut-off |
| **#5416** | 118 | do **not** release against 11.1.0 — `GetGenesisConfig` 15/16 field break. Fixed upstream by `consensus#2251`, merged 2026-08-31T16:38:25Z; **no published node release carries it** and 11.1.0 predates it |
| **#5409** | 118 | CI must test both LedgerDB backends — `V1LMDB` removed in 11.1.0, mainnet now `V2InMemory` and untested |
| **#5417** | 118 | node-rejected external transactions return 500 `CreatedInvalidTransaction` instead of 4xx |

**#5417 was `milestone: NONE` until this page was written and is now stamped into
#118.** That is the **fourth** unmilestoned-ticket instance after #5401, #5416
and #5418 — and the rule was already on the record: *an unmilestoned release gate
is invisible to every board, which is how the last one reached me as a
discovery.* It was one of my own four blockers and I had been listing it for
days without noticing it was invisible.

**#5416 is the one that decides whether M6 can close at all.** Its resolution is
not in this repository — it is a node release carrying `consensus#2251`. There is
no action here that advances it and no amount of wallet work substitutes.

## Not mine — recorded so the state is not lost

**Census lane, four unpushed commits.** `fix/5209-dijkstra-census-gate @
9049dd9fb7` exists **only on `/tmp`**; the remote branch is at `53f95d5e`, the
planning commit alone. `%106`, `%109` and `%110` are **gone**. The
observer-free durability risk named in `NOTE-019` has now lost even its parked
observers, and the head has advanced past what is on the remote.

**The decision is the operator's; `%107` has raised it. This desk does not touch
that tree.** Written here only so a resurrector finds it instead of rediscovering
it.

**#5419 / #5420 belong to M1.** Lanes `%310` and `%304` were released
2026-09-01 and nothing returns them. **Do not contact them.** Their release-state
was handed over in `outbox/CONFIRM-003-lanes-released-to-m1.md`, including the
part most easily lost: **audit owed, never run, nothing accepted.**

**#5418** → M7 (#119). **#5290, #5412, #5411, #5413** → M1 (#113).

## Live rules this desk is carrying, which outlast it

- **A decision justified by a condition must be re-measured when that condition
  can change. Asserting the premise is not observing it.** Bound project-wide
  today. Instances: `reviewDecision` read as a signal when it is a stored
  verdict; the #5413 fold justified by review scarcity and never re-measured
  after #5399 merged; and — while writing this page — a remote-tracking ref read
  as an observation of the remote when it was a **cached value four days stale**.
- **A parked desk's open decisions do not stay open.** Park with decisions
  closed, or deferred with a named owner and trigger.
- **A scoped measurement answers the scoped question.** Reusing its output for
  the unscoped question is a different claim with no evidence.
- **A control record with two writers is a weaker instrument than it looks.**
  One writer per journal.
- **`44` is a ceiling, not a quota**, and a rename that hides a stub from the
  census is not a repair.

## At the signal

Write `PAUSED` to `STATUS.md` with `origin/master = 4a2227a725582dc611b10ec6f7338274a990938f`
and stop. Nothing else is in flight at this desk.
