# M6 — Dijkstra HF readiness (#118) — ledger

**Home repo:** `cardano-foundation/cardano-wallet`. **Desk:** `%4`,
session `wallet`, window `cardano-wallet-ms6-dijkstra`.
**Runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra` — legacy path,
deliberately not `/tmp/ms-cardano-wallet-118`.
**Rewritten:** 2026-09-07, on the skills reload at `0fc525f`.

## The outcome and its observable test

The wallet is **ready for the Dijkstra hard fork** — every era code path
implemented and exercised. The outcome is readiness, **not a version number**.

**Observable test, and it is deliberately narrower than the outcome:**

```
dijkstra stub census == 0    (error stubs + pendingWith, multi-line-aware)
```

**That test is necessary and not sufficient, and the epic says so.** The census
population is *code that announces it is unimplemented*. An era arm that was
written rather than stubbed announces nothing — the known instance is the
Dijkstra arm of `setRequiredSigners` (`guardsTxBodyL`), a reviewer's finding.
**A stub fails loudly; that succeeds wrongly**, in the required-signers field,
at the hard fork. Nobody may read zero as done.

## Where the milestone stands

| | |
|---|---|
| `master` | `6b42c36b586c495a294eb11331984f90a3609470` |
| census | **39** — 33 `error`, 6 `pendingWith`, 15 files |
| ratchet | **on `master`** since #5407 merged 2026-09-04, `MAX=39` |
| M6 delivery | **39 of 44**; the shim module's 5 die with #5290 |

### The delivery split, and why it is forced

Five of the original 44 live in `lib/wallet/src/Cardano/Api/Extra.hs`, the shim
#5290 deletes. Implementing them here would gate M6 on M1 **by construction**,
which the operator forbade. So **M6 delivers 39 and owes one verification** —
confirm the five died with their owner, and take back any that owner drops.
The census must still reach 0; a census falling *by agreement between two
desks* is what epic criterion 3 forbids.

## Lanes

**PARKED 2026-09-07 on the operator saying "pause until tomorrow". The
three-deep stack, its three open items and the three failures that cost hours
are in `resume/stack-2026-09-07.md` — read that first.**

| lane | issue | window / pane | stage |
|---|---|---|---|
| ticket owner | **#5422** | `wallet:7` `cardano-wallet-ms6-t5422-delete-era-split`, `%673` | **resumed after pane death**; worktree `/code/cardano-wallet-5422`, branch `refactor/5422-delete-era-split` at `6b42c36b58` |

Archived, accepted: `t5413-review-findings` (evidence lane, #5413 review) and
`t5421-invalid-continuation` (cause investigation, #5421). Both verified
mechanically before acceptance and moved under `.archived/`.

## Priority order, with reasons

1. **The four release blockers** — they decide whether the milestone can ship
   and need no review and no merge. Three now carry a mechanical re-check a
   successor can run instead of re-deriving.
2. **#5422**, the first Dijkstra child — cut from the partition's `delete`
   bucket after the `consume` bucket collapsed to one item.
3. Everything else.

**Inversion on the record:** the plan was to open on the 11-item `consume`
bucket. Measured against `master`, **10 of 11 were unavailable** — 5 excluded
with the shim, 5 retired by #5420. A one-item bucket cannot carry a ticket, so
the `delete` bucket was cut instead. Starting where being wrong costs least is
what made that cheap to learn.

## Release blockers

| # | state | disposition |
|---|---|---|
| **#5408** | open | `.github/RELEASE_TEMPLATE.md` says *"Compatible with `cardano-node@X`"* — **a point is not a floor**. 11.0.1 and earlier fail *silently*. Nothing upstream gates this; trigger is the next release. |
| **#5416** | open | Do not release against node 11.1.0 until a published node release carries `ouroboros-consensus#2251`. **Re-measured 2026-09-02: still not.** Body carries the marker-file query and its `main` control. |
| **#5409** | open | CI must matrix the LedgerDB backend. Measured: one config sets it, **zero `.github` files name it**. |
| **#5417** | open | **A decision, not a measurement** — HTTP 500 `CreatedInvalidTransaction` vs a 4xx for node-rejected external transactions. Public API contract. **With the operator.** No amount of measuring settles it. |

## Parked decisions and what would unblock them

| decision | owner | unblocker |
|---|---|---|
| remove the five suppressions #5399 added | M6 | the M1 front merging. **Residue is zero file edits** — all five are spoken for by #5290, #5412, #5420 and #5413; the unit is a **check, not a change**. |
| convert #5399's `MAX` declaration into a file edit | M6 | row 2 landing |
| #5421 and the genesis-checkpoint data-loss path | **not M6** | escalated to the project owner; node-bump causation was refuted |
| whether ratchet slack should exit non-zero | **not M6** | #5406's owner |

## Escalations in flight

None owed from this desk.

## Operator-facing projection — published

| | |
|---|---|
| wiki page | https://github.com/cardano-foundation/cardano-wallet/wiki/Milestone-118 |
| register | `Milestone-118-Stories.json` in the wiki repo, schema `milestone-stories/v1` |
| published wiki commit | `845637b2d60eabed27153cac8231d229ce3397f3` |
| register digest | `fd483b145dfd5f1f191042eb1717401712d0b93a9d64571e5f53dff056318632` |
| linked from | `Home.md` |
| renderer | `debrief/scripts/render-milestone.mjs`, `--check` clean at publication |

13 stories in 4 groups. The legacy `M118-State.md` page now redirects here —
two pages describing one milestone is how a stale one gets read as current.

**Renderer trap, recorded because it returns success:** invoking the script
through its `~/.claude/skills` symlink makes its `import.meta.url` guard
false, so it **prints nothing, writes nothing and exits 0**. Invoke it at its
real path `/code/llm-settings/shared/skills/debrief/scripts/render-milestone.mjs`.
An exit code of 0 from that script is not evidence that a page was written;
check the file.
