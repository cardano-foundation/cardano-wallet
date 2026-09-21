# M6 — Dijkstra HF readiness (#118) — ledger

**Home repo:** `cardano-foundation/cardano-wallet`. **Desk:** `%4`,
session `wallet`, window `cardano-wallet-ms6-dijkstra`.
**Runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra` — legacy path,
deliberately not `/tmp/ms-cardano-wallet-118`.
**Rewritten:** 2026-09-21T16:30Z, on the desk's adoption by the operator and
the skills reload at `c1665a4`/`9c01138`.

## The outcome and its observable test

The wallet is **ready for the Dijkstra hard fork** — every era code path
implemented and exercised. The outcome is readiness, **not a version number**.

**Observable test, and it is deliberately narrower than the outcome:**

```
dijkstra stub census == 0    (error stubs + pendingWith, multi-line-aware)
```

**That test is necessary and not sufficient, and the epic says so.** The census
population is *code that announces it is unimplemented*. An era arm that was
written rather than stubbed announces nothing. Two instances are now on the
record, and the second is worse than the first:

- **`guardsTxBodyL`** — the Dijkstra arm of `setRequiredSigners`, a reviewer's
  finding. **A stub fails loudly; that succeeds wrongly**, in the
  required-signers field, at the hard fork.
- **`readTxFromBytes` (#5410)** — era detection is by **trial decode,
  newest-era-first, taking the first decoder that does not throw**. That is
  correct only while a newer era's decoder *rejects* an older era's encoding —
  an invariant nothing declares, nothing tests, entirely at the ledger's
  discretion, and **the opposite of the direction upstream is moving**
  (`cardano-ledger-alonzo` 1.16.0.0: *"Make AlonzoTx decoder that used for
  Mempool backwards compatible with prior-eras"*). The issue says it has
  already stopped holding.

**Nobody may read a zero census as a delivered outcome.** The census cannot
count either of these.

## Where the milestone stands — measured 2026-09-21, not remembered

| | |
|---|---|
| `origin/master` | `92b57304dfb556d80edf30d75e3f19218e66e891` (2026-09-16, #5438 release-candidate merge-back) |
| census | **32** — 26 `error`, 6 `pendingWith`, **10 files** |
| ratchet | `MAX=32` on `master`, gate **GREEN**, `controls: positive=PASS negative=PASS` |
| trend | 44/15 at milestone open → 39/15 on 2026-09-08 → **32/10 today** |

Re-run the measurement rather than trusting this table:

```sh
git -C /code/cardano-wallet archive origin/master lib | tar -x -C <tmp>
git -C /code/cardano-wallet show origin/master:scripts/ci/dijkstra-stub-gate.sh > <tmp>/gate.sh
bash <tmp>/gate.sh <tmp>
```

Per-file at `92b57304dfb5`:

```
  2 error  0 pendingWith  lib/api/src/Cardano/Wallet/Api/Types/Era.hs
  2 error  0 pendingWith  lib/cardano-wallet-read/haskell/Cardano/Wallet/Read/Tx/Gen/Dijkstra.hs
  1 error  0 pendingWith  lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Convert.hs
  2 error  4 pendingWith  lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs
  6 error  2 pendingWith  lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs
  5 error  0 pendingWith  lib/wallet/src/Cardano/Api/Extra.hs          <- M1's shim, not M6's
  3 error  0 pendingWith  lib/wallet/src/Cardano/Wallet/Delegation.hs
  1 error  0 pendingWith  lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs
  1 error  0 pendingWith  lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs
  3 error  0 pendingWith  lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs
```

### The delivery split, and why it is forced

Five of the 32 live in `lib/wallet/src/Cardano/Api/Extra.hs`, the shim M1's
#5290 deletes. Implementing them here would gate M6 on M1 **by construction**,
which the operator forbade. So **M6 delivers 27 and owes one verification** —
confirm the five died with their owner, and take back any that owner drops.
The census must still reach 0; a census falling *by agreement between two
desks* is what epic criterion 3 forbids.

## Lanes — both live as of 2026-09-21T16:30Z

| lane | issue | window / pane | stage |
|---|---|---|---|
| `t-delegation-certs` | unfiled | `wallet:3` `cardano-wallet-ms6-delegation-certs`, **`%936`** | **RELEASED 16:24Z**, roster agreed. Parked 09-11→09-21 on Q-001/Q-003. Next: file the issue, cut worktree from `92b57304dfb5`, author the frozen gate, dispatch its one commit owner. |
| `t5410-era-detection` | **#5410** | `wallet:4` `cardano-wallet-ms6-t5410-era-detection`, **`%1880`** | PLANNING. Branch `test/5410-era-detection-invariant` at `92b57304dfb5`, draft PR pending. Baseline `-Werror` build running ~40 min — **verified healthy daemon-side** (ghc-9.12.3 at 88% CPU), not stalled. |

**Roster agreed by the operator at this desk, 2026-09-21, for
`t-delegation-certs` only:** ticket owner `%936` (existing seat, authors its
own frozen gate) **+ one commit owner on Codex `gpt-5.6-sol` effort `high`**.
Gate-author pair **waived**. Auditor **DEFERRED, not omitted** — the slice may
run RED→GREEN→commit, may never be called audited, and acceptance waits for a
later review decision. `audit=DEFERRED` goes in the PR body. Answer:
`t-delegation-certs/answers/A-003-team-negotiation.md`.

Archived and accepted: `t5413-review-findings`, `t5421-invalid-continuation`,
`t5419-specs-deprecations`, `t5422-delete-era-split`, `w-wire-format-15-16`.
**Their prose artifacts no longer exist — see "What the /tmp sweep took".**

## Priority order, with reasons

1. **The four release blockers.** They decide whether the milestone can ship
   and need no review and no merge. **#5416 now outranks the rest** — see below.
2. **#5410 (`t5410`).** New to this desk and it **outranks stub-clearing**:
   same class as `guardsTxBodyL`, code that succeeds wrongly rather than
   failing loudly, and the census cannot count it.
3. **`t-delegation-certs`.** Six of the 27 owed stubs, shape fixed by in-repo
   precedent, now unblocked.
4. Everything else.

**Inversion on the record:** #5410 was filed after the stub work was already
sliced and it went ahead of it anyway. A discovered production blindness
outranks a cleanup arc regardless of filing order.

## Release blockers

| # | state | disposition |
|---|---|---|
| **#5416** | **CLOSED COMPLETED 09-16 — AND TWO INDEPENDENT CHECKS DISAGREE** | See below. **Stands as a release blocker at project level.** Do not reopen, do not comment, do not touch #5435 or #5444. |
| **#5408** | open | `.github/RELEASE_TEMPLATE.md` says *"Compatible with `cardano-node@X`"* — **a point is not a floor**. 11.0.1 and earlier fail *silently*. Nothing upstream gates this; trigger is the next release. |
| **#5409** | open | CI must matrix the LedgerDB backend. Measured: one config sets it, **zero `.github` files name it**. |
| **#5417** | open | **A decision, not a measurement** — HTTP 500 `CreatedInvalidTransaction` vs a 4xx for node-rejected external transactions. Public API contract. **With the operator.** No amount of measuring settles it. |

### #5416 — the close is unsupported by two independent routes

The issue was closed as completed on 2026-09-16. Two desks measured
independently and neither can show the fix in the pinned set.

**This desk's route (marker file, with a control).** The marker
`changelog.d/20260828_183500_..._getgenesisconfig_no_extra_config.md` is
**ABSENT** at `ouroboros-consensus` 4.2.1.0's pinned rev `82ecba329d`; CHaP
carries **no consensus newer than 4.2.1.0**; wallet `master` still pins
`ouroboros-consensus ==4.2.1.0`. **Positive control passes** — the same query
finds the marker on `main`. *Honest limit:* the instrument's subject is CHaP,
and `cardano-node` may pin consensus by another route.

**The project owner's route (`%107`), closing exactly that gap.**
`cardano-node` 11.1.2's own cabal carries
`ouroboros-consensus:{...} ^>= 4.2.0.1` with
`index-state: cardano-haskell-packages 2026-09-16T23:53:07Z`. **A caret at
`^>= 4.2.0.1` cannot reach 4.3 while the newest consensus in CHaP is 4.2.1.0**
— the version lacking the marker. So node 11.1.2 cannot contain #2251 by that
route. *Its limit, named by it:* it verified the **constraint**, not the
resolved plan, and a `source-repository-package` grep finding nothing is
inconclusive, not confirming.

**The burden has moved to whoever closed it.** Routed to the operator with both
measurements. **Not reopened** — reversing another party's decision is outward.

**Why this one is carried loudest:** the failure it guards is the *silent* one.
The wallet connects, negotiates, **applies zero blocks, and logs no error**. A
release cut against that is indistinguishable from a healthy one.

## Parked decisions and what would unblock them

| decision | owner | unblocker |
|---|---|---|
| the five `Api/Extra.hs` stubs | M6 owes a **check, not a change** | M1's #5290 merging. Then verify they died with their owner; take back any that owner drops. |
| `cardano-ledger-read` #20 — era-named entry points | **not M6's to decide** | The draft comment at `handoffs/DRAFT-COMMENT-ledger-read-20.md` is **NOT posted**; whether anything is said to that author is the operator's. **`cabal.project:129` pins `cardano-ledger-read` at `f4d3f064`, which is exactly #20's merge-base — the wallet is one pin bump from consuming it.** |
| #5421 / genesis-checkpoint data loss | **not M6** | escalated to the project owner; node-bump causation was refuted |
| whether ratchet slack should exit non-zero | **not M6** | #5406's owner. Recorded as a deliberate waiver, not an oversight. |
| #5417 | **operator** | a ruling; it is a contract decision, not a measurement |

## Escalations in flight

- **#5416** — routed to the operator by the project owner with both measurements.
- **Stranded composers** — see below. Reported at the desk 2026-09-21.

## What the /tmp sweep took, and the rule it forced

`systemd-tmpfiles` swept `/tmp` at 2026-09-21T12:19 local under the 10-day
rule. **Because it deletes individual files it eats parked and archived state
specifically.** This tree went from **eight** archived roots to **five**, and
four of the five survivors lost their `STATUS.md`. The #5421 investigation's
report with its 31 hash-verified evidence artefacts, and the #5413 evidence
lane's report and CODE-DELIVERY, **no longer exist**.

**The substance survived only because it had been written into `STATUS.md` at
acceptance time.** Archiving a runtime root is not preservation: `/tmp` is not
durable, so the durable record was the journal all along.

> **RULE: journals survived, prose artifacts did not. Write evidence into
> `STATUS.md`.** And: **a missing file read as an absent finding** is the
> failure mode this project has now catalogued seven times. **Verify before
> citing.**

The `/tmp/projects` exclusion is committed to infrastructure `main` but takes
effect only at the operator's rebuild, so the window is still open.

## Standing corrections recorded against this desk

1. **A local `cabal build` green does not predict CI.** The nix derivations
   build with `-O2 -Werror`. Run
   `nix develop --quiet -c cabal build all --ghc-options=-Werror` before any
   push. This desk paid hours for that once.
2. **A rebase of a stack floor silently stops CI above it** — GitHub creates
   **no run object** for a conflicting PR, so the absence of a red check is not
   the presence of a green one.
3. **The census falling is not evidence the paths run.** Every retired site
   needs a test that *executes* it for Dijkstra.
4. **Normalize source before counting.** Six stubs span lines via Haskell
   `\...\` string gaps and are invisible to any single-line regex. This desk's
   first census was wrong by that exact defect after warning someone else about
   it.
5. **One `forall era. IsEra era` entry point, with the `case theEra` inside.**
   Era-named entry points break `applyEraFun`. Measured on #20: adding Dijkstra
   to the polymorphic form cost **3 added, 0 removed**; to the era-named form
   **46 added, 9 removed** plus `UndecidableInstances` and two standalone
   deriving clauses.
6. **`render-milestone.mjs` invoked through its `~/.claude/skills` symlink
   prints nothing, writes nothing and exits 0** — its `import.meta.url` guard
   is false. Invoke it at its real path and check the file, never the exit code.

## Operator-facing projection

| | |
|---|---|
| wiki page | https://github.com/cardano-foundation/cardano-wallet/wiki/Milestone-118 |
| register | `Milestone-118-Stories.json` in the wiki repo, schema `milestone-stories/v1` |
| last published wiki commit | `845637b2d60eabed27153cac8231d229ce3397f3` (2026-09-08) |
| register digest | `fd483b145dfd5f1f191042eb1717401712d0b93a9d64571e5f53dff056318632` |
| renderer | `/code/llm-settings/shared/skills/debrief/scripts/render-milestone.mjs` |

**WIKI STALE as of 2026-09-21** — the register predates the 39→32 census move,
#5410, the #5416 close, and both current lanes. Owed at the next sweep.
The legacy `M118-State.md` page redirects here; two pages describing one
milestone is how a stale one gets read as current.
