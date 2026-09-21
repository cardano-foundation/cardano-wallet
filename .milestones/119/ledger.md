# cardano-wallet technical-debt cleanup ledger

Home repo: cardano-foundation/cardano-wallet.
GitHub milestone: #119, `M7 — Technical debt cleanup`.
Desk: session `wallet`, window `wallet:2 cardano-wallet-ms119-technical-debt`,
pane `%37`, family `claude`, model `claude-opus-5[1m]`.
Desk runtime root: `/tmp/machine/session-restore-20260824/wallet/ms119-desk/`
(assigned by the project owner on the 2026-08-24 restore; the historical root
`/tmp/ms-cw-tech-debt/` still holds the long-run desk journal and `.archived/`
lanes, and its STATUS.md remains appended for continuity).
Parent: **cardano-wallet project owner**, pane `%35`, window `0-projects:3`.

Separate from milestone #113 "Drop cardano-api" — out of scope for this desk.

**Issue #5350 / PR #5363 is NOT an M119 child.** Project-direct standalone lane,
owned by the project owner through ticket owner `%38` (window `wallet:3`). Ruled
2026-08-24. This desk does not dispatch into it, supervise it, or count it.

## State — PARKED 2026-08-24 (was ACTIVE after the morning restore)

Parked `2026-08-23T15:18:41Z` under OMNIA-PAUSA-2026-08-23; released
`2026-08-24T09:3xZ` by operator release relayed through the project owner.
Desk seat was rebuilt on a fresh context (pane `%5355` → `%37`, Fable/Sonnet →
`claude-opus-5[1m]`).

## Reconciliation of 2026-08-24 (re-derived, not restated)

**Nothing moved in the repository during the park window.** Master head is
`346786a112` (2026-08-23T06:36:57Z), which predates the park. No issue or PR in
the repo carries an `updatedAt` inside 08-23T15:18Z → 08-24T09:40Z.

**Burn-down 6/27 — CONFIRMED** by independently re-querying all 27 starting-set
issues. Closed: #5063, #5103, #5196, #5246, #5325, #5341. Exactly matches the
2026-08-19 recount. Unchanged while parked.

**S tickets 3/4 — CONFIRMED.** #5103, #5196, #5246 closed; #5115 open, parked.

**The GitHub milestone counter has self-healed.** Recorded stale at 0/0 on
2026-08-02; now reports 8 closed / 2 open. M119 membership is now exactly:
open {#5326, #5381}; closed {#5325, #5358, #5359, #5373, #5375, #5377, #5379,
#5385}.

**Three ledger entries were found stale — two of them were already wrong when
the 2026-08-22 sweep wrote them:**

| Item | Ledger said | Reality | Verdict |
|---|---|---|---|
| PR #5370 | "blocked on operator decision: fork-CI approval + review routing" | **MERGED 2026-08-21T06:39:54Z** | stale at authoring — resolved a day before the sweep |
| #5387 / PR #5384 | "in flight, stale-payload snag, operator hasn't said go" | **MERGED 2026-08-22T09:50:28Z**, issue #5387 closed | stale at authoring — landed ~3h before the sweep |
| #5385 / PR #5386 | "spec-first PR, converted to draft, pre-implementation" | **MERGED 2026-08-23T06:36:58Z** — it *is* the current master head | stale since the sweep |

Consequence: **the post-release automation line is COMPLETE**, and the
"operator decisions outstanding" list drops from five to three.

**Queue-membership nuance (does not change order):** the desk queue is drawn
from the reconstructed 27-item technical-debt set, **not** from M119 membership.
#5334 / #5108 / #5094 / #5330 sit on milestone `M3 — CI trust`, #5115 on
`M5 — Benchmarks`, #5146 on no milestone. Do not "correct" the queue by
filtering on milestone 119 — that would silently drop four queued items.

## Outcome test

Starting set (reconstructed 2026-08-19, exactly 27): 4411 5063 5075 5086
5094 5097 5098 5103 5106 5108 5114 5115 5146 5147 5155 5159 5170 5182 5196
5233 5246 5252 5325 5326 5330 5334 5341.

1. S tickets: **3/4** (#5103, #5196, #5246; #5115 parked).
2. Burn-down: **6/27** (#5063, #5103, #5196, #5246, #5325, #5341) — 8 more
   needed for the ≥14 clause.
3. CI badges green: **MET 2026-08-18/19**, embodied in the shipped release.

## Product milestone event

**Release v2026-08-21 shipped** (published 2026-08-22T08:01Z, tag at
`be7898f4`) — the release the badge-green drive prepared. Carries all four
badge fixes (#5374, #5376, #5378, #5380) and ships cardano-addresses 4.0.2
per the deferral ruling.

## Active lane

**#5326 — graceful shutdown drain of wallet database workers.** Dispatched
2026-08-24T09:44Z.

- Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5326
- Ticket owner: pane `%40`, window `wallet:4
  cardano-wallet-ms119-t5326-shutdown-drain`, family `codex`,
  model `gpt-5.6-sol`, effort `high`.
- Runtime root:
  `/tmp/machine/session-restore-20260824/wallet/ms119-desk/t5326-shutdown-drain/`
- Branch (to be created by the owner): `fix/5326-drain-wallet-workers-on-shutdown`
- Worktree (owner-created): `/code/cardano-wallet-issue-5326`
- Base: `origin/master` @ `346786a112`
- Seats derived mechanically: T.O. `codex` → commit owner `grok` → auditor
  `claude`; all three families distinct. One grok seat on this ticket.
- Post-delivery state is **AWAITING-EXTERNAL-REVIEW** (Pawel), per the issue
  body — that review wait is success, not a blocker.
- **Dispatch incident, resolved:** the owner's first `START` reported
  `pane=%38` because it ran *untargeted* `tmux display-message -p '#{pane_id}'`,
  which returns the attached client's focused pane — the client was on
  `wallet:3`. Corrected via `inbox/NOTE-001-pane-identity-correction.md`,
  acked 09:45:04Z, then verified mechanically from `/proc/1089213/environ`
  (`TMUX_PANE=%40`). Blast radius nil: the #5350 lane was never written to.
  The hazard was prospective — a split targeting `%38` would have put the grok
  commit owner inside the #5350 window.

## Priority queue (standing, from the 2026-08-19 paving)

1. **#5326** shutdown drain — PR #5388 open (draft), CI queued, **NOT accepted**, parked.
2. #5334, #5146 (small CI/CD+docs) → #5108, #5094 (flake family).
3. #5252 (spec draft PR #5364 exists) → test-perf cluster
   (#5097/#5098/#5114/#5155/#5159) → tail (#4411, #5086, #5147, #5170,
   #5182, #5233).

## Blocked / waiting

- **#5381** — cardano-addresses 4.0.8. TWO independent blockers (see the 2026-08-25 analysis below): CHaP index-state visibility, AND an unresolved crypton ceiling in
  ecosystem bump, most naturally with the next node pin-set advance.
  Relaxation of the crypton bound to `>= 1.0` was **empirically disproven**
  (bound welded to the memory→ram migration `8b3978a6`; crypton==1.0.6 fails
  with 7 GHC-39999 ByteArrayAccess errors; no 1.0.7 exists). No PR opened —
  honesty clause held. Operator to tell Pawel the verbal ask is unsatisfiable.
  No movement since 2026-08-22T12:43Z.

## Blocked on operator decision (three, down from five)

- **#5330** — alerting option. Untouched since 2026-07-29.
- **#5115** — last S ticket; needs release from the no-new-work order.
  Untouched since 2026-07-29.
- **Ancillary-verification probe** (from the #5380 review): does
  `mithril-client` actually enforce ancillary-key verification, or
  warn-and-proceed? Pursue or drop.

Closed by reconciliation: ~~#5370 CI approval~~ (merged 08-21),
~~#5384 empty-commit retrigger~~ (merged 08-22).

## Standing constraints

Operator alone merges. No cardano-api work. Pawel reviews production-semantics
changes. tmux/worker-protocol dispatch only — no Agent-tool subagents. No
unattended pollers, crons, or detached watchers; `monitor-workers` only while a
lane is actually dispatched. Disk floor respected before any large closure
(2026-08-24: `/` 91G free, `/nix` 544G free). Shared-pool CI probes capped per
brief. Preserve untracked `/code/cardano-wallet/.llm/issue-5309-unit-memory-analysis.md`
on master. Standing authoritative families: `claude`, `codex`, `grok`;
`agy` REVOKED 2026-08-14; `qwen` draft-only.

## Park — 2026-08-24 ~13:59Z

Operator instruction "pause the work on this session", relayed by the wallet
session owner as `NOTE-004-pause-wallet-session`. Complied with; routing flagged
below. State page refreshed to PARKED *before* the park was logged.

**#5326 reached a PR while the desk was unattended.** PR #5388, draft, head
`68c5457c46`, MERGEABLE, 12 files. Full OWNER campaign: grok commit owner `%88`;
submission 1 (`c4eab5e868`) rejected by fresh claude auditor `%93` on 2 blocking
findings; submission 2 (`1f5dab7c95`) accepted by a second fresh auditor `%101`
(`blocking=1 reason=diff-ceiling-only`, `semantic_findings_closed=true`);
CEILING-RAISE 1/2; `next_submission=forbidden`; FINAL-COMMIT `68c5457c46`.
Seat alternation held throughout (codex → grok → claude, fresh pane and worktree
per audit).

**NOT ACCEPTED.** The lane failed its own final ticket gate twice, classified
both as `local-cluster-startup-environment` / `no-process-holds-node-socket`,
exhausted its 4/4 build budget, and pushed with `remote_CI_required=true`. The
classification is plausible and unverified. The acceptance bar is in
`resume/ms.md` and must be applied on resume.

**Routing note:** this pause arrived from the *session owner*, not the project
owner. Per the role contract a pause from the machine/session edge is misrouted
and should be acknowledged without acting pending a governed cascade. It was
complied with anyway because it carried a **direct operator instruction**, the
operator outranks the cascade, and parking is safe and reversible — the cost of
wrongly pausing is far below the cost of wrongly continuing. Flagged so the
project owner can confirm or correct the route.

**Morning ledger-publish blocker RESOLVED.** GPG signing succeeded at 13:57:58Z;
commit `5f1cc6dc02` verifies as `Good signature from paolo`, sibling
`.milestones/1` preserved.

**Sweep defect on record (mine).** At ~14:58Z I ran `ledger-sweep.sh pull`
*after* editing the checkout. `pull` does `rm -rf` on the checkout, so it
destroyed the PARKED edits and the subsequent push landed an empty-diff commit
(`845341a6a9`). Recovered: `state.md` was restored from the already-published
wiki page, `ledger.md`/`resume/ms.md` re-applied, and pushed without a
re-pull. Rule for the next sweep: **pull, then edit, then push — never pull
between edit and push.**

## Epic (approved, NOT yet founded) — Agda→Lean specification migration

Operator ruling relayed by the project owner as
`inbox/NOTE-004-lean-migration-epic-approved.md` (2026-08-24). The operator
approved the direction and ruled it be founded as an epic **under M119,
sequenced after the current queue**: #5326 (in flight, PR #5388), then
#5334 / #5146 / #5108 / #5094. The ruling is the product premise; it is cited,
not re-litigated. Custody of the drafts passed project owner → this desk; the
#5350 lane drafted them and files nothing. Frozen source artifacts:
`…/0-projects/cardano-wallet/decisions/lean-migration-epic.md` and
`lean-migration-children.md` (epic body + five ordered children: pinned Lean
project/CI gate → delegation model + #5350 mirrors ∥ read primitives → rest of
read model → remove Agda surface with a zero-reintroduction guard).

**Status: approved and recorded, deliberately NOT filed.** The desk is PARKED
under an operator pause; founding an epic means filing GitHub issues, which is a
repository mutation and new work. Recorded here so a crash cannot lose the
ruling; filing waits for a release. Sequencing costs nothing by waiting — the
epic sits behind the whole current queue by the operator's own ruling.

**Draft claims verified independently (2026-08-24, read-only):**

| Claim | Verdict |
|---|---|
| twelve Agda sources, 1 in `specifications/` + 11 under `lib/cardano-wallet-read/agda` | **CONFIRMED** — `git ls-files '*.agda'` = 12, split 1/11, plus 1 `.agda-lib` |
| `leanpkg.toml` pins Lean 3.49.1 | **CONFIRMED** — `specifications/leanpkg.toml`, `lean_version = "leanprover-community/lean:3.49.1"`, self-described as a *dummy* package existing only to pin the version |
| only `IsEra` carries `AGDA2HS`; these are not generated sources | **CONFIRMED** — exactly one directive, `Eras.agda:32`, `existing-class` |
| duplicate search clean | **CONFIRMED** — no open Lean/Agda migration issue; newest issue is #5387 (2026-08-22); #5388 is a PR, not an issue |

**Two findings the drafts do not carry — both registered as contracts:**

1. **The #5350 guarantees are not on master.** All six QuickCheck properties
   return zero files on master; they exist only in unmerged PR #5363. The epic's
   binding contract points at artifacts that do not yet exist, owned by a lane
   this desk is fenced from. Child 2 has an unstated hard dependency on #5363
   merging. Escalated, not resolved here.
2. **The existing Lean CI gate has never gated anything.** `lean.yml` is
   path-filtered to `specifications/**.lean`, which no master push has ever
   touched; the workflow has exactly one run in its whole history (2026-02-13,
   manual dispatch, non-master branch). #5340 diagnosed this and was closed by
   removing the badge, not by fixing the gate. Child 1 is therefore founding a
   Lean gate from zero, not exposing an existing one.

## Re-park — OMNIA PAUSA 2026-08-24T14:20Z

Human-operator order, verified sha256 `22e4bade…0438c156`; `wallet` named
explicitly, **M119 not exempt**. Relayed by the project owner as `NOTE-005`.
Does not overwrite the 13:59Z session pause; both records stand.

Order of operations honoured: state page refreshed and verified live **before**
`PARKED` was logged, then the cascade to `%40`, then owned-automation check
(TaskList empty, CronList empty, no `wait-status` / `monitor-workers` /
`gh run watch` process owned), then this ledger, then STATUS.

**#5326 / PR #5388 — CI completed during the park.** Head `68c5457c46`,
draft, MERGEABLE. **56/57 checks green; `Conway Integration Tests` FAILED**
(run 32735463006). Classification **UNDETERMINED**; see `resume/ms.md` for why
guessing it is the specific trap on this ticket. Nobody acts on it until a
written RELEASE naming this order.

**Q-001 resolved by this order.** It asked whether the epic-founding note
superseded the earlier pause. Answer: no — file nothing. The approved Agda→Lean
epic stays recorded, drafts frozen and hash-verified, unfiled.

## RELEASED 2026-08-25T09:48Z — full Wallet scope

`RELEASE-2026-08-25T0948Z-wallet-full.md`, sha256 `c65b00fd…4e04011` (recomputed
here, MATCH), releasing from `OMNIA-PAUSA-2026-08-24T1755Z` (`d29a7020…fffa8e54`,
also verified). Operator: *"I want the full wallet lane released."* Desk ACTIVE.
Waives nothing. No pause-era automation re-armed; this desk still owns none.

**Q-001 CLOSED** — A-001 read, `RESUMED` logged. Ruling was option 2 (file
nothing), which the subsequent OMNIA PAUSA independently confirmed.

### #5326 — criterion 4 is MET, verified independently

`%40` classified the `Conway Integration Tests` red from the actual job log
(`evidence_sha256=30e8f9ac…`) and **this desk verified it from the raw log
separately** rather than relaying the claim:

- job log line 717: `shutdown drain acquired=2 closed=2 … sigterm=True exit=ExitFailure (-2)`
- line 718: `SIGTERM drain observes two wallet close callbacks [✔] (22248ms)`

So the proof deferred to CI **passed**: two workers, real SIGTERM on the shipped
path, both close callbacks observed, bounded at 22.2s — acceptance criterion 4,
met. The terminal failure was `SHARED_TRANSACTIONS_LIST_RANGE_03 — "Expected at
least one transaction with a time"` in a `1055 examples, 1 failure, 41 pending`
run: a shared-wallet transaction time-range test, unrelated by scope to worker
shutdown drain. **Neither `%40` nor this desk asserted "flake"** — `%40` recorded
`flake_label=not-asserted`, correctly. It was neither of the two outcomes this
desk predicted; it was a third, which is why the brief told the lane to stay open
to one.

Follow-up worth owning separately: that unrelated red is still *someone's*
problem. Candidate standalone ticket — it is not #5326's to absorb.

`%40` first fresh state: **progressing**. Rebased `68c5457c46` → `df1cdf6dc6`
onto `ab51934b7b`; post-rebase gate running. Note it was woken at 09:55:35Z by a
**direct operator writing** ("not blocked anymore") three minutes before this
desk's durable release reached it — its conclusion was correct and is now backed
by the verified release file, and it recorded its interpretation explicitly
rather than acting silently. Recorded because direct operator→grandchild
writings bypass this desk and can skew a receipt denominator.

### Agda→Lean epic FILED

Epic **#5389** + children **#5390–#5394**, all on milestone #119, labels applied,
duplicate search re-verified at filing time (clean; newest prior issue #5387).
Sequenced **after** the current queue per the operator ruling. Two findings from
this desk's own reading were folded into the body and are not in the original
drafts:

1. the Agda gate is *proven able to fail* per law, so this is not
   "unchecked → checked" but "a check that always proves it can fail" →
   "a check that has never run";
2. `lean.yml` has never gated anything, so #5390 founds a gate from zero and must
   reach **parity**, not merely exist.

The draft sentence "the repository already has a Lean project and CI workflow"
did **not** survive unqualified.

### Scope boundary accepted

cardano-node 11.1.0 under **M6 — Dijkstra HF readiness** (milestone #118, anchor
#5209) is **not M119's**. Not absorbed, not queued, nothing filed against #118,
no lateral coordination. Flagged for arbitration: a node pin-set advance is
exactly what **#5381** waits on, so there is a plausible real dependency-surface
overlap between that milestone and M119's blocked cardano-addresses line.

## Terminal boundary — "pause after 5326 merge"

Operator instruction, 2026-08-25. #5326 finishes genuinely and the desk then
parks; no next queue item, no epic dispatch, no new lane. Recorded in full in
`resume/ms.md`, including the explicit guard that the boundary must not be read
as pressure to merge. `%40` acked at 10:05:39Z with
`boundary=after-genuine-completion-not-merge-shortcut`,
`gate_head=df1cdf6dc6`, `rebased_shutdown_smoke=still-required`,
`merge_authority=operator`.

## #5381 — blocker analysis corrected ADDITIVELY (2026-08-25), one claim disputed

NOTE-007 reported #5381's recorded blocker as false and reversed A-001's
ordering. I re-derived from the published `.cabal` files. **Half of it holds and
is a genuinely new finding; the central claim does not, and the ledger's original
sentence was right.** Q-002 filed; the public #5381 comment is deliberately
withheld pending the ruling.

**Blocker 1 — visibility. REAL, and newly identified by the M6 desk.**
`cabal.project:56` pins `cardano-haskell-packages 2026-07-20T14:49:58Z`.
`cardano-addresses-4.0.8` entered CHaP 2026-08-16 and is invisible at that
index-state. Positive control: 4.0.2 predates the cutoff and resolves today.
This was never in this desk's record and it should have been. Good catch.

**Blocker 2 — solvability. ALSO REAL. Not deleted.**

```
cabal.project:191             cardano-crypto-class ==2.3.3.0  (dep of 4+ wallet libs)
cardano-crypto-class-2.3.3.0  crypton ^>=1.0   ==  >=1.0 && <1.1
cardano-addresses-4.0.8       crypton >=1.1 && <1.2
                              --------------------------------
intersection                  EMPTY
```

NOTE-007 states 4.0.8 declares `crypton >= 1.0 && < 1.2`. **It declares
`>= 1.1 && < 1.2`** (line 137 of the published cabal). The refutation checked
whether `cardano-addresses` *declares* `cardano-crypto-class` — it does not
(count 0), and this ledger never claimed it did. The coupling is a **transitive
solver conflict through `crypton`**, and it stands.

**New fact neither record carried, and it is worse than both:** no published
`cardano-crypto-class` accepts `crypton >= 1.1`. Checked forward from the pin —
2.3.3.0 `^>=1.0`, 2.4.0.0 `^>=1.0`, **2.5.0.0 `^>=1.0`** (2.3.4.0 and 2.4.1.0
return 404). **A node pin-set advance would therefore not unblock #5381 either.**
The ceiling is upstream in `cardano-crypto-class` across every published version.

**Real unblock:** an upstream `cardano-crypto-class` crypton relaxation — or a
cardano-addresses floor drop, which the archived grok lane already disproved
empirically (memory→ram coupling, 7 GHC-39999 errors at crypton 1.0.6). That
lane now reads as a coherent attack on the true conflict rather than a
misdirected one.

**Ordering:** #5381 is **not** established as independently actionable.
Advancing the index-state makes 4.0.8 visible but unsolvable. Pending Q-002, it
stays queued behind the current work, and it is not dispatched at all before the
"pause after #5326 merge" boundary.

**Caveat on this desk's own claim:** declared bounds were verified, not a solver
run. A `cabal build --dry-run` at an advanced index-state would be conclusive and
was not run — that is a build, and the desk is under a pause-after-merge
boundary. Bounded task for the #5381 lane when dispatched.

**Accepted unchanged from NOTE-007:** `cardano-addresses` is not in 11.1.0's
forced closure (nothing inherited from M6 here); and #5293 closes as superseded
by #5381 — with a comment naming it — **when #5381 lands, not before**.

## Q-002 RESOLVED — A-002: reversal withdrawn, both blockers stand

The project owner re-fetched the CHaP cabal files independently and confirmed
the refutation in every particular. **NOTE-007 §2, the A-001 ordering reversal,
and "#5381 is independently actionable now" are WITHDRAWN.**

Settled position on #5381 — **two independent blockers, neither dropped**:

1. **Visibility** — `cabal.project:56` pins CHaP `2026-07-20T14:49:58Z`;
   `cardano-addresses-4.0.8` entered CHaP `2026-08-16`. Invisible to the solver.
   (The project owner's find; genuinely new and previously unnamed here.)
2. **Solvability** — `cardano-crypto-class ==2.3.3.0` declares `crypton ^>=1.0`
   (`<1.1`); `cardano-addresses-4.0.8` declares `crypton >=1.1 && <1.2`.
   Intersection empty. Not a direct dependency — a transitive solver conflict.

**Clearing either alone changes nothing.** #5381 stays parked; no index-state
advance on its behalf alone, because that spends the diff and leaves the lane
unbuildable.

**The decisive new fact:** no published `cardano-crypto-class` admits
`crypton >=1.1` — 2.3.3.0, 2.4.0.0 and **2.5.0.0** (the version `cardano-node`
11.1.0 requires) all declare `^>=1.0`. So a node pin-set advance does **not**
unblock #5381. The project owner is carrying that to the M6 desk directly; this
desk does not cross the fence.

**Public record corrected:** both blockers, with the crypton intersection shown
as a table rather than described, at
https://github.com/cardano-foundation/cardano-wallet/issues/5381#issuecomment-5409171747

**Upstream candidate filed, NOT started** —
`asks/ASK-002-upstream-crypton-ceiling-candidate.md`: relax
`cardano-crypto-class`'s crypton ceiling to `<1.2` in `IntersectMBO/cardano-base`.
Estimate given with uncertainty named (~60% one-line bound widening + green
build; ~40% it hits the mirror image of the disproven floor-drop, if
`cardano-crypto-class` depends on the old `memory`-flavoured `ByteArrayAccess`).
Decisive question is cheap: build `cardano-crypto-class` 2.5.0.0 against
crypton 1.1.x and read the errors. Placement belongs to the project owner —
upstream work in another organization's repository. Not opened, will not be.

**#5293** closure stays blocked behind #5381, unchanged.

**Process note for the record.** Two successive rulings from the project owner on
this question were wrong (A-001's ordering, then NOTE-007's reversal of it), and
both were corrected because this desk re-derived instead of complying. The
escalate-to-parent / no-lateral-contact fence routed both errors back through the
one seat that could see them. Worth keeping because it is the argument for the
fence, not against it.

## A-003 + the finding that obviates it — #5381 reshaped (2026-08-25)

**A-003 ruling accepted:** ASK-002 closed as *superseded*, not rejected. The
crypton-ceiling work became a named sub-deliverable of A2.0 in M6's epic #5395.
The project owner's correction — that this repo *pins upstream commits* rather
than waiting for upstream releases — was verified here: five
`source-repository-package` blocks (`cabal.project` lines 94, 101, 113, 125,
137), all bare SHAs, three of them `cardano-foundation`. Nuance recorded, not
disputed: every precedent is an org the team controls plus one small third-party
library, so forking `IntersectMBO/cardano-base` would exceed any current
precedent.

**Then the question dissolved.** `cardano-crypto-class-2.6.0.0` declares
`crypton ^>=1.1` (`>=1.1 && <1.2`), which **intersects**
`cardano-addresses-4.0.8`'s `>=1.1 && <1.2`. Verified across every published
version — 2.3.3.0, 2.4.0.0, 2.5.0.0, 2.5.1.0 all `^>=1.0`; only **2.6.0.0** is
`^>=1.1`. It entered CHaP **2026-07-27**.

**The upstream relaxation does not need doing. Upstream shipped it a month ago.**
If A2.0 is scoped to perform it, that scope is wrong — routed to the project
owner for M6, per the fence.

**Public record self-corrected.** This desk's earlier #5381 comment claimed no
published `cardano-crypto-class` accepts `crypton >=1.1`. That was false and is
corrected on the issue with the full version table:
https://github.com/cardano-foundation/cardano-wallet/issues/5381#issuecomment-5409304870

**Revised #5381 position — the two blockers are less independent than recorded:**
`cardano-addresses-4.0.8` (CHaP 08-16) and `cardano-crypto-class-2.6.0.0`
(CHaP 07-27) are *both* invisible at index-state `2026-07-20T14:49:58Z`.
Advancing past 08-16 makes both visible and a compatible pair exists.

**What is still genuinely hard, and it is in-repo, not upstream:**
`cardano-crypto-class ==2.3.3.0` sits inside the Cardano Node 11.0.1 block at
`cabal.project:191`. Node 11.1.0 carries **2.5.0.0** (`^>=1.0`), so **neither the
current pin nor the 11.1.0 bump delivers 2.6.0.0**. Realistic paths: a node
pin-set advance that carries `>=2.6.0.0`, or an out-of-set bump of that package
alone. Acceptance criterion 5 governs — stop and report if it drags in unrelated
major bumps.

**#5381 remains parked.** Not acted on; reported only.

**Lesson recorded against this desk, not the parent.** ASK-002 proposed doing
upstream work without first checking whether upstream had already done it. It
named the right mechanism and the right decisive experiment and still missed a
published answer. "Has upstream already solved this?" belongs *before* "should we
go and solve it upstream?" — and the check cost one HTTP request.

## #5326 — DELIVERED to its defined post-delivery state (2026-08-25 10:53Z)

**58/58 CI green** on the rebased head `df1cdf6dc6`, PR #5388 marked ready
(`draft=false`), MERGEABLE, `AWAITING-EXTERNAL-REVIEW` with Pawel — which the
issue body defines as delivery success, not a blocker.

**Acceptance bar satisfied on independently re-derived evidence.** The rebase
invalidated the earlier proof, so criterion 4 was re-verified by this desk on the
exact head that will merge — job 97770010149:

```
717: shutdown drain acquired=2 closed=2 … sigterm=True exit=ExitFailure (-2)
718: SIGTERM drain observes two wallet close callbacks ✔ (22305ms)
     1055 examples, 0 failures, 41 pending
```

Two workers, real SIGTERM on the shipped shutdown path, both close callbacks
completed, bounded at 22.3s — not an unbounded wait. Criterion 4: **met**.

`SHARED_TRANSACTIONS_LIST_RANGE_03`, which failed the pre-rebase run, passes
here on its own. That retrospectively supports the unrelated-and-transient
reading **without either desk having had to guess it in advance** — which is
precisely the value of having refused to label it while parked. No flake label
was ever asserted and none was needed.

Campaign shape, for the record: grok commit owner `%88`; submission 1
(`c4eab5e868`) rejected by fresh claude auditor `%93` on two blocking findings;
submission 2 (`1f5dab7c95`) accepted by a second fresh auditor `%101`; final
commit, rebase onto `ab51934b7b`, force-push with exact lease. Seat alternation
held throughout (codex → grok → claude, fresh pane and worktree per audit).

**Merge is not this desk's.** Pawel's review and the operator's merge authority
both stand. The operator boundary — *pause after #5326 merge* — takes effect when
that merge lands: #5334/#5146/#5108/#5094 stay undispatched and epic #5389 stays
filed-and-undispatched.

## Standing method rule adopted project-wide (2026-08-25)

> **Before reporting an absence, show the method finding something you know is
> there.**

Adopted from the project owner, and adopted *concretely*: filed ticket **#5394**
now requires a positive control on both its absence criteria (`rg --files -g
'*.agda'` returning nothing, and "no active agda2hs reference remains"), and epic
**#5389** carries the rule as a standing requirement for every child. Registered
as a contract with an honest `PARTIAL` enforcement line. See `registry.md`.

## PARKED — 2026-08-25, operator: "pause the milestone"

State page refreshed and verified live **before** this was logged, per contract.
Does not overwrite prior pause records.

**#5326 COMPLETE and merged.** PR #5388 → merge commit `0a7332482c`, parents
`ab51934b7b` + `df1cdf6dc6` (exact audited, CI-green head). True merge, no
rebase/squash. Issue closed COMPLETED 10:59:47Z. **Burn-down 6/27 → 7/27**
(`5063 5103 5196 5246 5325 5326 5341`, re-derived issue by issue).

Drain verified present on `master` — `Registry.hs`, `ShutdownDrain.hs`, wired
into `Run.hs` — with a positive control (`newApiLayer`) run first, per the rule
adopted this morning.

**RESIDUAL 1 — the ticket's own review gate was bypassed.** #5326 required Pawel
as reviewer and required the ticket to remain open for his review of the exact
accepted head. PR #5388 merged with **0 reviews**, `reviewDecision=REVIEW_REQUIRED`,
and auto-closed the issue. The change alters production shutdown and
wallet-worker lifecycle semantics — the reason the gate existed. The code is
sound on the evidence this desk gathered; that is not the same as the gate having
been satisfied, and green is not review. Operator's call; flagged, not reversed.

**RESIDUAL 2 — no terminal event from the lane.** `%40` ended at
`AWAITING-EXTERNAL-REVIEW` and its pane/window were closed by something other
than this desk, with no `COMPLETE`. Worker-protocol requires every stop to carry
a terminal tag; this one did not, so the parent accepted from durable artifacts
instead of a lane claim. Root archived to `.archived/t5326-shutdown-drain/` with
all handoff evidence and its full STATUS journal preserved.

**Everything else stopped in place** — queue undispatched, epic #5389 filed and
undispatched, #5381 parked, three operator decisions open.

## RELEASED 2026-08-25T13:20Z, and the first thing reconciliation found

`RELEASE-2026-08-25T1320Z-wallet-full.md` (`14d760ba…304c4e10`) from
`BUDGET-PAUSE-2026-08-25T1140Z-wallet-too` (`d4c7e6a9…6eba1e20`) — both hashes
recomputed here. Operator knowingly reopened a metered lane; capacity, not a
waiver. Desk ACTIVE.

**master is red and it is this milestone's doing — but not in the way the raw
signal suggests.** Merge `0a7332482c`: 99 success / 1 failure,
`Conway Integration Tests (macOS)`, sole failure the drain smoke #5326 added
(`cluster start timed out`, `ShutdownDrain.hs:154:5`, 180159 ms).

**First framing corrected before it was published.** Pre-merge master
`ab51934b7b` had the same job at **54 failures** with no drain test present
(positive control: 0 vs 119 hits). 54 → 1. #5326 *improved* master. The honest
claim is "added one new failing test on a platform its PR never exercised", not
"introduced a regression", and the two are not interchangeable.

Drain logic not implicated: success diagnostic absent entirely, negative controls
passed, only the smoke's own second `local-cluster` missed a Linux-tuned budget.
**Criterion 4: Linux proven, macOS unproven.**

**#5396 filed** (milestone #119) with the full evidence and a non-goal
forbidding a macOS skip. #5326 untouched and terminal.

**Priority inversion proposed, not taken:** #5396 ahead of
#5334/#5146/#5108/#5094, reason recorded — sole cause of a red master, and it
protects a criterion just delivered. Awaiting the project owner's sequencing.

## Inbound escalation from ms118 — accepted, and half of it refuted (2026-08-26)

`ESCALATION-werror-not-gated`, from the `e-tracing-migration` epic owner `%157`
under **ms118** — not this chain — routed on direct operator instruction.
Accepted as M119 subject matter: build-gate hygiene is technical debt. The
sender was scrupulous, writing in its own root and not into this one.

**Protocol deviation not complied with:** the pointer asked for the
acknowledgement to be written into *that lane's* `STATUS.md`. Acknowledged here
instead. A process writes only its own journal; the concrete harm is that
`%157`'s parent `%121` supervises through it, so a foreign line could be read as
its worker's own event. Read as a templating slip — every prior pointer this
session named this desk's own file.

**CONFIRMED — their Finding 2, the sharper one.** `cabal.project:258` sets
`-Wwarn=unused-packages`, deliberately demoting that class; `justfile:51`
appends `--ghc-options="-Werror "` on the *command line*, re-promoting every
class including that one. `just build` overrides a policy the project explicitly
wrote down, and the `rio` failure is one `master`'s own config says should not
occur.

**REFUTED — their Finding 1.** Claim was "no CI gate enforces `-Werror` at all"
and "`-Werror` exists in exactly one place". Both wrong:
`nix/haskell.nix:277-278` sets `flags.release = true` for every project package,
and each `.cabal` carries `if flag(release) → ghc-options: -O2 -Werror`. **CI's
nix build does enable `-Werror`.** It also appears in ~30 `.cabal` files and
twice in the justfile. Positive control: ~80 `Werror` hits repo-wide, so the
pattern finds things.

**The correction yields a better question:** if CI enables `-Werror` on project
packages, why did the `PaymentCredential` deprecation reach `master`? Four
candidates listed to *measure*, not assume.

**#5400 filed** to milestone #119 with the confirmed finding, the correction on
the record so it stops propagating, the open question with an acceptance
criterion demanding a measured answer, a shown-able-to-fail clause, and the
positive-control rule.

**Fence held.** No contact with `%157`, `%121`, ms118, or epic #5395; no action
inside their tree beyond reading the file pointed at. The Finding-1 correction
reaches them through the project owner, and via public ticket #5400.
