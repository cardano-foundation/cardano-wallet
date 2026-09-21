# Resume — cardano-wallet technical-debt cleanup desk (#119)

- **State: ACTIVE** — released 2026-08-25T13:20Z by `RELEASE-2026-08-25T1320Z-wallet-full.md` (sha256 `14d760ba…304c4e10`, recomputed and verified) from `BUDGET-PAUSE-2026-08-25T1140Z-wallet-too` (`d4c7e6a9…6eba1e20`, verified). Operator knowingly reopened a metered lane.
  `wallet:2 cardano-wallet-ms119-technical-debt`, session `wallet`,
  family `claude`, model `claude-opus-5[1m]`.
- **Parent:** cardano-wallet **project owner**, pane `%35`, window
  `0-projects:3`. Escalate there — not to the machine or session owner.
- **Desk runtime root:**
  `/tmp/machine/session-restore-20260824/wallet/ms119-desk/`.
  The historical root `/tmp/ms-cw-tech-debt/` holds the long-run journal and
  `.archived/` lanes and is still appended for continuity.
- **Active lane:** #5326 shutdown drain — ticket owner pane `%40`, window
  `wallet:4 cardano-wallet-ms119-t5326-shutdown-drain`, codex `gpt-5.6-sol`
  high, worktree `/code/cardano-wallet-issue-5326`, branch
  `fix/5326-drain-wallet-workers-on-shutdown`, base `346786a112`. Runtime root
  `…/ms119-desk/t5326-shutdown-drain/`. Post-delivery state is
  AWAITING-EXTERNAL-REVIEW (Pawel).
- **Next after #5326:** #5334, #5146 → #5108, #5094. Queue is drawn from the
  27-item set, **not** from M119 membership — do not filter by milestone.
- **Operator decisions pending (three):** #5330 alerting, #5115 release from
  the no-new-work order, mithril ancillary-key probe. (#5370 and #5384 were
  closed by the 2026-08-24 reconciliation — both had already merged.)
- **Waiting:** #5381 on a cardano-crypto-class ecosystem bump.
- **Not ours:** #5350 / PR #5363 — project-direct lane, ruled 2026-08-24.
- Full detail: `ledger.md` (same snapshot).

## Park state (2026-08-24 ~13:59Z)

- **In flight, deliberately not stopped:** PR #5388 CI (5 Build Gates queued,
  none failed) at head `68c5457c46`. Remote and already running; cancelling
  would be a mutation. Nobody acts on the result until release.
- **Child `%40` parked** at a read-only boundary, `mutations_after_park=none`.
  Its own children `%88` (grok commit owner), `%93`/`%101` (claude auditors) had
  already completed and closed — no live grandchild.
- **This desk holds no cron, watcher, poller, or background monitor.**
- **On resume, do NOT accept #5326 on its own word.** Acceptance bar, verbatim:
  (1) read the gate evidence at sha256 `22fb511c67…` and `e969a140c2…` directly
  and confirm the two `final-ticket-gate` failures really are local-cluster
  *startup* environment and not the drain; (2) prove acceptance criterion 4
  (two workers, shipped shutdown path, both close callbacks finish before exit,
  no unbounded wait) is exercised **somewhere** — if it runs neither in the local
  gate nor in the CI matrix it is a check that cannot fail; (3) criterion 2's
  negative control shown RED against a drain that only clears the registry
  without awaiting finalizers; (4) the meaning of residuals R1–R4. This desk
  misclassified a real failure as a flake on 2026-08-18; the label is not taken
  on trust.

## Re-park under OMNIA PAUSA 2026-08-24T14:20Z

State page refreshed **before** `PARKED` was logged (contract requirement; the
08-23 omission is not repeated). Both pause records stand; neither overwrote the
other.

**PR #5388's CI finished during the park: 56/57 green, `Conway Integration
Tests` FAILED** (run 32735463006, 14:06:49Z→14:27:45Z).
**Classification is UNDETERMINED and must not be guessed.** The lane pushed
because its local integration gate could not start, deferring criterion 4's proof
to real CI — so this red is either that deferred proof failing, or the same
environment failure reproducing remotely. Opposite consequences, identical
colour. First thing to demand on release: the log of that exact run and an
evidence-backed classification, not a label. (Precedent: this desk called a real
`ConwayIncompleteWithdrawals` failure a "timing flake" on 2026-08-18 without
reading the log.)

Child `%40` acked at 14:30:22Z: `in_hand=none`, `started_nothing=true`,
`watching=false`, `rerunning=false`, `acting=false`, pane/worktree/branch/handoffs
all preserved, `classification=undetermined`.

**Held, not filed:** the operator-approved Agda→Lean migration epic. Drafts frozen
and hash-verified (`lean-migration-epic.md` `d6ae30ce…`,
`lean-migration-children.md` `8189d432…`). Founding it is new product action and
this pause forbids it. Q-001 asked whether the earlier epic order superseded the
earlier pause; OMNIA PAUSA answers it — **do not file** — so Q-001 is resolved by
this order rather than by an A-file.

## Current position (2026-08-25)

- **#5326 / PR #5388 — progressing, criterion 4 MET.** `%40` rebased
  `68c5457c46` → `df1cdf6dc6` onto `ab51934b7b`; post-rebase gate running.
  The Conway red was classified from the log by the lane *and independently
  re-verified by this desk*: the shutdown smoke passed
  (`acquired=2 closed=2 sigterm=True`, `SIGTERM drain observes two wallet close
  callbacks ✔ 22248ms`); the failure was `SHARED_TRANSACTIONS_LIST_RANGE_03`,
  unrelated by scope. Nobody asserted "flake". Authorized to run to terminal
  `COMPLETE` — diagnosis → correction if needed → verification → review →
  merge → closure. Completing it means completing it, not closing it.
- **Agda→Lean epic FILED:** #5389, children #5390–#5394, sequenced after the
  current queue. Do not start it while #5326 is unfinished.
- **Queue after #5326:** #5334, #5146 → #5108, #5094 → then epic #5389.
- **Not ours:** M6/#118 cardano-node 11.1.0 (raise #5381 overlap to the project
  owner, do not coordinate laterally).

## TERMINAL BOUNDARY — operator: "pause after 5326 merge" (2026-08-25)

**#5326 runs to genuine completion — diagnosis, correction if required,
verification, review, merge, closure — and then the desk PARKS.**

After that merge, start nothing:

- **do not** dispatch #5334 / #5146 / #5108 / #5094;
- epic **#5389 stays filed and undispatched** (children #5390–#5394);
- no new lane, worker, watcher, or monitor.

**This is not pressure to merge.** If the post-rebase gate fails, or criterion 4
turns out not to hold on the rebased head `df1cdf6dc6`, or review raises
something real, then #5326 is *not done* and the boundary has simply not been
reached. An unfinished ticket merged to reach a pause is worse than a pause that
arrives later. `%40` has this in writing and acked
`boundary=after-genuine-completion-not-merge-shortcut`.

Merge authority remains the operator's; PR #5388 is a draft and ordinary review
applies.

**Carry forward:** the rebase invalidates the old green —
`rebased_shutdown_smoke=still-required` on `df1cdf6dc6`. And
`SHARED_TRANSACTIONS_LIST_RANGE_03` is not #5326's to absorb; it is a candidate
standalone ticket for this desk to raise, and `%40` is to report rather than
silently rerun past it.

## PARKED 2026-08-25 — operator: "pause the milestone"

Nothing in flight. No loop, cron, watcher, or monitor — none was ever armed.
No lane running: `%40`'s pane and window are gone, `%38`/`%39` closed too.
Only the session owner `%36` and this desk `%37` remain in `wallet`.

**#5326 COMPLETE.** PR #5388 merged 10:59:45Z, merge commit `0a7332482c`,
parents `ab51934b7b` + audited head `df1cdf6dc6` — audited tree unrewritten.
Issue closed COMPLETED. Burn-down **7/27**. Criterion 4 verified by this desk on
the merged head. Drain confirmed on `master` (positive control run first).

**TWO RESIDUALS — do not lose these on resume:**

1. **Pawel's required review never happened.** #5326's body required it and
   required the ticket to stay open for it; PR #5388 merged with **zero reviews**
   and `reviewDecision=REVIEW_REQUIRED`, and the closing keyword auto-closed the
   issue. Production shutdown semantics changed without the external review the
   ticket demanded. Operator's to decide; this desk flagged it and did not
   reverse it.
2. **`%40` never logged a terminal event.** Last line `AWAITING-EXTERNAL-REVIEW`
   10:53:23Z; pane closed by something other than this desk with no `COMPLETE`.
   Acceptance was done from durable artifacts. Root archived at
   `.archived/t5326-shutdown-drain/`, evidence intact, nothing deleted.

**Stopped where it stood:** #5334/#5146/#5108/#5094 queued and never dispatched;
epic #5389 + children #5390–#5394 filed and never dispatched; #5381 parked with
both blockers public and the pin-set question owned by M6; decisions #5330,
#5115, mithril ancillary probe still open.

**On resume, first three things:** (1) put the Pawel-review residual in front of
the operator before anything else; (2) decide whether #5326 needs reopening or a
follow-up review ticket; (3) then the queue — #5334 next, epic #5389 stays behind
the queue.

## Post-release position (2026-08-25T13:2xZ)

Reconciliation done against live state with a **positive control on its own
method** (probed a known-CLOSED #5326 and known-OPEN #5389 first). Board
confirmed line by line. Only change while parked: **#5395** opened 11:08Z — M6's
epic, not ours, untouched.

**master is RED, and the cause is ours.** `0a7332482c` carries 99 success /
1 failure: `Conway Integration Tests (macOS)`, sole failure = the drain smoke
#5326 added — `user error (cluster start timed out)`, `ShutdownDrain.hs:154:5`,
180159 ms.

**Correction held on the record:** this is *not* a regression. The previous
master commit `ab51934b7b` had that same macOS job at **54 failures**
(`Faucet: no more mnemonics available`) with no drain test present — verified by
positive control (0 `shutdown drain` hits vs 119 `API Specifications` hits).
**54 → 1.** #5326 improved master and left one new failing test. A future reader
must not reconstruct a false regression story from the raw red.

What the evidence supports: the drain logic is **not** shown broken — its success
diagnostic is entirely absent, so the test never reached the drain; its two
negative controls **passed** on macOS, so the check can still fail; only the
smoke's own second `local-cluster` missed a 180 s budget tuned to Linux, where
the same test passes in 22.3 s. **Criterion 4: proven on Linux, unproven on
macOS.**

**#5396 filed** — https://github.com/cardano-foundation/cardano-wallet/issues/5396
— milestone #119, Bug/CI-CD/Tests. Does not reopen #5326 (terminal). Explicit
non-goal: **do not skip the smoke on macOS**; skipping the platform where a
live-boundary proof fails turns criterion 4 into a check that cannot fail there.

**Proposed priority inversion, awaiting the project owner:** dispatch #5396
ahead of the paved queue (#5334 / #5146 / #5108 / #5094), because it is the only
item keeping master red and it protects an acceptance criterion this milestone
just delivered. Recorded with its reason rather than performed silently. Epic
#5389 stays behind the whole queue per the operator ruling.

**No wake source armed.** TaskList/CronList empty; none will be armed until a
lane is dispatched, and the reason will be recorded when it is.
