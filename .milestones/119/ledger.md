# cardano-wallet technical-debt cleanup ledger

Home repo: cardano-foundation/cardano-wallet.
GitHub milestone: #119, `M7 — Technical debt cleanup`.
Desk session/window: `wallet` / `cardano-wallet-ms119-technical-debt`.
Desk pane: `%5161`. Runtime root: `/tmp/ms-cw-tech-debt`.

This is an operator-defined cleanup drive. It is separate from GitHub
milestone #113, “M1 — Drop cardano-api”; this desk must not dispatch or mutate
cardano-api-removal work.

## Outcome test

Starting set: the 27 open, actionable, non-#113 issues reconciled in
`qwen-audit/report.md` on 2026-07-30 (the upstream-blocked #5293 is excluded).

Complete when:

1. all four S tickets #5103, #5115, #5196 and #5246 are closed by merged PRs
   or evidence-backed “not planned” decisions; and
2. at least 14 of the 27 starting actionable issues are closed on GitHub with
   merged-PR or evidence-backed disposition.

3. **all master README CI badges are green** at milestone close — operator
   amendment, 2026-08-05. Audited against the actual workflow-status API
   (`gh run list --branch master`), not the shields.io cache. As of
   2026-08-05T10:15Z two are red on head `d3d170d0`, both pre-existing
   flakiness, not caused by any #119 merge:
   - `Windows` — `wallet-test-utils` diagnostic-timeout test, `UnliftIO`
     EOF; identical failure reproduced on the two prior master heads too
     (`ee8def32be`, `ecf489fad679`), before #5343/#5361/#5362 landed.
   - `macOS Integration Tests` — 8 Conway stake-pool timing failures
     (metadata-fetch / non-myopic-reward 90s waits). First red at this
     head; the only thing that just merged (#5362) touches only
     `scripts/ci/*`/`specs/5359-*`, nothing near stake pools.
   Neither issue is filed yet. Wallet is currently parked under
   `OMNIA-PAUSA-2026-08-04` (machine-wide; `RELEASE-CLAUDE-HOLD.md` at
   2026-08-05T08:25Z released only the Claude-provider hold and named
   wallet as still parked) — no ticket dispatch until machine-owner
   release.

The operator ratified this as the separate Technical Debt Cleanup milestone
on 2026-07-31. GitHub milestone #113 remains separate and out of scope.

## Priority

1. Completed merge train: PRs #5349, #5355, #5356 and #5357 were merged by
   the operator; completed windows/worktrees were cleaned, with branch refs
   retained.
2. #5303 / PR #5343 — `AWAITING-EXTERNAL-REVIEW` by Pawel at accepted exact
   head `995e82041d83…`. Live readback on 2026-08-02 found 54/54 checks green,
   zero commits behind master, and `MERGEABLE`; it must not be proposed to the
   operator before Pawel reviews it. Its execution window was intentionally
   closed after acceptance, with all recovery artifacts preserved.
3. #5325 / PR #5345 — DELIVERY COMPLETE at accepted/pushed head
   `14842f1fbc14…`; 54/54 CI checks green, semantic gate lifecycle accepted,
   six tasks stamped, and worktree clean. The issue is now attached to milestone
   #119 and is `AWAITING-EXTERNAL-REVIEW` by Pawel because it changes user-visible
   `deposit_returned` semantics. The PR remains draft behind #5343 and cannot
   become the merge proposal until the prior queue entry is resolved.
4. #5358 — scoped 2026-08-03 release active. Draft PR #5361 is open at planning
   head `86db1f887e…`; Opus owner `%5298` supervises Sol/xhigh driver `%5299`
   and Opus/high navigator `%5301` under frozen gate v1. #5359 remains prepared
   and explicitly held with no lane or PR.
5. #5326 — PREPARED and attached to milestone #119 as one bounded shutdown-drain
   ticket. No implementation, dispatch, lane, branch or PR exists. Pawel review
   is required after any delivery because it changes production shutdown and
   wallet-worker lifecycle semantics; the eventual wait state is
   `AWAITING-EXTERNAL-REVIEW`, not `BLOCKED`.
6. #5246 — PR #5356 merged but the issue remains OPEN; owner-lane closeout is
   required for the milestone's S-ticket outcome.
7. #5115 — OPEN and parked under the operator's no-new-work order.

## Merge queue mode

Operator order 2026-07-30 remains: queue only exact rebased, semantically
accepted heads, one proposal at a time, and only the operator merges. The
original four-entry queue is merged. PR #5343 is not an operator proposal: the
operator corrected the desk that it is Pawel's review item. It remains
`AWAITING-EXTERNAL-REVIEW` despite being exact-head green and mergeable.

The bounded `RELEASE-2026-08-02-wallet.md` exception prepared exactly two
existing issues, #5325 and #5326, in `new-ticket` shape and attached them to
milestone #119. It created no issue, PR, lane, branch or implementation. Both
candidates are one reviewable vertical slice, so neither needed epic escalation
or a split between semantic core and mechanical remainder.

PR #5345 delivery is complete but remains a draft, not a merge proposal.
It is parked behind #5343 and awaits Pawel's external semantic review at its
exact accepted head. The earlier pause after preparing those two tickets was
later superseded only for the operator-scoped #5358 delivery; only the operator
may merge.

The second bounded release, `RELEASE-2026-08-02b-wallet.md`, created and
recorded exactly two additional non-Pawel issues after the operator approved
their complete drafts:

- #5358 — focused Cabal recipes must select only locally testable suites;
- #5359 — issue-backed PRs must carry GitHub closing-keyword references.

Both are on milestone #119 and the Planning backlog with Category=Wallet,
Ownership=Work, and Status unset. That release itself prohibited
implementation. On 2026-08-03 a later scoped release authorized #5358 alone;
#5359 remains held and undispatched.

## Current state

| Item | State | Owner/runtime |
|---|---|---|
| Grok boundary audit | COMPLETE | `/tmp/ms-cw-tech-debt/grok-audit` |
| Qwen inventory audit | COMPLETE | `/tmp/ms-cw-tech-debt/qwen-audit` |
| PR #5349 / issue #5103 | MERGED by operator; merge commit `320b1abc174ba406d474b006e719204626a903fd` | legacy lane remains parked |
| #5196 / PR #5355 | MERGED by operator at `efe473937feea6260cfb5e64cb6d0a9d1655aab0`. | completed lane removed; durable runtime `/tmp/ms-cw-tech-debt/t5196` |
| #5246 / PR #5356 | MERGED by operator at `ac390d1ae4d207e2e1d22d9623dec9f536e937b5`; issue remains OPEN. | completed lane removed; durable runtime `/tmp/ms-cw-tech-debt/t5246` |
| #5063 / PR #5357 | MERGED by operator at `75cd99bd1754cbda5e255d4bd38d0c6c7bc65c13`. Master protection repaired first by deleting only the four obsolete `delta-*` required contexts; all 53 remaining required contexts were emitted and green. | completed lane removed; durable runtime `/tmp/ms-cw-tech-debt/t5063` |
| #5303 / PR #5343 | MERGED by operator at `ee8def32be4b689875d0883d5e5bb7b17985d650` (2026-08-05T08:52:40Z), after rebase-onto-master and Pawel review. Issue #5303 CLOSED. | lane already retired; runtime `/tmp/epic-5304` preserved historically. |
| #5325 / PR #5345 | MERGED by operator at `00d1dde353f15c1ea8f35d89bad781fe167860c8` (2026-08-04T13:03Z), after Pawel approval. Issue #5325 CLOSED. | lane already retired. |
| #5326 | PREPARED in `new-ticket` shape and attached to milestone #119. Acceptance covers idempotent registry drain, finalizer completion, all wallet API layers, a bounded shutdown smoke, and preservation of deletion and signal semantics. Required post-delivery reviewer: Pawel; eventual durable wait state `AWAITING-EXTERNAL-REVIEW`. | PARKED without implementation, dispatch, lane, branch or PR. One vertical PR is sufficient; no epic escalation or split is needed. |
| #5358 / PR #5361 | MERGED by operator (2026-08-04T11:32:45Z), 54/54 checks green. Issue #5358 CLOSED. | lane already retired. |
| #5359 / PR #5362 | MERGED by operator at `7aa34da8939e70e69305b15f7e9ace629ee5af58` (2026-08-05T09:50:51Z), 57/57 checks green. Issue #5359 CLOSED. | lane already retired. |
| Windows-flake ticket (unfiled) | Not yet a GitHub issue. `wallet-test-utils` diagnostic-timeout test throws `UnliftIO` EOF on Windows CI, reproduced on 3 consecutive master heads. In scope only because of the 2026-08-05 CI-green outcome-test amendment. | Blocked on machine-owner release of wallet from `OMNIA-PAUSA-2026-08-04`. |
| macOS-Integration-flake ticket (unfiled) | Not yet a GitHub issue. 8 Conway stake-pool timing failures (metadata-fetch / non-myopic-reward 90s waits) on macOS Integration Tests, first red at head `d3d170d0`. In scope only because of the 2026-08-05 CI-green outcome-test amendment. | Blocked on machine-owner release of wallet from `OMNIA-PAUSA-2026-08-04`. |

GitHub issue readback lists #5325, #5326, #5358 and #5359 on milestone
`M7 — Technical debt cleanup`. The milestone REST object's aggregate counters
still report 0 open / 0 closed; treat those counters as stale and use issue
readback until GitHub recomputes them.

## Parked decisions

- The harness defect found by #5063 is #5358 and is active under the scoped
  2026-08-03 release as draft PR #5361.
- #5330: choose its failure-alerting option before dispatch.
- #5341: choose timeout increase versus root-cause work before dispatch.
- CI-green amendment (2026-08-05): Windows flake and macOS Integration flake both need root-caused/fixed before milestone close. Prepare as two standalone tickets once wallet is released from OMNIA-PAUSA-2026-08-04; do not dispatch under the pause.
