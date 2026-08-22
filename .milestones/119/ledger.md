# cardano-wallet technical-debt cleanup ledger

Home repo: cardano-foundation/cardano-wallet.
GitHub milestone: #119, `M7 — Technical debt cleanup`.
Desk: session `wallet`, window `cardano-wallet-ms119-technical-debt`, pane `%5355`.
Runtime root: `/tmp/ms-cw-tech-debt` (completed lanes under `.archived/`).

Separate from milestone #113 "Drop cardano-api" — out of scope for this desk.

## Product milestone event

**Release v2026-08-21 shipped** (published 2026-08-22T08:01Z, tag at
`be7898f4`) — the release the badge-green drive prepared. Carries all four
badge fixes (#5374, #5376, #5378, #5380) and ships cardano-addresses 4.0.2
per the deferral ruling.

## Outcome test

Starting set (reconstructed 2026-08-19, exactly 27): 4411 5063 5075 5086
5094 5097 5098 5103 5106 5108 5114 5115 5146 5147 5155 5159 5170 5182 5196
5233 5246 5252 5325 5326 5330 5334 5341.

1. S tickets: **3/4** (#5103, #5196, #5246; #5115 parked).
2. Burn-down: **6/27** (#5063, #5103, #5196, #5246, #5325, #5341) — 8 more
   needed.
3. CI badges green: **MET 2026-08-18/19**, and now embodied in the shipped
   release.

## The #5381 saga (cardano-addresses 4.0.8) — closed line of inquiry

Operator asked for the bump pre-release. Investigation (archived lanes
`t5381-cardano-addresses-bump`, `t5381-crypton-relax-upstream`):

- 4.0.8 needs `crypton >= 1.1`; wallet's Node-11.0.1 pin
  `cardano-crypto-class ==2.3.3.0` caps crypton `< 1.1`. Not a pin-only bump.
- Operator ruled defer; release shipped on 4.0.2.
- Upstream relaxation to `>= 1.0` (operator asked Pawel; desk sent a grok
  lane to deliver it) **empirically disproven**: the bound is welded to the
  memory→ram migration (`8b3978a6`); constrained crypton==1.0.6 build fails
  with 7 GHC-39999 ByteArrayAccess errors; no crypton 1.0.7 exists. No PR
  opened — honesty clause held. Evidence: archived lane handoffs/ logs +
  local branch `relax-crypton-1.0` in /code/cardano-addresses (unpushed).
- **#5381 wait-state: blocked on a cardano-crypto-class ecosystem bump**,
  most naturally with the next node pin-set advance. The minimal pin pair
  for when it unblocks is recorded on the issue.
- Operator to tell Pawel the verbal relaxation ask is unsatisfiable.

## Post-release automation line (new, 2026-08-22)

- **#5387 / PR #5384** — manual merge-back of release-candidate/v2026-08-21
  into master. In flight in the operator-driven lane (window
  `cardano-wallet-no-epic-t5385-auto-mergeback-pr`, codex pane %6754, with a
  grok assistant seat). Snag: `Validate PR Body Closing Link` is now
  REQUIRED on master and reruns reuse stale event payloads (same bug class
  as #5376) — needs a fresh PR event (empty-commit retrigger offered,
  operator hasn't said go).
- **#5385 / PR #5386** — automate that merge-back henceforth. Spec-first PR
  (converted to draft by desk after it was opened ready by mistake);
  acceptance criteria absorbing lessons from the manual walk before
  implementation. Attached to milestone #119 (operator: "it's tech debt").

## Priority queue (standing, from the 2026-08-19 paving)

1. **#5326** shutdown drain — prepared, no decisions pending, next dispatch.
2. #5334, #5146 (small CI/CD+docs) → #5108, #5094 (flake family).
3. #5252 (spec draft PR #5364 exists) → test-perf cluster
   (#5097/#5098/#5114/#5155/#5159) → tail (#4411, #5086, #5147, #5170,
   #5182, #5233).

## Blocked on operator decision

- #5330 — alerting option (parked since 07-30).
- #5115 — last S ticket, needs release from no-new-work order.
- #5370 — external contribution: fork-CI approval + review routing.
- #5384 — empty-commit retrigger authorization.
- Ancillary-verification probe (from #5380 review) — pursue or drop.

## Standing constraints

Operator alone merges. No cardano-api work. Pawel reviews
production-semantics changes. tmux/worker-protocol dispatch only — no
Agent-tool subagents. Disk tight; no speculative large closures; builds
check the ~40GiB floor. Shared-pool CI probes capped per brief. grok-4.6 the
preferred implementation seat (operator), codex for supervision; both
weeklies were near-dry 08-19, grok reset by 08-22.
