# cardano-wallet technical-debt cleanup ledger

Home repo: cardano-foundation/cardano-wallet.
GitHub milestone: #119, `M7 — Technical debt cleanup`.
Desk session/window: `wallet` / `cardano-wallet-ms119-technical-debt`, pane `%5355`.
Runtime root: `/tmp/ms-cw-tech-debt`.

Separate from GitHub milestone #113 "Drop cardano-api"; this desk must not
dispatch or mutate cardano-api-removal work.

## Outcome test

Starting set: 27 open, actionable, non-#113 issues as of 2026-07-30. The
original `qwen-audit/report.md` was lost from /tmp; the set was reconstructed
2026-08-19 from GitHub state (issues open at audit time, non-#113, #5293
excluded) and reconstructs to exactly 27:
4411 5063 5075 5086 5094 5097 5098 5103 5106 5108 5114 5115 5146 5147 5155
5159 5170 5182 5196 5233 5246 5252 5325 5326 5330 5334 5341.

Complete when:
1. all four S tickets #5103, #5115, #5196, #5246 closed by merged PRs or
   evidence-backed "not planned" — **3/4 done** (#5115 remains);
2. ≥14 of the 27 closed with merged-PR or evidence-backed disposition —
   **6/27 done** (#5063, #5103, #5196, #5246, #5325, #5341); 8 more needed;
3. all master README CI badges green (operator amendment 2026-08-05) —
   **MET as of 2026-08-18/19** (all 9 badges verified green after #5374,
   #5376, #5378, #5380 merged; Mithril Sync will show green on its next
   scheduled 2am run, fix verified on a real dispatch run).

## Closed under this milestone (beyond the starting set)

#5358, #5359 (2026-08-04/05); #5373/PR#5374, #5375/PR#5376, #5377/PR#5378,
#5379/PR#5380 (2026-08-18/19) — the badge-green drive. All merged by the
operator; all issues auto-closed.

## Priority queue (paved 2026-08-19)

1. **#5326** — shutdown drain. Prepared in new-ticket shape since 08-02, one
   vertical PR, no operator decision pending. NEXT DISPATCH. Pawel review
   required post-delivery (production shutdown semantics).
2. **#5334** — Windows golden-sample mismatch warnings (small, CI/CD).
3. **#5146** — CI docs review after GHA migration (small, docs; feeds the
   release-runbook accuracy this desk already relies on).
4. **#5108** — STAKE_POOLS_SMASH_01 flaky timeout (medium; same
   local-cluster flake family observed repeatedly during the badge drive).
5. **#5094** — local cluster flakiness umbrella (medium; overlaps 4).
6. **#5252** — ProtocolParameters type alignment. Spec-only draft PR #5364
   already exists (operator-authored); implementation undispatched.
7. **#5097/#5098/#5114/#5155/#5159** — test-performance cluster (larger).
8. **#4411, #5086, #5147, #5170, #5182, #5233** — unscoped tail.

## Blocked on operator decision

- **#5330** — macOS/Windows CI only on push:master, no failure alerting.
  Parked since 07-30 pending choice of alerting option.
- **#5115** — last S ticket. Parked under the operator no-new-work order of
  2026-08-03; needs explicit release to dispatch.
- **#5370** — external contribution (Byron key verification, Crypto2099).
  Grok-reviewed "merge as-is" 2026-08-18; fork-PR CI approval + review
  routing (Pawel?) both operator calls. Relates to #5075 but does not close it.
- **Ancillary-verification enforcement** — open question from #5380 review:
  does mithril-client actually reject a bad ANCILLARY_VERIFICATION_KEY or
  warn-and-proceed? Key location resolved (public keys, tracked in
  run/mainnet/nix/.env, sourced by run.sh:20 and snapshot.sh:37). Negative
  control designed but not run; ~1-2 cheap CI probes if pursued.

## Current state

| Item | State |
|---|---|
| Master head | `6761259ee9` (#5380 merge), all 9 README badges green |
| S tickets | #5103 ✅ #5196 ✅ #5246 ✅ #5115 ⛔ parked |
| 14/27 threshold | 6/27 — honest recount 2026-08-19; the badge drive closed new issues, not starting-set ones |
| Open PRs (this desk's scope) | none — all merged; #5364 is an operator spec draft, #5370 external |
| Active lanes | none — all workers archived, windows closed |

## Standing constraints

Operator alone merges. No cardano-api work. Tickets touching user-visible or
production semantics route to Pawel post-delivery. No internal Agent-tool
subagents — tmux/worker-protocol dispatch only (operator directive
2026-08-18). Disk headroom on this host is tight; no speculative large nix
closures. Real CI probes against the shared colo2-nix-public pool are
budgeted per-ticket and capped explicitly in briefs.
