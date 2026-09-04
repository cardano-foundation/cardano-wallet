> **SUPERSEDED 2026-08-28 — read as history only.**
>
> This directory is the record of a 2026-07-29/30 desk that carried the `ms1`
> name but acted at **project altitude**: an all-milestone priority table, a
> repo-wide backlog drive, and a serial merge queue. That role now belongs to
> the Cardano Wallet **project owner** (`%107`).
>
> The live M1 ledger is **`.milestones/113/`**, under the current
> `.milestones/<GitHub milestone number>` convention (cf. `118`, `119`).
>
> Everything below is dead as operational state: `/tmp/ms-cw-1/` and every
> runtime root under it, the lanes `t5088`/`t5242`, the pane IDs, the merge
> queue, the OMNIA PAUSA of 2026-07-29, and the priority table. Contracts
> c1/c2/c3 were carried forward into `.milestones/113/registry.md`.

# cardano-wallet — Milestone 1 ledger (desk: ms1-drop-cardano-api)

Home repo: cardano-foundation/cardano-wallet. Desk session: tmux `wallet`,
window `ms1-drop-cardano-api`. Runtime root: /tmp/ms-cw-1.

## Active desk milestone

M1 — Drop cardano-api (GitHub milestone #113)
https://github.com/cardano-foundation/cardano-wallet/milestone/113
Outcome test: cardano-api absent from the build closure of all
cardano-wallet packages, build and test suites green.
Issues: #5237 (anchor), #5243, #5288, #5289, #5290, #5241. PR #5236 in
flight (SealedTx rewritten, mkLedgerTx added, wiring next) — operator-owned.

## Milestone map (all GitHub milestones on the repo)

| GH# | Name | Due | State |
|-----|------|-----|-------|
| 113 | M1 — Drop cardano-api | — | active desk milestone; PR #5236 in flight |
| 114 | M2 — Node 11.0.1 readiness | 2026-08-31 | CORE SHIPPED (PRs #5277/#5280/#5332; #5275 closed 2026-07-29 after audit). Residue: #5293 cardano-addresses bump only |
| 115 | M3 — CI trust | — | backlog pile: #5330 anchor + 11 more |
| 116 | M4 — Randomness hardening | — | PRs #5343, #5327 open; epic #5304 |
| 117 | M5 — Benchmarks tell the truth | — | #5178 is a release blocker |
| 118 | M6 — Dijkstra HF readiness | 2026-11-30 (provisional) | HF not scheduled yet; Intersect targets before end 2026. #5209 |
| 112 | Release 10.6.2 | — | stale: 0 open / 4 closed — propose closing |

## Priority order (cross-milestone, with reasons)

1. M6 (#118) — the deadline milestone (Dijkstra mainnet HF, expected before end
   2026; not yet scheduled). Start the upstream cascade EARLY: balance-transaction,
   ledger-read, coin-selection will need Dijkstra-era ledger bumps on main before
   the wallet re-pins (registry c3) — same pattern as the 11.0.1 bump. #5209.
2. M1 (#113) — active desk milestone, operator PR #5236 in flight (registry c1).
3. M5 (#117) — contains release blocker #5178.
4. M4 (#116) — two PRs already open, cheap to land.
5. M3 (#115) — important but no external clock.
6. M2 (#114) residue — #5293 cardano-addresses, small quick win anytime.
## Parked decisions (operator)

- Dispatch the M6 Dijkstra-readiness lane now? Its upstream cascade (registry c3, same 4 repos as the 11.0.1 bump) argues for an epic-orchestrator started early, ahead of the node-12 release. Desk recommends yes, low intensity until Intersect announces the Preview fork.
- Close stale GitHub milestone #112 (Release 10.6.2)? Release-team territory;
  desk recommends closing.
- M1 vs M2 sequencing at the tx-layer seam (registry c1): which arc rebases on
  the other. Needs arbitration before both have lanes.

## Dijkstra dates (evidence, 2026-07-29)

- WALLET STATUS: already ships 11.0.1 on master (verified 2026-07-29).
  enacted ~2026-07-20 (submitted 2026-06-16, epoch 637).
- Dijkstra era (Leios): not yet scheduled; Intersect targets before end of
  2026. Leios public testnet (Musashi Dojo) live since 2026-06-23.
- Watch: https://intersectmbo.org/news and IntersectMBO governance actions.

## Backlog-execution drive (operator directive 2026-07-29)

Operator wants open tickets drastically reduced BY EXECUTION (project is
low-maintenance; ~49 open is uncomfortable). Waves:
- Wave 0 (RUNNING): audit-backlog lane verifies 39 non-active issues against
  master; desk closes verified-done/obsolete with evidence (precedent #5275).
- Wave 1: quick chores (audit-sized S) as short ticket lanes, a few in parallel.
- Wave 2: M3 CI-trust survivors + M5 bench cluster (#5178 release blocker).
- Wave 3: user-facing bugs (#5325, #5326, #5075).
Active arcs excluded from audit: M1 set, M4 set, #5209, #5178.

## Backlog drive state (2026-07-29, post-audit)

- Audit lane COMPLETE: 39 audited (full table: .milestones/1/audit-2026-07-29.md).
  Closed 7 with evidence: 5101/5202/5255 completed, 5083/5120/5135/5297 obsolete.
  Open issues: 49 -> 42.
- Wave 1 PARKED-AT-PR: both lanes pushed and await OPERATOR merges — PR 5344 MERGED 2026-07-29 (#5242 closed); t5242 lane now on #5126 (then #5115); PR 5346 MERGED 2026-07-29 (#5088 closed; flake #5347 filed en route); t5088 lane now on #5103 (then #5196, #5246) behind a GPG precheck; runtimes under /tmp/ms-cw-1/.parked/; queues frozen. Also PR 5343 review fixes pushed (qwen worker, verified, archived). Original wave plan: — S-sized tickets, two standalone-ticket lanes (Codex TO
  effort high; pair = Codex driver medium + Opus5 navigator high; no Fable in
  execution lanes — operator law):
  - lane t5088 (window cardano-wallet-ms1-t5088-drop-cli-import): #5088, then
    queue #5103, #5196, #5246 (OPERATOR merges Wave-1 PRs personally; desk only reports PR URLs and feeds the queue after the operator merge).
  - lane t5242 (window cardano-wallet-ms1-t5242-nix-eval-cache): #5242, then
    queue #5126, #5115.
- Parked decisions from audit (operator):
  - #5330 needs an option choice (failure alerting: which of the issue's 3 options).
  - #5341 timeout-bump vs root-cause (nightly Mithril sync cancelled 3 nights running).
  - #5241 refactor vs wont-fix (SealedTx record; interacts with M1 c1 seam).
  - #5293 blocked upstream: cardano-addresses 4.0.6 not on CHaP — needs upstream publish.
- Flake filed by t5088: #5347 (WALLET_RESTORE_0.4 tip/funding race) — M3 candidate. Wave 2 candidates (M after Wave 1): 5063 5075 5086 5097 5098 5106 5108 5170
  5182 5252 5325 5326 5334 5146 5147 5155. Wave 3 (L): 4411 5094 5114 5159 5233.

## HOST BLOCKER (2026-07-29 10:37Z): GPG signing broken machine-wide

gpg-agent misdirects pinentry to /dev/pts/0; no lane can sign. ALL workers
parked gpg-blocked (runtimes in /tmp/ms-cw-1/.parked/):
- t5242: #5126 spec commit pending (Q-003 trail).
- t5088: parked at precheck before #5103.
- pr5327-review: PR 5327 rebased + review addressed; local unsigned WIP
  9b9e06ad72, push withheld pending re-sign (resume steps in its A-003).
Unblock = operator completes/cancels the /dev/pts/0 prompt or restarts
gpg-agent with a working pinentry, then desk unparks all three with resume
notes. Also noted: SSH key auth intermittently unavailable (ledger pushes
fall back to HTTPS); keyring contains placeholder-identity key 35E60442501672D2.

## Serial merge queue state (2026-07-29 14:10Z)

Operator law: one green PR proposed at a time; no new tickets while PRs pile.
MERGED today: 5344 (#5242), 5346 (#5088, flake #5347 filed), 5327 (#5322
closed — M4 milestone issue done; gitignore-gate convention landed on master).
Queue: 1) PR 5348 (#5126) — rebasing onto master post-5327, then proposal;
2) PR 5349 (#5103) — slice landed, finalization; 3) PR 5343 — 1 CI failure to
triage; 4) PR 5345 (#5325, provenance unclear — ask operator) — 1 failure.
Flake issues filed: #5347, #5351. Open issues now 39.

## PAUSED (2026-07-29, OMNIA PAUSA machine-wide)

Desk and both lanes parked at safe points. Pending operator merges: PR 5349
(#5103), PR 5353 (badge #5352). Queue after release: triage PR 5343 failure,
then PR 5345 (provenance unconfirmed). Lanes wake only on desk RELEASE note.
