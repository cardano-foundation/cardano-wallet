# Resume — cardano-wallet technical-debt cleanup desk (#119)

- **State:** ACTIVE. Desk pane `%5355`, window
  `cardano-wallet-ms119-technical-debt`, session `wallet`. No lanes in
  flight; all workers archived under `/tmp/ms-cw-tech-debt/.archived/`.
- **Model note:** desk reseated on Fable 5 (operator /model, 2026-08-19).

## Where things stand (2026-08-19, post badge drive)

- All 9 README badges green at master `6761259ee9`. Outcome-test clause 3 MET.
- S tickets 3/4 (#5115 parked). Burn-down honestly recounted: **6/27**,
  need 8 more. Full reconstruction + paved queue in `ledger.md`.
- #5341 closed as completed-by-#5380. Issues #5373/75/77/79 all fixed,
  merged, closed — but they're new issues, they don't advance the 27-count.

## Next actions

1. Dispatch **#5326** (shutdown drain) — prepared since 08-02, no decision
   pending. tmux/worker-protocol dispatch, codex-raw seat, fresh worktree,
   Pawel review post-delivery.
2. Then the small CI/CD pair **#5334**, **#5146**; then the flake family
   **#5108**/**#5094**.
3. Chase operator decisions: #5330 (alerting option), #5115 (release from
   no-new-work), #5370 (external PR CI approval + review routing),
   ancillary-verification probe (pursue or drop).

## Standing constraints

Operator alone merges. No cardano-api. No Agent-tool subagents — tmux only.
Pawel reviews production-semantics changes. Disk tight on this host. CI
probes against colo2-nix-public capped per brief.
