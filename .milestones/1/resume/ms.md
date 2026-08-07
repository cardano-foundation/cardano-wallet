# Resume brief — cardano-wallet milestone desk (M1 ms1-drop-cardano-api)
# Handover 2026-07-30: desk moves from Claude(Fable) to Codex sol on operator
# order (Claude weekly budget at ceiling). New desk: read this fully, then act.

## Role
You are the milestone-orchestrator for cardano-wallet M1 (GitHub milestone
#113 "Drop cardano-api"). Load the role contract:
/code/llm-settings/shared/skills/milestone-orchestrator/SKILL.md and
tmux-orchestrator. Desk law: only asks, answers, sweeps. NEVER write code,
never investigate failures yourself (forward URLs to lanes), serialize PR
merges (one green proposal at a time), no new tickets while PRs pile up,
every reply to the operator carries bare URLs for remote artifacts.

## Machine state (2026-07-30)
- OMNIA PAUSA + Claude hold in effect: /tmp/machine/pausa/ (CLAUDE-HOLD.md;
  a wallet-scoped release was granted then REVOKED — see
  REVOKE-wallet-2026-07-30.md). NOTHING moves until the machine owner posts
  a release. You are Codex-backed, so a Codex-scoped release may free you
  first; do not infer releases — read the directory.
- Ack file to maintain: /tmp/machine/pausa/wallet.md (append-style).

## Immediate state
- Proposed to operator, all verified 58/58 green: merge PR 5349 (#5103),
  PR 5353 (#5352 badge), PR 5343 (#5303, M4). Do NOT merge PR 5345 — empty
  scaffold (no implementation; would wrongly close #5325). Decision pending
  with operator: close it or keep as draft.
- Lanes (both PARKED, resumable, Codex TOs): t5088 pane %4664 runtime
  /tmp/ms-cw-1/t5088 (queue after merges: #5196, #5246); t5242 pane %4665
  runtime /tmp/ms-cw-1/t5242 (no queue). Wake protocol: write
  inbox/NOTE-*.md then paste a one-line pointer + Enter; verify RESUMED.
  Pair teams (when allowed): driver codex medium, navigator
  claude-opus-5[1m] high — Fable NEVER in worker lanes; but no Claude panes
  at all while the Claude hold stands.
- Post-merge ceremony per merged PR: lane removes worktree, deletes
  branches, prunes, rebases base clone; then desk feeds next queue ticket
  (subject to the pause and the serialize rule).

## Standing facts
- gate.sh is gitignored on master, never committed (operator law; skill
  updated). Lanes keep local untracked gates + /tmp backups.
- Flake trackers filed today/yesterday: #5347, #5351, #5354; #2886
  recurrence commented. macOS integration runs on push:master (proof run
  30464793708) with README badge PR 5353 pending.
- Milestone map M1-M6 = GitHub milestones #113-#118 (see ledger.md);
  priority: M6 Dijkstra deadline-driven, M1 active (operator PR #5236),
  registry contracts c1-c3 in registry.md.
- Ledger sweep: /code/llm-settings/shared/skills/milestone-orchestrator/scripts/ledger-sweep.sh
  with MS_SWEEP_ROOT=/tmp/ms-cw-1 (N collides with amaru otherwise); push
  falls back to HTTPS (SSH key intermittently absent). Sweep on every
  transition; force-push fresh root commit.
- Arm a Monitor: tail -n 0 -F each /tmp/ms-cw-1/*/STATUS.md + mtime STALE
  watchdog (10 min); timestamp-filter old lines (lanes rewrite files).
- Registry of milestones: /code/llm-settings/shared/milestones.md.

## Outcome test (never forget it)
M1 closes when cardano-api is absent from the build closure of all
cardano-wallet packages with green build+tests — audit against that, not
against closed-ticket counts.
