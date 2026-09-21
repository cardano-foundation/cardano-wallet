# Session `wallet` — cardano-wallet technical-debt milestone #119

# Snapshot 2026-08-24. This file carries NO past: dead windows and completed
# lanes are deliberately absent — they have other owners (STATUS journals, the
# issue/PR trail, git). A stranger with tmux and git rebuilds the CURRENT
# session from this file alone.

# ---------------------------------------------------------------------------
# Window wallet:1 — `orch`, pane %36 — SESSION OWNER (not mine)
# ---------------------------------------------------------------------------
# Technical coordinator for the session; brokers window/pane mechanics.
# Family codex, model gpt-5.6-sol, effort high, cwd /code/infrastructure.
# Runtime root: /tmp/machine/session-restore-20260824/wallet/orch/
# This desk ASKS it for lane mechanics (see asks/ASK-001-lane-t5326.md); it
# does not own this milestone's content.

# ---------------------------------------------------------------------------
# Window wallet:2 — `cardano-wallet-ms119-technical-debt`, pane %37 — THE DESK
# ---------------------------------------------------------------------------
# Singleton milestone owner. One pane, always — a quadrant here would be three
# dead seats advertising work that must never happen at this altitude.
# Family claude, model claude-opus-5[1m], cwd /code/cardano-wallet.
# Runtime root: /tmp/machine/session-restore-20260824/wallet/ms119-desk/
# Parent: cardano-wallet project owner, pane %35, window 0-projects:3.
#
# Launch:
#   claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
#   (the [1m] suffix is part of the approved identifier and the quotes are
#    required — bare [1m] is a shell glob)
#
# Paste after launch:
#   Read /tmp/ms-119/sweep-checkout/.milestones/119/resume/ms.md in full and
#   resume the registered cardano-wallet milestone #119 desk.

# ---------------------------------------------------------------------------
# Window wallet:3 — NOT THIS MILESTONE'S. Do not resurrect from this file.
# ---------------------------------------------------------------------------
# Issue #5350 / PR #5363 (re-delegating to a previously used DRep) is a
# project-direct standalone lane owned by the cardano-wallet project owner
# through its own ticket owner. Ruled 2026-08-24: it is NOT an M119 child.
# This desk does not dispatch into it, supervise it, count it, or document its
# internals. Its resurrection record belongs to the project owner, not here.

# ---------------------------------------------------------------------------
# Window wallet:4 — `cardano-wallet-ms119-t5326-shutdown-drain`, pane %40
# ---------------------------------------------------------------------------
# ACTIVE standalone ticket lane for issue #5326 (graceful shutdown drain of
# wallet database workers). Dispatched 2026-08-24T09:44Z; window brokered by
# the session owner on ASK-001.
# Family codex, model gpt-5.6-sol, effort high.
# Runtime root:
#   /tmp/machine/session-restore-20260824/wallet/ms119-desk/t5326-shutdown-drain/
# Worktree: /code/cardano-wallet-issue-5326
# Branch:   fix/5326-drain-wallet-workers-on-shutdown  (base 346786a112)
#
# Launch (cwd is deliberately the master checkout for bootstrap; the owner
# works in its worktree — see the fence below):
#   codex-raw --dangerously-bypass-approvals-and-sandbox \
#     -C /code/cardano-wallet -c model_reasoning_effort=high
#
# Paste after launch:
#   Read /tmp/machine/session-restore-20260824/wallet/ms119-desk/t5326-shutdown-drain/brief.md
#   in full and resume as the #5326 ticket owner.
#
# Seats for this ticket, derived mechanically (alternate-authoritative-cli):
#   ticket owner codex -> commit owner grok (grok --always-approve -m grok-4.6)
#   -> auditor claude (claude --dangerously-skip-permissions
#      --model 'claude-opus-5[1m]' --effort high)
#   Three distinct families. One grok seat per ticket. agy REVOKED 2026-08-14.
#   qwen draft-only.
#
# TWO STANDING FENCES on this lane, both acknowledged by the owner:
#  1. Bind pane identity from $TMUX_PANE and target every tmux command
#     explicitly. An untargeted `tmux display-message -p '#{pane_id}'` returns
#     the ATTACHED CLIENT's focused pane; on 2026-08-24 that returned %38 (the
#     #5350 lane) and the owner mislabelled itself. Corrected, blast radius nil.
#  2. /code/cardano-wallet is READ-ONLY ground for this lane. It holds
#     .llm/issue-5309-unit-memory-analysis.md — untracked, never committed,
#     unrecoverable from git, and preserved by explicit operator order
#     (sha256 71e6dee8a639fece22712f69cd794c0340d64de17ac8cc11e37ebe049cdfc1d5).
#     No git clean/stash/checkout --/reset --hard/restore, and no cabal|nix
#     clean, with that directory as cwd. The commit-owner brief carries the
#     same fence.
