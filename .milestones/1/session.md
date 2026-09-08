# tmux session layout — cardano-wallet M1 (session `wallet`, base-index 1)

# Window 1: ms1-drop-cardano-api — the milestone desk (CODEX SOL since 2026-07-30 handover; operator request:
# desk sits at position 1). Singleton, one pane. Role skill:
# milestone-orchestrator. cwd /code. Launch: `claude`; then a bare
# /milestone-orchestrator load — it discovers this milestone via
# /code/llm-settings/shared/milestones.md and resumes from
# .milestones/1/resume/ms.md on this branch. Runtime root: /tmp/ms-cw-1.

# Window 2: cardano-wallet-no-epic-t5303-rndstate-reseed — standalone ticket
# lane for #5303 (M4 randomness, PR #5343). PRE-EXISTS the desk (not
# desk-dispatched); its own orchestrator maintains .orch/resume.md in its
# worktree. On resurrection: check the lane's STATUS/resume files rather
# than respawning blind; if dead, respawn a ticket-orchestrator pointed at
# issue #5303 + PR #5343.

# Window 3: cardano-wallet-ms1-audit-backlog — Wave-0 backlog-execution audit
# (claude worker, runtime /tmp/ms-cw-1/audit-backlog, brief.md there). Verifies
# 39 backlog issues against master; desk closes verified-done ones.

# Future desk-dispatched lanes follow: cardano-wallet-ms1-t<NNN>-<goal>
# (standalone tickets) or cardano-wallet-e<NNN>-t<MMM>-<goal> (epic children).
