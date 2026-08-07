# Session `wallet` — technical-debt cleanup

# Historical session-owner window
# Window @3590 / pane %5226 no longer exists. Do not resurrect its Qwen
# mechanics context: the milestone desk directly owns the standalone #5325
# ticket lane, and Codex is the only authoritative provider for that ticket.

# Window 1: cardano-wallet-ms119-technical-debt
# Singleton milestone desk, pane %5161, Codex sol, cwd /code.
# Resume from /tmp/ms-cw-tech-debt/resume/ms.md.
# Registered as GitHub milestone #119 and in the shared milestone registry.
# Launch: `codex-raw --dangerously-bypass-approvals-and-sandbox -C /code`
# Paste after launch: `Read
# /tmp/ms-119/sweep-checkout/.milestones/119/resume/ms.md in full and resume
# the registered cardano-wallet milestone #119 desk.`
# Historical window: cardano-wallet-e5304-t5303-randomness-hardening
# Window @3584 and panes %5199-%5202 were intentionally closed on operator
# order at 2026-07-31T16:43:36Z after child acceptance. Runtime
# /tmp/epic-5304, worktree /code/cardano-wallet-issue-5303, branch, and all
# acceptance evidence remain preserved; PR #5343 remains open and queued.
# Live-host fragment: `/tmp/epic-5304/.orch/window-brief.md`.
# Remote fragment: `.milestones/119/resume/e5304.md`.
# If resurrection is explicitly ordered, allocate a fresh Codex owner and
# fresh panes, then deliver that fragment plus `/tmp/epic-5304/brief.md`.
# Stage at closure: epic CHILD-ACCEPTED exact head
# 995e82041d83ecf4471902e2bfd96f336de17dec after independent 17/17 and NC
# exit=1; final CI is 53 green plus sole Cabal index-hash infrastructure red.
# PR is AWAITING-EXTERNAL-REVIEW by Pawel at exact signed head 995e82041d83;
# do not propose it to the operator before that review.

# Window 2: cardano-wallet-ms119-t5325-deposit-returned
# Live standalone ticket owner: Codex pane %5249, window @3591, cwd
# /code/cardano-wallet-issue-5325. Runtime:
# /tmp/ms-cw-tech-debt/broker-machine-wallet-session-004/ticket-5325.
# Launch: `codex-raw --dangerously-bypass-approvals-and-sandbox
# -C /code/cardano-wallet-issue-5325 -m gpt-5.6-sol
# -c model_reasoning_effort=xhigh`
# Resume from `.milestones/119/resume/t5325.md` and the live runtime STATUS.
# Stage: COMPLETE then AWAITING-EXTERNAL-REVIEW by Pawel and PARKED after the
# bounded 2026-08-02 ticket-preparation release. Accepted/pushed
# head 14842f1fbc14, remote PR #5345 open+draft+mergeable, 54/54 CI green,
# six tasks stamped, worktree/index clean. PAIR panes %5259/%5260 ended after
# durable completion; owner pane %5249 and its Codex/MCP context survive.
# Resume only after a new explicit machine-owner release, then reverify exact
# head/worktree/remote PR before any explicitly authorized follow-up.

# Prepared ticket #5326 has no window, pane, lane, branch, worktree or PR.
# It remains parked. Its body requires Pawel review after accepted delivery,
# using AWAITING-EXTERNAL-REVIEW rather than BLOCKED while that review waits.

# Window 2: cardano-wallet-ms119-t5358-local-cabal-tests
# Live standalone Opus ticket owner pane %5298, window @3608, cwd
# /code/cardano-wallet-issue-5358. Runtime /tmp/ms-cw-tech-debt/t5358-owner-002.
# Draft PR #5361 is open from fix/5358-local-cabal-test-targets at planning head
# 86db1f887e. Frozen gate v1 sha256 62d740288ca24ad7abb9079955322b7fe64053c06bd8c4a6f101eb758e2ea8cd.
# PAIR live since 2026-08-03T07:01:18Z: Sol/xhigh driver %5299 and Opus/high
# navigator %5301. Resume from `.milestones/119/resume/t5358.md` and the live
# owner STATUS. Operator alone merges.

# Prepared ticket #5359 has no window, pane, owner, lane, branch, worktree or
# PR. It remains explicitly held during the scoped #5358 release.

# Completed historical lanes are represented in the ledger and STATUS, not
# reconstructed here.
