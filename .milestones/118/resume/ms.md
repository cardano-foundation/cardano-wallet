# Resume brief — M6 (#118) milestone owner

You own `cardano-foundation/cardano-wallet` milestone **#118 — M6 — Dijkstra
HF readiness**. **Continue; do not restart.**

**Load the whole chain before acting**, not just your altitude file:
`workflow` → `orchestrator-contract` → `milestone-orchestrator` →
`worker-protocol` → `tmux-orchestrator` → `context-compiler`, then `debrief`
for the story register. An altitude role sits still while the rules governing
supervision move underneath it. Check what moved:

```sh
git -C /code/llm-settings log --oneline --since=2026-09-21 -- shared/skills/
```

**Last reload: 2026-09-21**, at `c1665a4` *"block orchestration until the
operator agrees the team"* and `9c01138` *"leave child model choices to
operator negotiation"*. Those two are live and they bind you: **team
negotiation is a hard prerequisite that outranks every staffing default in
every skill.** An agent-written proposal, a family list, capacity, silence or
green checks **is not approval**.

Then read `ledger.md`, `registry.md`, `session.md`.

## Status: ACTIVE, released, two lanes live

`RELEASE-2026-09-21T1518Z-wallet-lane.md` — **wallet session only**, 10 seats,
7 windows. Supersedes the 09-16 omnia pausa **for this session** and lifts no
narrower hold. `origin/master` = `92b57304dfb556d80edf30d75e3f19218e66e891`.
This desk holds **no worktree**; that ref is its state.

## The five things most easily got wrong

**1. A zero census does not mean the wallet is ready.** Its population is code
that *announces* it is unimplemented. `guardsTxBodyL` announces nothing;
`readTxFromBytes` (#5410) announces nothing. **A stub fails loudly; those
succeed wrongly.** Never report the ratchet as the outcome.

**2. `/tmp` is not durable and archiving is not preservation.** The sweep of
2026-09-21 ate three archived roots entirely and four survivors' journals.
**Write evidence into `STATUS.md` at acceptance time** — that is the only
reason the #5413 and #5421 findings still exist. **Verify a file before citing
it**; a missing file read as an absent finding is this project's most repeated
mistake.

**3. Check a composer byte-wise before concluding a seat is idle.** The prompt
marker carries a non-breaking space, so `grep '^❯ '` reports a full composer as
empty. Four stranded operator instructions were found this way on 2026-09-21,
one of which had parked a lane for over half an hour. Use
`capture-pane -p | cat -A`.

**4. You are an ask, an answer, or a sweep — nothing else.** Filing an issue,
cutting a worktree, writing a brief into a lane, running a merge: all of it is
the worker's bootstrap from your one ask. **The single exception exercised on
2026-09-21** was a tmux `move-window` performed directly *because the
session-owner seat is vacant* (`wallet:1` is a bare bash) — there was nobody to
route it to, and the journal says so explicitly rather than quietly widening
the role.

**5. Do not reopen, comment on, or touch #5416, #5435 or #5444.** #5416 is
closed and two independent measurements disagree with the close; the burden is
with whoever closed it and it is routed to the operator. Reversing another
party's decision is outward and is not yours.

## Your immediate next actions

1. **Arm one `wait-channels` over your own root** — it covers `answers/`,
   `inbox/` **and** both lanes' journals in a single wait. Never a children-only
   watch; that is how a desk sleeps through its own parent's correction.
   ```sh
   /code/llm-settings/shared/skills/worker-protocol/scripts/wait-channels \
     /tmp/projects/cardano-wallet/ms6-dijkstra 590
   ```
   An elapsed wait is **not** an event and never ends the turn: re-arm, or park
   on purpose and say so.
2. **`t-delegation-certs` (`%936`) owes you the `pendingWith` attribution split
   before it cuts its gate.** Four suppressions in `TransactionLedgerSpec.hs`;
   the ones covering its own sites are not a scope extension, they are the other
   half of the same site. That split decides `DIJKSTRA_STUB_MAX`.
3. **`t5410` (`%1880`) owes you a draft PR and an issue link.**
4. **The wiki is stale.** The register predates the 39→32 census move, #5410,
   the #5416 close and both current lanes. Owed at the next sweep — and a desk
   may not report a transition reconciled while its wiki is stale.
5. **`shared/milestones.md` line for #113 is stale** — that milestone moved to
   session `wallet-ms113`. It is M1's line to fix; if it is still wrong when
   you next sweep, say so upward rather than editing another owner's row.

## Standing obligations

- Rewrite this ledger **at every accepted landing and at the end of every desk
  turn.** Force-push a fresh root commit, depth 1, siblings `.milestones/1,113,119`
  preserved. **No history section** — the past has other owners.
- **Acceptance is an outcome audit, not epic counting.** The milestone closes
  against the observable test with the silent-failure caveat attached, never
  against a burn-down.
- `enforced: NONE` in `registry.md` is a scheduled incident. Either commission
  a check or record explicitly why it stays unenforced. Never leave it implicit.
