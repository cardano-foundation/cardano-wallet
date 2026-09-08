# Resume brief — M1 (#113) milestone owner

You are the **milestone owner** for `M1 — Drop cardano-api` (#113) in
`cardano-foundation/cardano-wallet`. Parent: Cardano Wallet **project owner**,
pane `%107`, `0-projects:cardano-wallet`,
root `/tmp/projects/cardano-wallet/owner/`. **Escalate only there** — not the
machine owner, not a session owner, and **never** the M6 desk.

Continue; do not restart.

## How to reach the parent — the brief omitted this once and cost 27 minutes

**Durable first, then ping. Both, every time.**

1. Write the artifact in your own root and append the `STATUS.md` event.
2. Then wake `%107`:

```sh
/code/llm-settings/shared/skills/tmux-orchestrator/scripts/send-pointer \
  %107 <absolute-artifact-path> \
  /tmp/projects/cardano-wallet/owner/STATUS.md '<ack-regex>'
```

Match the ack regex to **what the parent will actually write** — the question id
(`Q-001`), never an answer id it has not assigned yet. A `send-pointer` timeout
is **not** evidence of non-delivery: check `%107`'s `STATUS.md` and pane first.

**The parent has no monitor armed and has said so.** Never rely on one. Your
pane is a liveness transport, not a control record — prose in the pane is not a
report, and the only reader of a pane is whoever happens to be looking.

## When to wake `%107` — and when not to (NOTE-003, 2026-08-28)

**Wake it** — `send-pointer` plus a `BLOCKED` or a named report — only when:
blocked and unable to proceed; a decision crosses a fence (M6, another
milestone, the operator); you found something that **changes a ruling already
made**; a durability or safety exposure; an artifact needs review to pass a gate.

**Do not wake it** for: a fix it explicitly called non-gating; a confirmation
that something already ruled was applied; a revision that changes no decision.
Write the STATUS event and carry on — **`%107` reads this root at the start of
every turn it takes**, so a `NOTE` reaches it without interrupting.

> **The test: would a reasonable parent do something differently on reading
> this? If not, it is a journal entry.**

This exists because the desk sent three pointers on one file on its first day,
the last two a consistency fix and a citation format. Both changes were right;
neither needed a wake-up.

## Read, in this order

1. `/tmp/projects/cardano-wallet/ms1-drop-cardano-api/brief.md` — in full,
   including all amendments through 2026-08-28T11:55Z. It is current.
2. This branch: `.milestones/113/ledger.md`, `registry.md`, `state.md`,
   `session.md`.
3. `STATUS.md` — the journal.
4. `handoffs/`: `TICKET-DRAFT-closure-instrument.md`,
   `TICKET-DRAFT-deprecations-to-ledger.md`,
   `REPORT-001-sequencing-and-intake.md`,
   `FINDING-002-row2-landing-order-deadlocks.md`.
5. `answers/A-001` and `inbox/NOTE-001` — both read and applied.

If `/tmp` is gone, `.milestones/113/` is the whole surviving record. Rebuild the
runtime root with `init-worker`, reconstruct `brief.md` from this ledger's
Fences / Outcome / Decisions / Owed-units sections, and tell `%107` it was
reconstructed.

## Stage as of 2026-08-28T13:45Z

Desk has **no lanes, deliberately**. The instrument ticket has passed project
review and is **in front of the operator**. `%107`: *"Do not start a lane. When
the operator clears it, I will tell you."* That is now the only thing gating M1.

Both ratchet rulings are resolved; nothing technical blocks the milestone.
`MAX = 7` is confirmed at `origin/mastera7332482c` with two positive and two
negative controls and a blob comparison against `346786a112`; `#5399` declares
`MAX 7 -> 12`, `N = 5`, confirmed branch-side by M6 and held by `%107`.

Desk is one day old, **still has no lanes**, and that is correct: the standing
gate (D-3) is draft → project review → operator review → *then* a branch and a
lane. Both tickets are drafted and held.

Done: `START`; skill chain; fresh intake (parent's table confirmed on every row,
three corrections found, a predecessor desk found); sequencing decided (D-2);
Q-001 filed, ruled, `RESUMED`; `NOTE-001` read and its reporting mechanism
adopted; #5241 ruled; ledger published and re-swept.

## Exact next actions

**0. Stay idle unless woken.** Both tickets are at the operator gate; that queue
is the bottleneck, not this desk. No artifacts, no revisions, no pings. Idle is
the correct state for a desk whose next move is somebody else's decision.


1. **Wait on `%107`** for the operator clearing the instrument ticket, review of
   `TICKET-DRAFT-deprecations-to-ledger.md`, and P-3 / P-4 / P-5.
   review of `TICKET-DRAFT-deprecations-to-ledger.md`, the `FINDING-002` clause
   reaching M6, and P-3 / P-4 / P-5.
2. **File nothing outward.** Not the two tickets, not #5241's closure, not the
   description or state page. All are held deliberately.
3. **On review approval**, the instrument becomes the first lane. It is **one
   ask**: compile the packet through `context-compiler`, fold it into a
   `worker-protocol` brief, and **ask `%107` for the window** — this desk does
   not create windows or panes, and the `wallet` session has no session owner.
4. **Do not let the lane substitute an easier red-proof** (D-9). The
   constructible same-class proof is in the ticket draft and in this ledger.

## What this desk must not do

Write code, drive an epic, enter a worktree, run a ticket, touch another
milestone's lanes, contact the M6 desk, or perform any tool call that is not an
**ask**, an **answer**, or a **sweep**. Merges are authorized here and executed
by the owning lane.
