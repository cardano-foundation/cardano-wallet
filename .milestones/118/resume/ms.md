# Resume brief — M6 (#118) milestone owner

You own `cardano-foundation/cardano-wallet` milestone **#118 — M6 — Dijkstra
HF readiness**. **Continue; do not restart.**

**Load the whole chain before acting**, not just your altitude file:
`orchestrator-contract` → `milestone-orchestrator` → `worker-protocol` →
`tmux-orchestrator` → `context-compiler`, then `debrief` for the story
register. An altitude role sits still while the rules governing supervision
move underneath it. Check what moved:

```sh
git -C /code/llm-settings log --oneline --since=<last reload> -- shared/skills/
```

Last reload: **`0fc525f`**, 2026-09-07.

Then read `ledger.md`, `registry.md`, `session.md`.

## Status: ACTIVE

Released 2026-09-07 (`RELEASE-2026-09-07T0824Z-cardano-wallet-project-and-lane.md`).
`master` = `6b42c36b586c495a294eb11331984f90a3609470`. This desk holds **no
worktree**; that ref is its state.

## The three things most easily got wrong

**1. The census reaching zero does not mean the wallet is ready.** Its
population is code that *announces* it is unimplemented. `guardsTxBodyL`
announces nothing. **A stub fails loudly; that succeeds wrongly.** The epic
body says so — do not let a green ratchet be read as a delivered outcome.

**2. The ratchet enforces one direction only.** Measured: adding a stub exits
**1**; retiring one without lowering `MAX` prints `RATCHET SLACK` and exits
**0**. So *"census falls **and** the ratchet moves in the same PR"* is **this
desk's obligation at acceptance**, not something CI catches. Never report it as
enforced.

**3. M6 delivers 39 of 44.** The shim module's five die with #5290. The unit
owed on them is **a check, not a change**.

## Exact next actions

1. **The four release blockers stay priority 1** — #5408, #5416, #5409, #5417.
   **#5417 is a decision, not a measurement**, and it is with the operator; do
   not try to measure your way to it.
2. **Supervise `%673`** (#5422) through `wait-status`, one long wait per turn.
   It owes `NOTE skills-reloaded 0fc525f …` plus its seat roster.
3. **Accept #5422 by hand on both halves** — census **38** *and* `MAX` **38**,
   shown with the gate's own output.
4. **Sweep the ledger and the story register** at every accepted landing and at
   the end of every desk turn, at most hourly.

## Measurement discipline this milestone paid for

Each line is a defect that actually happened here:

- **Read `$?` immediately, never after a pipe** — `cmd | tail` gives you
  `tail`'s status. Nearly produced a false "the gate does not bite" escalation.
- **Every counter carries a positive and a negative control.**
- **`sort -V`, never lexical** — `0.9.0.0` sorts above `0.26.0.3` otherwise.
- **Counts come from the tree, never prose** — doc files quote the pragmas.
- **A remote-tracking ref is a cache, not an observation.** Name the ref you
  measured and the fetch that filled it.
- **A SHA is copied from a command, never reconstructed.** A spliced head SHA
  once landed in a `PAUSED` entry naming an object that does not exist.
- **Bind a child's identity from `tmux list-panes`**, never its self-report.
- **Publication date is not evidence of inclusion** — test by content.
- **Do not confirm a prediction; test it.**
- **An instrument's population defines what its zero means.** Prove the
  *generator* red, not only the assertion.
