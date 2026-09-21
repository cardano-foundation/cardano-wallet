# M6 (#118) — tmux session, rebuild and resume

**Swept 2026-09-21T16:30Z.** Every launch line below was read out of `ps` at
sweep time, not remembered. A stranger with `tmux` and `git` rebuilds the
session from this file alone.

**Session:** `wallet`. **This milestone owns three of its four windows.**

```
wallet:1  orch                                  bare bash, no agent      %3
wallet:2  cardano-wallet-ms6-dijkstra           THE DESK                 %4     <- me
wallet:3  cardano-wallet-ms6-delegation-certs   ticket owner             %936
wallet:4  cardano-wallet-ms6-t5410-era-detection ticket owner            %1880
```

**Window indices moved on 2026-09-21** when the drop-cardano-api milestone was
split out (below). **Pane IDs did not**, and pane IDs are what `send-pointer`
addresses — prefer them over `session:window` everywhere.

---

## wallet:2 — the desk (`%4`)

The milestone owner. **One window, one pane, always.** No code, no pairs, no
slices; a quadrant here would be three dead seats advertising work that must
never happen in them.

```sh
# cwd: /tmp/projects/cardano-wallet/ms6-dijkstra
claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
```

- **runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra`
- **journal:** `STATUS.md` in that root — **this is the durable record.** The
  `/tmp` sweep of 2026-09-21 ate the archived roots and left the journals; do
  not trust a prose artifact you have not just stat'd.
- **worktree:** none. This desk holds no checkout; `origin/master` is its state.
- **resume with:** `resume/ms.md` in this directory.

> The `[1m]` context pin is deliberate here and **wrong for a supervising
> child** — `orchestrator-contract` wants supervisors at the standard pin so
> compaction bounds the cost of every call.

## wallet:3 — `t-delegation-certs` (`%936`)

Ticket owner for the delegation-certificate stub slice. **Released
2026-09-21T16:24Z** after ten days parked on a roster question.

```sh
# cwd: /tmp/projects/cardano-wallet/ms6-dijkstra
claude --dangerously-skip-permissions --model claude-opus-5 --effort high
```

- **runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra/t-delegation-certs`
- **worktree:** none yet — to be cut from `92b57304dfb5`
- **issue / PR:** neither filed yet
- **agreed roster:** this seat + **one commit owner, Codex `gpt-5.6-sol`,
  effort `high`**. Gate-author pair waived; **auditor DEFERRED, not omitted.**
- **resume paste:**
  > Read `/tmp/projects/cardano-wallet/ms6-dijkstra/t-delegation-certs/answers/A-003-team-negotiation.md`
  > and `handoffs/remeasure-and-attribution-92b57304dfb5.md` in full, then
  > continue: file the issue, cut the worktree from `92b57304dfb5`, author the
  > frozen gate yourself, report the `pendingWith` attribution split **before**
  > cutting the gate, then dispatch the single commit owner.

## wallet:4 — `t5410-era-detection` (`%1880`)

Ticket owner for **#5410**, era detection by trial decode. Outranks
stub-clearing.

```sh
# cwd: /tmp/projects/cardano-wallet/ms6-dijkstra
bash -lc 'codex --dangerously-bypass-approvals-and-sandbox \
  -C /tmp/projects/cardano-wallet/ms6-dijkstra \
  -c model_reasoning_effort=high; echo "codex exited rc=$?"; exec bash'
```

- **runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra/t5410-era-detection`
- **worktree:** `<runtime root>/branch`, branch
  `test/5410-era-detection-invariant`, base `92b57304dfb5`
- **evidence:** `<runtime root>/evidence/baseline-werror-build.log`
- **resume paste:**
  > Read `/tmp/projects/cardano-wallet/ms6-dijkstra/asks/ASK-5410-era-detection-by-trial-decode.md`
  > in full and continue from your own `STATUS.md`. Escalate only to `%4`.

> **The 40-minute silent build is normal, not a stall.** The lane verified it
> daemon-side (`ghc-9.12.3` at 88% CPU) rather than guessing. Judge liveness
> from the child's **journal age** first; pane quiet is not a proxy.

## wallet:1 — `orch` (`%3`)

**A bare, empty `bash`. There is no session-owner agent in this session.**
Kept as a mechanic's shell. Recorded because its absence changes routing: a
mechanical session request has nobody to route to, and the desk performs it
itself and journals that it did.

---

## Split out 2026-09-21: session `wallet-ms113`

On operator instruction, **M1 — Drop cardano-api (#113) moved to its own
session**, so `wallet` now carries M6 only.

```sh
tmux new-session -d -s wallet-ms113 -n orch -c /tmp/projects/cardano-wallet/ms1-drop-cardano-api
tmux move-window -s @48  -t wallet-ms113:2     # the M1 desk           %122
tmux move-window -s @86  -t wallet-ms113:3     # t5412                 %310
tmux move-window -s @259 -t wallet-ms113:4     # t5288 parity oracle   %782 %790
```

All five panes verified `dead=0` and both composers verified intact
afterwards: **`move-window` relocates the window object; it does not restart a
process or clear a prompt.** Pane IDs are unchanged, so every `send-pointer`
address still resolves. **What is stale is the `session:window` coordinate in
M1's own ledger and in `shared/milestones.md` — M1's line to fix, not this
desk's.**

## Closed 2026-09-21: `wallet:7`

Three panes this desk never dispatched, one of them (`%1588`) idle **inside
this milestone's runtime root** on a 548k-token context — almost certainly the
`cardano-ledger-read` #20 evidence lane that ran here undispatched. Closed on
operator instruction **after** confirming its durable artifacts survive:
`handoffs/REPORT-ledger-read-20-era-shape.md` and
`handoffs/DRAFT-COMMENT-ledger-read-20.md` are on disk, and the verdict is also
in `STATUS.md` at `2026-09-21T15:23:44Z`. Conversation context was lost; the
finding was not.

---

## Known live hazard: stranded composers

**Four operator instructions were found sitting unsubmitted in pane composers
on 2026-09-21**, one of them a roster decision that had parked a lane for over
half an hour:

| pane | stranded text |
|---|---|
| `%936` | `go with the 1-seat roster, audit deferred` |
| `%122` | `seat the 5420 audit` |
| `%310` | `what did the gate say` |

**The mechanism:** the prompt marker carries a **non-breaking space**, so
`grep '^❯ '` reports a full composer as **empty**. Check byte-wise:

```sh
tmux capture-pane -p -t %NNN -S -3 | cat -A | tail -3
```

**Do not press Enter in another seat's composer and do not retype an
operator's words into a pane** — text entering a composer reads as the
operator speaking. Confirm at the desk, then write the answer file yourself;
the parent owns the answer anyway.
