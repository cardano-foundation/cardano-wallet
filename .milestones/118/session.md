# M6 (#118) — tmux session record

**Re-derived 2026-08-27.** Every pane id in the previous version of this file
was from a dead tmux server generation. **Do not trust a recorded pane id across
a restart — re-derive it:**

```sh
tmux list-panes -a -F '#{session_name} #{window_index} #{window_name} #{pane_id}'
```

## Live layout — session `wallet`

| window | pane | who |
|---|---|---|
| `wallet:1` `orch` | `%3` | session owner (bash) — window/pane mechanic |
| `wallet:2` `ms6-dijkstra` | `%4` | **this desk**, M6 milestone owner |
| `wallet:3` `e-node-11-1-0` | `%5` | epic owner — the `cardano-deps` bump (#5395 / #5397 / PR #5399) |
| `wallet:3` | `%53`, `%81`, `%86` | its L6/L7 ticket owners and L7 commit owner |
| `wallet:4` `e-tracing-migration` | `%6` | epic owner — drop iohk-monitoring (**#5401, moved into M6 by `A-005`**) |
| `wallet:5` `codex-raw` | `%97` | **NOT THIS DESK'S.** See below. |
| `wallet:6` `e-5209-dijkstra-era` | `%106` | epic owner — eliminate the 44 Dijkstra stubs (#5209) |

## Parent

Cardano Wallet **project owner**, pane **`%107`**, window
`0-projects:cardano-wallet`, root `/tmp/projects/cardano-wallet/owner/`.
**New desk, seated 2026-08-27T08:38Z.**

The previous parent address — `%35`, `0-projects:3`, root under
`/tmp/machine/session-restore-20260824/` — is **dead**. `%34` and `%35` in
session `0` are now **`llm-settings` worker panes, still parked** under
`OMNIA-PAUSA-2026-08-26T2014Z`. Escalating to the old address does not reach a
project owner and *does* reach an unrelated project's paused lane.

## The window-mechanics rule, and the one exception on the record

Standing rule: this desk **asks** the session owner for windows and panes; it
does not run `new-window` or `split-window` itself.

**Exception, 2026-08-27:** the operator instructed this desk directly to *"open
a 5209 window"*. `wallet:6` was created by this desk with `tmux new-window`.
Recorded as a named operator-directed exception rather than left as a silent
contradiction of the rule — the rule is unchanged for every other case.

## `wallet:5` `%97` — disowned

An unnamed `codex-raw` seat (`gpt-5.6-sol`, high) whose cwd is this milestone's
**tracing epic runtime root**. It is **not this desk's**: never spawned by it,
never authorised by it, absent from both this desk's journal and the tracing
epic's 138-line journal, with no brief, no role file and no `START`.

Its shell started **2026-08-27 08:29:55** local, its `codex-raw` at 08:30:02 —
*after* the tracing lane's own children were archived. Its prompts read as
direct human instructions, so it is most likely the **operator's own console**.

Two real problems remain regardless of ownership, and neither is about the
quality of its work:

- **Two agents share one runtime root** (`%6` and `%97` both hold
  `e-tracing-migration/`), which breaks the one-writer rule and makes that
  STATUS unreliable as a control record.
- **It acted on shared org infrastructure** from inside a milestone runtime
  root: a 50 GiB `nix-store --gc` on the shared macOS CI builder `cf-hal-mac`.

Fence that applies whoever owns it: **remote-builder capacity is host
territory.** Diagnosing that CI is red for infra reasons belongs to a lane;
reaching into `cf-hal-mac` to fix it goes to the machine owner.
