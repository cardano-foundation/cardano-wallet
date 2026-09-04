# M1 (#113) — tmux session record

Session `wallet` on this host. **The `wallet` session has no session owner**
(`wallet:1 orch %3` is a bare bash prompt). Window mechanics here have no
mechanic: this window was created by the **project owner** as a named,
operator-directed exception. A child window for an M1 lane is requested from
`%107` and recorded the same way — do not assume a mechanic exists.

---

## `wallet:6` — `ms1-drop-cardano-api` — MILESTONE OWNER (singleton, 1 pane)

Why: the M1 desk. Singleton because a milestone owner has no code, no pairs and
no slices; a quadrant here would be three dead panes inviting work that must
never happen at this altitude.

- pane: `%122`
- cwd / runtime root: `/tmp/projects/cardano-wallet/ms1-drop-cardano-api/`
- family/model: `claude` / `claude-opus-5[1m]`
- role skills, in order: `orchestrator-contract` → `milestone-orchestrator` →
  `context-compiler` → `worker-protocol` → `tmux-orchestrator` → `invariants`,
  then `haskell` and `cardano-deps`; `nix` before anything touches `flake.nix`,
  `cabal.project`, or a flake input.

Launch line — replay **exactly**, quotes included:

```sh
claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high
```

Resume paste:

```
Read /tmp/projects/cardano-wallet/ms1-drop-cardano-api/brief.md in full, then
.milestones/113/resume/ms.md on the `milestones` branch. Acknowledge in STATUS.md.
```

Note: the skill's naming convention is `<repo>-ms<N>-<goal>`, i.e.
`cardano-wallet-ms113-drop-cardano-api`. The current name was set by `%107`;
renaming is parked decision **P-5** rather than taken unilaterally, since a
colliding `ms1` namespace exists on this host (`treasury-ms1`).

## `wallet` session — RESTRUCTURED 2026-08-30/09-01

```
1 orch                       3 ms1-drop-cardano-api      (%122, claude — this desk)
2 ms6-dijkstra               4 w-specs-deprecations      (%304, codex-raw — #5419 commit owner)
                             5 t5419-specs-deprecations  (%310, claude — #5419 ticket owner)
```

**GONE:** `cardano-wallet-ms113-t-deprecations-to-ledger` (`%127`, `%242`) — the
#5411 lane, removed during the park. Also gone: M6's `e-node-11-1-0`,
`e-tracing-migration`, and `cw-e5209-t0-census-ratchet`.

`%310` and `%304` are **transferring to this desk** under `%107`; **no contact
until `%107` relays M6's confirmation.**

## `wallet:7` — `cardano-wallet-ms113-t-deprecations-to-ledger` — TICKET LANE (TERMINATED)

**The seat no longer exists.** Its durable root survives at
`t-deprecations-to-ledger/` with 154 journal events, 8 archived worker roots and
a closing artifact. Retained below as the record of how it ran.

Why: the deprecations ticket, cleared by the operator 2026-08-28T18:40Z. Window
created by `%107` as a named operator-directed exception (the `wallet` session
has no session owner).

- runtime root: `/tmp/projects/cardano-wallet/ms1-drop-cardano-api/t-deprecations-to-ledger/`
- brief: that root's `brief.md`
- `START`: `2026-08-28T13:28:59Z  mode=TICKET-OWNER pane=%127 family=claude model=claude-opus-5[1m] effort=high`

| pane | seat | family | launch, replay exactly |
|---|---|---|---|
| `%127` | ticket owner | `claude` | `claude --dangerously-skip-permissions --model 'claude-opus-5[1m]' --effort high` |
| `%128` | commit owner | `glm` | `glm --approve` — **the ticket owner launches it, not this desk** |
| `%129` | work slot / fresh auditor | `grok` | `grok --always-approve -m grok-4.6` — ticket owner launches |

Seats derived mechanically, not chosen:
`alternate-authoritative-cli --seat commit-owner claude` -> `glm`;
`--seat commit-auditor glm claude` -> `grok`. Three distinct families, auditor
never `glm`, one `glm` seat, no production secrets in scope.

**Binding `glm` identity:** `family=glm harness=pi provider=zai
model=glm-5.3-flash effort=max`. An earlier relay said `glm-5.3`; withdrawn by
`%107` after checking four sources.

Resume paste for `%127`:

```
Read /tmp/projects/cardano-wallet/ms1-drop-cardano-api/t-deprecations-to-ledger/brief.md
in full, then your STATUS.md and inbox/. Acknowledge with status-event.
```

**Journal format is enforced:** every event goes through
`worker-protocol/scripts/status-event`. The lane's first `START` was written as
markdown prose and was unmatchable by any wait; `inbox/NOTE-001` corrected it and
the brief now names the script. **A prose journal is indistinguishable from a
child that never started.**

## Lanes not yet created

The **closure instrument** lane is not created: its ticket is still at the
operator gate. **U-5**'s lane follows its issue being filed.

## Adjacent windows in this session — NOT M1's, do not touch

| window | owner | note |
|---|---|---|
| `wallet:1 orch %3` | none | bare bash; no session mechanic |
| `wallet:2 ms6-dijkstra` | M6 milestone owner | **no contact** (fence 1) |
| `wallet:3 e-node-11-1-0` | M6 | no contact |
| `wallet:4 e-tracing-migration` | M6 | no contact |
| `wallet:5 cw-e5209-t0-census-ratchet` | M6 | **PARKED** on operator instruction — not ours, not to be inferred as released |

Routing: pause / release / capacity orders reach this desk **only through
`%107`**. From any other desk they are unrouted — check with `%107` first.
