# Resume brief — M6 (#118) milestone owner

You are the milestone owner for `cardano-foundation/cardano-wallet` milestone
**#118 — M6 — Dijkstra HF readiness**. Continue; do not restart.

Read in this order: `ledger.md`, `registry.md`, `state.md`, `session.md`.
Load `orchestrator-contract` → `milestone-orchestrator` → `context-compiler` →
`worker-protocol` → `tmux-orchestrator` → `invariants`, then `cardano-deps`
and `haskell`.

## Status: AWAITING GO-DARK — wallet lane, 2026-09-01

**`RULING-2026-09-01-wallet-5420-then-dark.md`**, sha256
`090dfdbb57f7b7f6144780f709f68fe902e2f53fe1c3840d1e6360b92210b4b5`, recomputed
at this desk and matching. Machine owner's ruling, relayed by `%107`.

The wallet lane runs **only** until PR **#5420** is **green** (every check
`SUCCESS`) **and open** (out of draft). Then every wallet seat writes `PAUSED`
with its exact head SHA and stops. `%4` is named in the ruling's census.

**The terminal event is the CONDITION, not the ruling.** Do not go dark early and
do not pre-empt the relay.

> ### READ THIS FIRST: `resume/disposition-going-dark.md`
>
> Every open thing this desk owns, with **a disposition rather than a memory** —
> owner, trigger, named object, and the command that measures it. It supersedes
> the "Open work this desk still owns" section below wherever they disagree,
> because it was measured on 2026-09-01 and that section was not.

**Start nothing new.** The ruling names all three of: no new slice, no adjacent
cleanup, no *"while we're here"*. **#5420 is M1's** — no comment, no review
request, no reviewer ping. Do not contact `%310` or `%326`; they were released to
M1 on 2026-09-01 and nothing returns them.

**Scope: wallet lane only. No omnia pausa is in force** — the operator paused the
rest of the host by hand.

## The pause that PRECEDED this one is closed history

The OMNIA PAUSA of 2026-08-29T19:40Z (order sha256 `416a6331…6917`) and the
08-26 / 08-27 orders with their 22+22 acknowledgements are **closed records** —
do not edit or overwrite them.

## SUPERSEDED — the #5402 example below is HISTORY, and the LESSON is not

**#5402 MERGED 2026-08-31 (`99c3a7e88b`). #5399 MERGED 2026-09-01 (`4a2227a725`).**
Do not chase either. What follows is kept because the *reasoning* is the thing
this desk paid for, not because the state is current.

### The lesson, in its general form

> **A decision justified by a condition must be re-measured when that condition
> can change. Asserting the premise is not observing it.** Bound project-wide
> 2026-09-01. `reviewDecision` is a LATCH, not a signal; a remote-tracking ref is
> a CACHE, not the remote; a scarcity argument dies when the scarcity does.

### The original entry, as written


**#5402 is not "in review". Nobody has looked since 2026-08-26.**

```
reviews:        2026-08-26T15:06:58Z copilot COMMENTED
                2026-08-26T16:47:24Z disassembler CHANGES_REQUESTED
re-requested:   2026-08-27T09:05:43Z disassembler   <- POSTDATES both
activity since: 0 issue comments, 0 review comments
```

> **`reviewDecision` is a LATCH, not a signal.** `CHANGES_REQUESTED` is sticky
> from 08-26 and says nothing about whether the reviewer has responded to the
> re-request. **Re-check reviews BY TIMESTAMP**, never by `reviewDecision`.

Every item of that review is addressed and pushed. The tracing lane owes no
code. **The whole stack is waiting on one human who has not looked in two days.**

```sh
gh pr view 5402 --repo cardano-foundation/cardano-wallet --json reviews \
  -q '.reviews[] | "\(.submittedAt) \(.author.login) \(.state)"'
```

**Re-arm on release:** the child-lane monitor, stopped at this pause —

```sh
tail -n 0 -F ./e-node-11-1-0/STATUS.md ./e-tracing-migration/STATUS.md \
  ./w-coin-selection-ghc912/STATUS.md | grep -E --line-buffered \
  "GATE|unblock|COMPLETE|BLOCKED|AUDIT|ACCEPT|push|packet|worktree|5402"
```

Watch the CHILD lanes, **not this desk's own STATUS** — watching your own echoes
your writes back at you, and watching only the epic's misses events that land in
a sibling lane. Both mistakes were made here on 2026-08-28.

## Addresses are STALE — re-derive them, never trust the record

The tmux pane IDs written throughout `ledger.md`, `STATUS.md` and the epic's
own journal are from a previous tmux server generation and **no longer exist**.
`%120`, `%121`, `%122`, `%34`, `%35` are all dead references. NOTE: `%34`/`%35`
in session `0` are now **llm-settings worker panes, still parked under the omnia
pausa** — escalating there reaches an unrelated project's paused lane.

As of 2026-08-26 the live census is:

| role | address |
|---|---|
| this desk (M6 milestone owner) | `wallet:2` pane `%4` |
| epic owner `e-node-11-1-0` | `wallet:3` pane `%5` |
| `w5397-L6` ticket owner | `wallet:3` pane `%53` |
| `w5397-L7` ticket owner | `wallet:3` pane `%81` |
| `w5397-L7` commit owner | `wallet:3` pane `%86` |
| tracing migration epic (**#5401, in M6**) | `wallet:4` pane `%6` |
| `e-node-11-1-0` glm commit owner (coin-selection GHC 9.12) | `wallet:3` pane `%119` |
| #5209 epic owner | `wallet:5` pane `%106` |
| #5406 child-0 ticket owner | `wallet:5` pane `%109` |
| #5406 glm commit owner | `wallet:5` pane `%110` |
| **project owner (parent)** | **`%107`**, `0-projects:cardano-wallet`, root `/tmp/projects/cardano-wallet/owner/` — NEW desk 2026-08-27T08:38Z |
| unowned `codex-raw` (NOT mine — see ledger) | `wallet:5` pane `%97` |

Run `tmux list-panes -a -F '#{session_name} #{window_index} #{window_name} #{pane_id}'`
and rebuild this table before addressing anyone.

## Where things stand at a glance

- branch `chore/issue-5397-node-11-1-0` @ `6dd77f1` = PUSHED remote head, clean
- **28/28 adapted**; gate `GATE-PASS[structure]`, solved set verified PER PATCH via the stg ladder (9 OK, 0 failing) with a positive control; CI on `33ccafc` was IN PROGRESS at the pause
- census **44 across 15 files**, re-proven across the rebase, both controls run
- temporary carry `b42b244` DROPPED by the rebase onto #5402 tip `1ca16ca`
- PR #5399 base is `chore/drop-iohk-monitoring` — STACKED, intended, un-stacking owed
- suppressions **12** (master 7 + 5), both spellings; MAX is MEASURED AT LAND
  TIME and moved 9->10->11->12 in one afternoon — never copy a number in
- **M6 is coupled to #5402**: if it dies, the contra-tracer 0.2 conflict returns
  and this bump has no independent answer

## Where the work actually stands

The `cardano-deps` bump to `cardano-node` 11.1.0 ran as ticket **#5397** (PR
**#5399**, draft) in eight topological levels `L0`→`L7`, one commit per
sublibrary.

- `L0`–`L6`: **26 of 28 packages done.** `L6` accepted 19:04Z at `71f7c29`.
- `L7` (2 benchmark packages): commit owner reached **PROOF-COMPLETE** on
  `71f7c29` before the pause. Needs only a fresh **Codex** auditor — its commit
  owner is claude, so alternation forces codex.
- Gate: `GATE-PASS[structure]`. `GATE-FAIL[solved-set]` on
  `cardano-wallet-tracing` only, which is the out-of-fence carry, pre-existing
  hlint hints, **not** a compile failure.
- **Operator invariant held at every level: census 44, positive control 3.**

### The carry that must be dropped

Commit **`b42b244`** is a temporary carry of a fix owned by **PR #5402** (three
`Monad m` constraints plus one `runTracer`→`traceWith` in
`lib/cardano-wallet-tracing`). It is proven — builds green against
contra-tracer 0.2.1.1, `fourmolu` clean — but it is **not this branch's code**.
Patch: `e-node-11-1-0/../e-tracing-migration/handoffs/PROVEN-tracing-0.2-fix.patch`.
Check whether #5402 has taken it; if so, rebase and **drop `b42b244`**. Left in
place it lands another epic's code under this PR.

## Exact next actions, in order, once RELEASED

1. Verify `71f7c29` still exists and the worktree is clean.
2. The epic owner drives: fresh Codex auditor on `L7` → acceptance → 28/28.
3. Push and PR update are **epic-owned** — credential-bearing, and the barred
   seats (`qwen`, `grok`) must not touch it.
4. Re-cut ticket **#5397**'s title. It was filed as "bump two
   source-repository-package pins" and was executed as a 28-package topological
   bump. The title under-describes the change by an order of magnitude.
5. Re-check **Arc 1's premise** before trusting its decomposition — see below.

## Open work this desk still owns

- **Arc 1's premise is SETTLED and FALSE** — RE-DECOMPOSE before dispatching.
  Upstream carries MERGED Dijkstra feature work: cardano-ledger-read 025829fa
  `feat: add upgradeToOutputDijkstra (#24)` plus two 2026-03-13 era commits, and
  cardano-balance-transaction PR #13 `Replace Babbage with Dijkstra in RecentEra`
  MERGED. The 44 stubs are largely CONSUME-the-upstream-API, not
  implement-from-nothing. The branch already pins the commits carrying them.
- **The upstream pin-set moves are DONE and CONSUMED** (verified on release,
  not inherited): cardano-ledger-read pinned f4d3f064 which contains d5847e29
  (PR #22 MERGED); cardano-balance-transaction pinned d0360834 = main, which
  contains PR #46 MERGED. My earlier "no 11.1.0 branch exists, author it" was
  correct when measured (~17:02Z, between issue #45 at 13:57Z and PR #46 at
  19:21Z) and stale hours later. A negative result about upstream carries an
  EXPIRY: record when it was measured, re-measure before acting.
- **`A1.6`** (one `reconcile(declared, observed)` over the pin-pair registry)
  is held for a **strong metered** ticket-owner seat per `A-009` — claude
  `opus-5[1m]` high or codex-raw high. Not Sonnet, grok or qwen. It now has a
  fourth registered pair, `srp-pin-set-vs-node-target`.
- **#5209** re-scope: its acceptance criterion cannot fail. Binding.
- **#5403** was filed during the work (tracked `specs/` breaks GHC's C-compiler
  probe from the project root; latent on master).

## Things a successor will otherwise get wrong

- **The state page lagged its own milestone by 26 packages.** It was refreshed
  on this desk's transitions but not on its children's. Start every sweep at the
  child lanes' `STATUS.md`, not at this desk's.
- **`publish-state`'s render check has returned could-not-evaluate eleven
  consecutive times** while `curl` returns 200. It is RED, never a silent pass.
  Verify by hand: fetch the wiki page, count `data-type="mermaid"` (must be ≥1)
  and `highlight-source-mermaid` (must be 0), and run the zero-control against
  https://github.com/cardano-foundation/cardano-wallet/milestone/118 which
  genuinely has none. Reported to the llm-settings owner via the project owner.
- **`main` is not "upstream".** Two published negative results were false
  because both were measured against each repo's `main` while the answer lived
  on an open PR. When reporting an absence, **state the search space and justify
  it**. Both had correct positive controls on the *instrument* and none on the
  *denominator*.
- #5209's title says "13 error stubs from node 10.6.2". Both numbers are wrong
  and the bump is overtaken. The census is **44 across 15 files**; do not
  re-inherit 13.
- `cardano-node` 11.1.0 is a **prerelease**. `A-002` rules DECOUPLE: migrate
  against it now, hold the pinned **ref** on a stable release, the flip is its
  own final gated child.
- `/code/cardano-wallet` is **read-only bootstrap ground** carrying a protected
  untracked file `.llm/issue-5309-unit-memory-analysis.md` (sha256
  `71e6dee8a639fece22712f69cd794c0340d64de17ac8cc11e37ebe049cdfc1d5`) the
  operator ordered preserved. No `git clean`, `stash`, `checkout --`,
  `reset --hard`, `restore`, or `cabal`/`nix clean` with that cwd. Anchor every
  command with an absolute path or `git -C`. **Never `cd` there.**
- **Seat bars:** `qwen` and `grok` must never hold a seat that touches
  production secrets or credentials — route the push to a metered seat instead,
  never hand a barred seat the secret and never keep one "just out of frame".
  `agy` is **REVOKED** for every role since 2026-08-14.
- **No lateral contact with M119.** The project owner is the channel both ways.
- Local Nix builds are barred by standing rule; CI proves the Nix side.
- A **supervision gap** cost this run more time than any technical problem: a
  ticket owner has no event loop, so a finished commit owner sits unnoticed. It
  happened at `L5` (40 min) and `L6` (120 min). Poll your children on a cadence,
  or arm a monitor on their `STATUS.md` for `COMPLETE|BLOCKED`.
