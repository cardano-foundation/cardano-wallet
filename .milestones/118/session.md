# M6 (#118) — how to reopen and resume this milestone's agents

Session `wallet` on host `epyc`. A stranger with `tmux` and `git` rebuilds the
milestone from this file alone. Every launch line is exact and pasteable.

**Standard context pin for supervising seats — no `[1m]`.** Per
`llm-settings` `615711b`: compaction bounds the cost of every supervisor call.
The desk itself is still on `[1m]` from before that rule and closes the
deviation at its next context boundary.

---

## Window: `cardano-wallet-ms6-dijkstra` — the desk (singleton, one pane)

Why it exists: the milestone owner. Present at the desk, never executing.
Its only actions are **asks, answers and sweeps**.

```sh
tmux new-window -t wallet: -n cardano-wallet-ms6-dijkstra \
  -c /tmp/projects/cardano-wallet/ms6-dijkstra \
  "claude --dangerously-skip-permissions --model claude-opus-5 --effort high"
```

- **cwd / runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra`
  (legacy path; **not** `/tmp/ms-cardano-wallet-118`)
- **resume paste:** *"Read `.milestones/118/resume/ms.md` on the `milestones`
  branch of `cardano-foundation/cardano-wallet` and continue."*
- **holds no worktree.** The ref that identifies its state is `origin/master`.

---

## Window: `cardano-wallet-ms6-t5422-delete-era-split` — ticket owner, #5422

Why it exists: the first Dijkstra child — delete the spurious era split in
`mkLedgerTx` and sweep six sites for six evidenced verdicts.

```sh
tmux new-window -t wallet: -n cardano-wallet-ms6-t5422-delete-era-split \
  -c /tmp/projects/cardano-wallet/ms6-dijkstra/t5422-delete-era-split \
  "claude --dangerously-skip-permissions --model claude-opus-5 --effort high"
```

- **runtime root:** `/tmp/projects/cardano-wallet/ms6-dijkstra/t5422-delete-era-split`
- **worktree:** `/code/cardano-wallet-5422`
- **branch:** `refactor/5422-delete-era-split`, based on `6b42c36b58`
- **resume paste:** the pointer to `brief.md` in that root, then its `inbox/`.
- **history:** first seat `%670` died mid-`PLANNING` 2026-09-07 with no
  terminal event; respawned as `%673` in the **same** root, standard pin.

---

## Archived lanes — roots preserved, do not delete

Both accepted after mechanical verification, under
`/tmp/projects/cardano-wallet/ms6-dijkstra/.archived/`:

- `t5413-review-findings` — evidence lane on #5413's review. Candidate
  `3fa0284708` pinned at `refs/5413/candidate`, on no branch.
- `t5421-invalid-continuation` — cause investigation on #5421. Report and 31
  evidence artefacts, all hash-verified.

---

## Standing environment facts

- **`/code/cardano-wallet` is read-only bootstrap ground.** Never `cd` there;
  anchor with `git -C`. It carries a protected untracked file
  `.llm/issue-5309-unit-memory-analysis.md` the operator ordered preserved.
- **Local `nix build` is barred.** `nix develop -c cabal` is sanctioned.
- **No lateral contact** with M1 / M7 / M119 desks — the project owner `%107`
  is the channel in both directions.
- **No outward prose.** No PR comment, issue comment, review or reviewer ping.
  Drafts go to `handoffs/` and the operator posts them.
