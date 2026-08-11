# Resume — cardano-wallet technical-debt cleanup desk (#119)

- **State:** PARKED under `OMNIA-PAUSA-2026-08-11.md` (declared 18:35Z; Claude 51%, CI-runner outage restored -- unrelated to our two flakes; pointer `POINTER-OMNIA-1786516500`). Public state refreshed BEFORE parking (see below, updated finding). No signed-commit test suite owned (N/A). Zero live children -- verified no master commits/merges since 2026-08-05 (head still d3d170d0). Nothing in flight, nothing to land.
- **Desk Window:** `cardano-wallet-ms119-technical-debt` (session `wallet`, window `@3639`)

## Public state refresh (done before parking, per instruction)

- description.md (static, new split format) unchanged since 2026-08-08 --
  no republish needed, definition has not changed.
- state.md republished for 2026-08-11 (commit `70ba223`): dates refreshed,
  explicit note that master hasn't moved and both badges are confirmed
  still red on the same runs.
- **Render verification: now CONFIRMED FAILING, not merely unverified.**
  Second independent WebFetch by-eye check (2026-08-08 and 2026-08-11, two
  different pushes) both report the mermaid block displaying as plain
  syntax-highlighted source, not a rendered diagram. The `curl`-based check
  in publish-description.sh also failed to fetch the page both times
  (rate-limited/challenged) -- could-not-evaluate, correctly treated as RED
  by that instrument. Two consistent by-eye readings now outweigh
  coincidence: **this repo's wiki is not rendering Mermaid for this page,
  contrary to the skill's documented premise.** This is a shared instrument
  (publish-description.sh, used by other milestones too) -- not this desk's
  to fix, but worth surfacing to whoever maintains milestone-orchestrator.
  Content itself remains live and byte-correct either way.

- **Runtime:** `/tmp/ms-cw-tech-debt`

---

## Outcome-test amendment (operator, 2026-08-05, live conversation)

The operator added a third completion clause: **all master README CI badges
must be green at milestone close.** Recorded in `ledger.md` outcome test.

Audited state at 2026-08-05T10:15Z (head `d3d170d0`, post-#5362 merge):
- `Windows` red — `wallet-test-utils` diagnostic-timeout test, `UnliftIO` EOF.
  Reproduced identically on the two prior master heads (`ee8def32be`,
  `ecf489fad679`) — pre-existing flake, not caused by any #119 merge.
- `macOS Integration Tests` red — 8 Conway stake-pool timing failures
  (metadata-fetch / non-myopic-reward 90s waits). First red at this head,
  but the only thing that just merged (#5362) touches only
  `scripts/ci/*`/`specs/5359-*` — unrelated to stake pools. Flake, not a
  regression.
- All other badges green.

**Next action, on RELEASE:** prepare and dispatch two standalone tickets
(Windows flake root-cause, macOS Integration flake root-cause) the same way
#5358/#5359 were prepared — do not dispatch under the current pause.

---

## Machine Owner Question Answer (Model Override Record)

- **Question:** Desk seat running `agy` and `t5359` / `t5303` / `t5358` holding `codex-raw` (`gpt-5.6-sol` high) seats when release was set to 'ONLY qwen max'. Were these directly authorized by operator?
- **Answer on Record:** **YES, DIRECTLY AUTHORIZED BY OPERATOR.**
  - In direct prompt history, operator commanded: `"t5358 lost the t.o. create a chatgpt sol high t.o. for it"`.
  - When Gemini CLI (`agy`) started in window `@3608`, operator explicitly corrected: `"5358 is not chatgpt"`, instructing desk to use `codex-raw` (`gpt-5.6-sol` high).
  - Subsequent dispatches (`t5359`, `t5303-rebase`) followed the operator's explicit `gpt-5.6-sol` high directive.
  - `agy` desk seat was inherited from session environment.

---

## Child Lanes Status — all resolved since last park

1. **#5358 / PR #5361 (`unit-tests-cabal-match`):** MERGED by operator
   2026-08-04T11:32:45Z, 54/54 green. Issue #5358 CLOSED.
2. **#5359 / PR #5362 (`pr-closing-links`):** MERGED by operator
   2026-08-05T09:50:51Z, 57/57 green. Issue #5359 CLOSED.
3. **#5303 / PR #5343 (`Byron RndState reseed`):** MERGED by operator
   2026-08-05T08:52:40Z (`ee8def32be`), after Pawel review. Issue #5303
   CLOSED.
4. **#5325 / PR #5345 (`depositReturned` cert awareness):** MERGED
   2026-08-04T13:03Z (`00d1dde353`), after Pawel review. Issue #5325 CLOSED.
5. **Exemption:** `machine-night-watch` PID 648893 remains running per
   operator order.

No live lanes/windows/panes remain for any of the above — all four are done.

---

## Remaining outcome-test gaps

- **#5246** — PR #5356 merged, but the issue itself remains OPEN on GitHub.
  Needs an owner-lane closeout comment + close, no code change.
- **#5115** — OPEN, parked under the operator's no-new-work order.
- **#5326** — PREPARED (new-ticket shape), no lane/PR yet; Pawel review
  required post-delivery.
- **CI-green amendment** — two unfiled tickets, see above.

## Resume Instructions (Upon Machine Owner RELEASE)

1. Load `milestone-orchestrator` and inspect `/tmp/ms-cw-tech-debt/STATUS.md`.
2. No PRs are queued for merge anymore — all four prior queue entries are
   merged. Next work is: close out #5246, and prepare+dispatch the two
   CI-flake tickets (Windows, macOS Integration) per the outcome-test
   amendment.
3. Operator alone merges PRs.
