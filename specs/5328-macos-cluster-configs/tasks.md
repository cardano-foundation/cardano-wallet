# Tasks — #5328

## Slice A — drop-job-env (driver + navigator)

- [ ] T001 Reproduce the RED: run `./gate.sh` unchanged, capture the
      `GATE FAIL: ... still sets a job-level LOCAL_CLUSTER_CONFIGS` line in
      `WIP.md`.
- [ ] T002 Delete the `env:` block (`LOCAL_CLUSTER_CONFIGS`) from the
      `unit-tests` job in `.github/workflows/macos-unit-tests.yml`, leaving
      no `env:` key on that job. No other edit in the file, no other file.
- [ ] T003 GREEN: `./gate.sh` passes; capture the output in `WIP.md`.
      Commit as `fix(ci): drop job-level LOCAL_CLUSTER_CONFIGS on macOS unit-tests`
      with trailer `Tasks: T001, T002, T003`. Do not push.

## Slice B — verification + finalize (orchestrator-owned)

- [ ] T004 Dispatch `macos-unit-tests.yml` against the branch ref and record
      the run URL.
- [ ] T005 Report the `wallet-unit / Shelley` outcome in a PR comment and
      refresh the PR body with the real result (or the explicit
      residual-risk statement if the dispatch is not possible).
- [ ] T006 `chore: drop gate.sh (ready for review)`; `gh pr ready 5331`.
