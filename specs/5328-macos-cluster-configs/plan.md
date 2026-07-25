# Plan — #5328

## Tech context

- Deliverable: GitHub Actions workflow YAML only.
- No compiler, no test harness, no product code. `Env.hs` is read-only
  reference material for this ticket.
- Verification is static (`./gate.sh`, yq-based structural assertions) plus
  one manual `workflow_dispatch` run on the self-hosted macOS runner
  (`cf-hal-mac`) for real signal.

## Slice breakdown

One behaviour-changing slice. The change is a two-line deletion in a single
file; splitting it further would produce commits that are not independently
meaningful.

### Slice A — `drop-job-env` (driver + navigator)

Delete the `env:` block (lines 48–49) from the `unit-tests` job in
`.github/workflows/macos-unit-tests.yml`, leaving the job with no `env:` key
at all — matching `ci.yml`'s `unit-tests`.

- **Owned file**: `.github/workflows/macos-unit-tests.yml` (only).
- **RED**: already established and reproducible — `./gate.sh` at the parent
  commit fails with
  `GATE FAIL: ... still sets a job-level LOCAL_CLUSTER_CONFIGS on unit-tests`.
  The driver reproduces this before editing and records the output in
  `WIP.md`; there is no unit-test harness for workflow YAML, so the gate's
  structural assertion is the RED, and that substitution is recorded as a
  documented deviation rather than a skipped step.
- **GREEN**: after the deletion, `./gate.sh` passes.
- **Commit**: `fix(ci): drop job-level LOCAL_CLUSTER_CONFIGS on macOS unit-tests`
  with `Tasks: T001, T002, T003`.

### Slice B — orchestrator-owned: dispatch verification + finalize

Not delegated. After Slice A is pushed:

1. Trigger `macos-unit-tests.yml` via `workflow_dispatch` on the branch ref.
2. Watch the `wallet-unit / Shelley` shard; record the run URL and outcome in
   a PR comment.
3. Refresh the PR body with the real result (or the explicit residual-risk
   statement if the dispatch is not permitted on a non-default ref).
4. `chore: drop gate.sh (ready for review)`, then `gh pr ready`.

## Risks

- **R1 — a root-CWD shard silently needed the variable.** Retired by the
  source survey in `spec.md` (only `NetworkSpec.hs` consumes it) and pinned
  by the gate's step-level assertion.
- **R2 — a future edit removes `working-directory: lib/unit` from a shard**,
  reintroducing the failure through the other branch of `Env.hs`. Pinned by
  the gate's FR3 assertion for this PR's lifetime; after the gate is dropped
  the protection is `ci.yml` parity plus the issue record.
- **R3 — no real macOS signal before merge.** `macos-unit-tests.yml` has no
  `pull_request` trigger, so this PR's own checks cannot exercise it.
  Mitigation is the manual dispatch in Slice B; if that is unavailable the
  PR body must say so plainly rather than claim victory on Linux-only
  reasoning.
