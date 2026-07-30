# Spec — #5196 Windows E2E temp cleanup on cancellation

## Problem in one paragraph

When Windows E2E tests are cancelled (timeout or `cancel-in-progress`),
Haskell's `withSystemTempDirectory` cleanup never runs. Temp state lands in
the shared Windows system temp root and leaks ~17GB per run. Linux already
scopes E2E temp to the job-private `${{ runner.temp }}` (see #5195 /
`.github/workflows/linux-e2e.yml`). The Windows workflow must scope temp the
same way, clean fail-closed under that job-private root even when the smoke
step is cancelled, and prove that on a live cancelled run before the PR leaves
draft.

## P1 user story

As a cardano-wallet maintainer running Windows E2E on GitHub Actions, I want
E2E temp directories scoped to the job-private runner temp and removed even
when the job is cancelled, so a timed-out run cannot fill the Windows runner
disk the way cancelled Linux E2E runs once filled `/tmp`.

## Supporting user stories

- As a CI maintainer, I want the Windows cleanup pattern to match Linux's
  safety model: never glob a shared temp root that sibling jobs also use.
- As a reviewer, I want a cheap local check (static workflow assertions +
  actionlint) for the YAML contract, plus a mandatory live cancelled-run
  artifact that shows fail-closed cleanup and zero residual directories.

## Functional requirements

- **FR1** — The `e2e-tests` job in `.github/workflows/windows-e2e.yml` MUST
  set Windows temp env vars (`TEMP`, `TMP`, and `TMPDIR` for consistency) on
  the run step to `${{ runner.temp }}` so `withSystemTempDirectory` creates
  directories under the job-private root.
- **FR2** — The same job MUST contain an `if: always()` cleanup step that
  removes only directories matching `e2e*` / `test-cluster*` under that
  job-private temp root (not under a shared system temp path).
- **FR3** — The workflow MUST document, in a comment next to the temp
  scoping or cleanup step, that cleanup must not glob a shared temp location
  that could wipe sibling jobs' live state (same constraint as Linux E2E).
- **FR4** — The cleanup step MUST be fail-closed:
  - missing or empty `RUNNER_TEMP` fails (non-zero exit);
  - enumeration errors fail;
  - deletion errors fail;
  - after deletion, re-enumerate the same name patterns under the job-private
    root and fail if any residual match remains;
  - on success, emit the stable line
    `Cleanup verified: 0 residual E2E temp directories`.
- **FR5** — No other workflow, Nix, or application source is in scope. Do not
  change the smoke command, runner, timeout, build job, or triggers.

## Non-goals

- Changing which E2E test is matched or how the Windows bundle is built.
- Milestone #113 / cardano-api work (hard exclusion for this desk).
- Linux E2E (already fixed).

## Success criteria

- **SC1** — Focused checker fails on baseline / soft cleanup and passes on the
  fail-closed fixed file (RED then GREEN).
- **SC2** — `actionlint` accepts `.github/workflows/windows-e2e.yml`.
- **SC3** — Workflow YAML shows `${{ runner.temp }}` scoping,
  `if: always()` fail-closed cleanup limited to job-private paths, sibling-job
  safety comment, and the stable zero-residual success line.
- **SC4** (**mandatory pre-merge live-boundary**) — A cancelled or timed-out
  Windows E2E run on the exact PR head shows the cleanup step in the job log
  with the stable success line
  `Cleanup verified: 0 residual E2E temp directories`.
  Named artifact: workflow run URL + cleanup step log excerpt stored under
  the lane runtime (`/tmp/ms-cw-tech-debt/t5196/sc4-cancel-proof.md`).
  This is a pre-merge acceptance item; draft must not flip ready without it.
