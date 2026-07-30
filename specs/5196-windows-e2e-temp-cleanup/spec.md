# Spec — #5196 Windows E2E temp cleanup on cancellation

## Problem in one paragraph

When Windows E2E tests are cancelled (timeout or `cancel-in-progress`),
Haskell's `withSystemTempDirectory` cleanup never runs. Temp state lands in
the shared Windows system temp root and leaks ~17GB per run. Linux already
scopes E2E temp to the job-private `${{ runner.temp }}` (see #5195 /
`.github/workflows/linux-e2e.yml`). The Windows workflow
(`.github/workflows/windows-e2e.yml`) has no temp scoping and no
`if: always()` cleanup.

## P1 user story

As a cardano-wallet maintainer running Windows E2E on GitHub Actions, I want
E2E temp directories scoped to the job-private runner temp and removed even
when the job is cancelled, so a timed-out run cannot fill the Windows runner
disk the way cancelled Linux E2E runs once filled `/tmp`.

## Functional requirements

- **FR1** — The `e2e-tests` job MUST set `TEMP`, `TMP`, and `TMPDIR` on the
  run step to `${{ runner.temp }}`.
- **FR2** — The same job MUST contain an `if: always()` cleanup step that
  removes only `e2e*` / `test-cluster*` under that job-private temp root.
- **FR3** — The workflow MUST document that cleanup must not glob a shared
  temp location that could wipe sibling jobs' live state.
- **FR4** — No other workflow, Nix, or application source is in scope.

## Non-goals

- Changing which E2E test is matched or how the Windows bundle is built.
- Milestone #113 / cardano-api work.
- Linux E2E (already fixed).

## Success criteria

- **SC1** — Focused checker fails on baseline and passes on the fixed file.
- **SC2** — `actionlint` accepts `.github/workflows/windows-e2e.yml`.
- **SC3** — Workflow YAML shows `${{ runner.temp }}` scoping, `if: always()`
  cleanup, and the sibling-job safety comment.
