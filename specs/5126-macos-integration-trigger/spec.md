# Issue #5126: run macOS integration tests on master pushes

## User story

As a maintainer, I want the dedicated macOS integration workflow to run after
every push to `master` so regressions at the live macOS integration boundary
are detected without a manual dispatch.

## Requirements

- FR1: `.github/workflows/macos-integration.yml` triggers on pushes to the
  `master` branch.
- FR2: The existing `workflow_dispatch` trigger remains available.
- FR3: Before the trigger change is accepted, a manual dispatch must reach the
  real macOS runner and execute successfully. Any workflow defect that blocks
  that outcome is in scope; unrelated job changes are not.
- FR4: If the manual run is already successful, no job, step, permission,
  runner label, or schedule in the workflow changes.
- FR5: Workflow syntax remains valid.

## Acceptance

- A focused checker must fail against the original workflow state and pass
  after the change.
- Actionlint must pass for the changed workflow, allowing the repository's
  custom runner labels.
- Before the trigger commit is accepted, a `workflow_dispatch` run must
  execute successfully on the macOS runner and its run URL must be recorded.
- After merge, the first push-to-`master` run must be observed scheduling this
  workflow, and its run URL must be recorded in the ticket STATUS.
- Five consecutive `master` runs must complete without retries as the issue's
  longer-running stability evidence.
