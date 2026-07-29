# Plan

## Scope

This is one vertical, bisect-safe implementation slice. It changes only:

- `.github/workflows/macos-integration.yml`

## Slice 1: add the master-push trigger

First dispatch the existing workflow manually on the branch and require it to
reach the real macOS runner and complete successfully. If that run exposes a
workflow defect, freeze the observed failure, amend this plan and the task
fence, and correct only what blocks execution before proceeding.

Once runnability is proven, add a `push` event filtered to the `master` branch
alongside the existing manual-dispatch event. Keep the workflow's jobs and
their configuration unchanged unless the live run proved a specific correction
necessary.

Proof consists of:

1. a successful manual branch dispatch on the real macOS runner;
2. a focused checker observed failing on the base workflow;
3. the same checker passing on the edited workflow;
4. Actionlint passing;
5. the first post-merge push-triggered run scheduling and succeeding;
6. five consecutive successful `master` runs without retries.

Items 5 and 6 are live-boundary observations made after merge. No pull-request
trigger, schedule, runner provisioning, or unrelated test-content change is
in scope. A job change enters scope only when the pre-merge live run proves it
is required for execution.
