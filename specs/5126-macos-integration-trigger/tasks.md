# Tasks

## Slice 1: add the master-push trigger

- [X] T5126 Prove a manual dispatch executes successfully on the real macOS
  runner; add the `master`-filtered `push` trigger while retaining
  `workflow_dispatch`; demonstrate RED then GREEN with a focused checker; pass
  Actionlint; and commit as
  `ci: run macOS integration tests on master pushes`.

## Post-merge acceptance

After merge, record the first push-to-`master` workflow run URL in this lane's
STATUS and issue #5126, then confirm five consecutive `master` runs complete
without retries. This external observation is not part of the pre-merge
implementation commit.
