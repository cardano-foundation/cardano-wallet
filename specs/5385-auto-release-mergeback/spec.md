# Specification: automatic release merge-back PR

## P1 outcome

When a maintainer publishes a stable release, the repository opens the
corresponding release-candidate merge-back pull request to `master` without a
manual `gh pr create` step.

## Requirements

- **R1:** Stable `release.published` events and explicit manual recovery runs
  select `release-candidate/<tag>` as the PR head and `master` as its base.
- **R2:** Existing open or merged PRs for that head prevent duplicate creation.
- **R3:** The tag's peeled commit must equal the release-candidate branch head;
  a mismatch fails before PR creation.
- **R4:** Prerelease publication does not create a merge-back PR.
- **R5:** The created PR is unapproved and unmerged, assigned to `paolino`, and
  labelled `Release` and `CI/CD`.
- **R6:** Tests exercise selection, validation, and idempotency without writing
  to GitHub.

## Invariants

- **I1 Identity:** tag, head branch, and head SHA describe the same released
  commit.
- **I2 At most one:** a release branch has at most one merge-back PR regardless
  of workflow retries.
- **I3 Stable only:** publication side effects exclude prereleases.
- **I4 Least authority:** workflow permissions are limited to contents read and
  pull-request write.

## Non-goals

No automatic merge or approval, ref rewriting, tag rewriting, release-note
changes, version generation changes, or prerelease merge-back PRs.
