# Spec: Require GitHub issue-closing references in PR bodies

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5359

## User story

As a cardano-wallet maintainer, when I merge an issue-backed pull request into
the default branch, its referenced GitHub issue closes automatically instead
of remaining open because the PR only described a relation such as
`Implements #123`.

## GitHub contract

GitHub recognizes these closing keywords, without regard to case: `close`,
`closes`, `closed`, `fix`, `fixes`, `fixed`, `resolve`, `resolves`, and
`resolved`. For an issue in this repository, the supported shape is a keyword,
an optional colon, and a positive issue reference such as `Closes #123` or
`FIXES: #123`.

Source: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue

## Functional requirements

- FR1: CI validates the body of every `pull_request` event and succeeds only
  when the visible body contains at least one supported same-repository closing
  reference.
- FR2: Matching is case-insensitive, accepts all nine supported keyword forms,
  accepts the documented optional colon, and works when the valid reference is
  on any line of a multiline body.
- FR3: Missing or empty bodies, relation-only text such as `Implements #123`,
  malformed references (`#`, `#0`, negative or non-numeric forms), keyword
  substrings, and closing examples that occur only inside HTML comments are
  rejected with a concise remediation message containing a valid example.
- FR4: Non-`pull_request` events exit successfully without attempting to
  validate a nonexistent PR body.
- FR5: The validator reads the GitHub event payload as data. The workflow does
  not interpolate or execute PR body contents, use `pull_request_target`, ask
  for write permissions, or mutate issues or PRs.
- FR6: Deterministic checked-in event fixtures and a focused test runner cover
  accepted, rejected, case, colon, multiline, commented-template, malformed,
  and non-PR variants.
- FR7: The focused test runner accepts an explicitly selected validator path so
  the gate can substitute a known-bad validator. A negative control that accepts
  `Implements #123` must make the test runner fail.
- FR8: `.github/PULL_REQUEST_TEMPLATE.md` tells authors to use explicit closing
  references and shows both `Closes #123` and `Fixes #123`; its instructional
  examples remain comments and cannot themselves satisfy FR1.

## Observable success

- The focused fixture suite passes with the production validator.
- The same suite fails with a seeded validator that wrongly accepts relation-only
  text, proving the rejection fixture is wired and able to go red.
- Independent event probes accept valid keyword/case/colon/multiline inputs,
  reject missing/malformed/relation-only/comment-only inputs, and skip a push
  event.
- The CI workflow invokes both the focused fixture suite and live event
  validation without `pull_request_target`, body interpolation, or write
  permissions.
- Shell lint, YAML parsing, formatting, and the repository's applicable local
  CI checks pass at the proposed head, apart from any baseline defect recorded
  before this ticket's edits.

## Non-goals

- Editing or closing existing issue or PR bodies.
- Supporting Jira references as a substitute for a GitHub closing reference.
- Changing branch protection, review policy, or merge permissions.
- Validating cross-repository issue references in this slice.
