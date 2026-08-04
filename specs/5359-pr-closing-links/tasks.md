# Tasks: Require GitHub issue-closing references in PR bodies

## Slice A — validator, executable fixtures, CI wiring, and template guidance

- [ ] T5359-S1 Validate real `pull_request` event bodies for the nine supported
      closing keywords, optional colon, case variants, multiline bodies, and a
      positive same-repository issue number
- [ ] T5359-S2 Reject missing, malformed, relation-only, substring, and
      HTML-comment-only references with actionable remediation; safely skip
      non-PR events
- [ ] T5359-S3 Add deterministic event fixtures and a focused runner whose
      seeded `Implements #123` negative control is proven able to fail
- [ ] T5359-S4 Wire the focused suite and live event validation into CI without
      `pull_request_target`, body interpolation, write permissions, or mutation
- [ ] T5359-S5 Update the PR template with commented `Closes #123` and
      `Fixes #123` guidance that cannot satisfy validation by itself
- [ ] T5359-S6 Pass the immutable slice gate, ticket gate, lint, formatting,
      YAML parsing, and applicable repository CI checks
- [ ] T5359-S7 Commit `fix(ci): require GitHub issue closing keywords in PR bodies`
      with the complete `Tasks:` trailer
