# Plan: Require GitHub issue-closing references in PR bodies

One behavior slice, because the validator, its negative controls, CI wiring,
and template guidance form one boundary: none is useful or safe to land without
the others.

## Slice A — validator, executable fixtures, CI wiring, and template guidance

1. Add a POSIX-facing Bash validator under `scripts/ci/` that receives the
   GitHub event name and event payload path through the runner environment,
   extracts the PR body as data with `jq`, removes HTML comments before
   matching, skips non-PR events, and emits a concise remediation on rejection.
2. Add deterministic JSON fixtures under a dedicated `scripts/ci/fixtures/`
   directory plus a focused test runner. The runner exercises the production
   validator by default and supports a validator-path override used only by the
   owner gate's independent negative control.
3. Extend `.github/workflows/ci.yml` so the quality matrix runs the focused
   fixture suite and validates the current GitHub event. Use the existing
   `pull_request` trigger and read-only checkout; do not add
   `pull_request_target`, PR-body expression interpolation, secrets, or write
   permissions.
4. Update `.github/PULL_REQUEST_TEMPLATE.md` with commented closing-keyword
   guidance and both requested examples. The validator must ignore those
   comments.
5. Verify the focused suite both ways (production validator green, seeded
   over-accepting validator red), independent event probes, ShellCheck, YAML
   parsing, formatting, changed-path scope, and repository CI.

## Invariants

- A template left untouched cannot satisfy the guard.
- A passing non-PR event is distinguishable from a validated PR event in the
  command output.
- A test harness that stops checking `Implements #123` is caught by the seeded
  negative control.
- No untrusted PR-body bytes become shell syntax or a GitHub Actions expression.
- The committed workflow is the caller of both the test suite and live
  validator; checked-in scripts without that wiring do not satisfy the ticket.

## Owned implementation paths

- `.github/workflows/ci.yml`
- `.github/PULL_REQUEST_TEMPLATE.md`
- `scripts/ci/check-pr-body-closing-link.sh`
- `scripts/ci/test-pr-body-closing-link.sh`
- `scripts/ci/fixtures/pr-body-closing-link/**`

The ticket owner separately owns this `specs/5359-pr-closing-links/` planning
record. All other repository paths are forbidden for the implementation slice.

## Topology decision

Use PAIR with fresh driver and navigator contexts. Although the patch is small,
the correctness surface crosses GitHub event semantics, Markdown comments,
workflow wiring, shell data handling, and a security boundary. A mechanical
gate can prove behavior and scope, but it cannot replace semantic review of all
those relationships.

Nested driver tools are forbidden: the slice is compact and no eligible
low-semantic-density production unit justifies another authority boundary.
