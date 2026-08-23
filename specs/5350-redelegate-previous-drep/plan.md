# Plan

## Constraints

- Reuse draft PR #5363 and its existing branch.
- Treat commit `5366766d1a8dc8f63a88f8bf6f55180eaf9232f7` and the preserved hostile-audit
  worktree as untrusted seed evidence; fresh Grok ownership is required.
- Preserve public API types and database state.
- Do not start a cold full-gate realization on this host. Focused warm tests,
  formatting, and linting are allowed; full verification is delegated to PR CI.

## Strategy

1. Rebase the legacy draft onto current `master` through a fresh planning base.
2. Add a RED regression bundle that distinguishes historical from effective
   delegation state and exercises both accepted and rejected outcomes.
3. Make the smallest pure and IO changes needed to select the effective state.
4. Run the focused gate, format/lint checks, and independent audit before push.

## Slice

- **S1 — effective DRep comparison:** tasks T1–T3, one bisect-safe behavior
  commit.
