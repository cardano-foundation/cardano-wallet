# Spec: Remove the stale Lean badge from README

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5340

## P1 user story

As a contributor reading the README, I see only CI badges that reflect a
real, current signal — so I don't waste time investigating a badge that
looks broken but isn't actually reporting anything.

## Problem

`.github/workflows/lean.yml` triggers on `push: branches: [master]` gated
by `paths: ['specifications/**.lean']`. No push to `master` has ever
touched that path, so the workflow has exactly one run ever (a manual
`workflow_dispatch` on a non-master branch, 2026-02-13). The README's
"Lean" badge queries `branch=master` and has therefore never had a run to
report — it shows "no status" permanently.

## Decision (owner-selected, see issue #5340)

Remove the "Lean" badge from `README.md`. The workflow itself
(`lean.yml`) is untouched — it still runs correctly on PRs/dispatch that
touch Lean spec files. Only the always-grey, uninformative README badge
goes.

## Functional requirements

- FR1: `README.md` no longer contains a badge/link referencing
  `lean.yml` or a "Lean" label.
- FR2: No other badge in the README is altered.
- FR3: The `<p align="center">...</p>` badge block remains well-formed
  HTML (no dangling `<a>` tag).

## Success criteria

- `grep -i lean README.md` returns no matches.
- The remaining 8 badges render identically to before (byte-identical
  aside from the removed block).
