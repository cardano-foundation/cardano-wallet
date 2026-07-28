# Plan: Remove the stale Lean badge from README

One slice — a single-file, non-code edit. No build/test tooling touches
this change; `gate.sh` checks the README directly.

## Slice A — remove the Lean badge block

Delete the `<a href=".../lean.yml">...<img .../></a>` block for the
"Lean" badge from `README.md`'s badge `<p align="center">` section.
Nothing else in the file changes.

Owned files: `README.md`.
