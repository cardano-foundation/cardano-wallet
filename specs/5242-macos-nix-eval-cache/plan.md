# Plan

## Scope

This is one vertical, bisect-safe implementation slice. It changes only:

- `.github/workflows/ci.yml`
- `.github/workflows/macos-boot-sync.yml`

## Slice 1: persist macOS evaluation caches

Add a job-level `env` override to each of the five macOS jobs in `ci.yml`.
Change the boot-sync workflow's macOS-only workflow-level value. Use the runner
account's persistent cache directory and keep `${{ github.job }}` isolation to
avoid reintroducing cross-job SQLite lock contention.

Proof consists of:

1. a focused checker observed failing on the base files;
2. the same checker passing on the edited files;
3. Actionlint, formatting, HLint, and the Nix build gate passing locally;
4. three consecutive successful live macOS build-gate runs without the
   reported repository error.

No runner provisioning, cache migration, cleanup command, Nix expression, or
non-macOS workflow behavior is in scope.
