# Issue #5242: persistent macOS Nix evaluation caches

## User story

As a maintainer, I want macOS CI to retain its Nix evaluation cache outside
`/tmp` so periodic operating-system cleanup cannot corrupt the cache's bare Git
repository and make otherwise-correct builds fail.

## Requirements

- FR1: Every macOS job in `.github/workflows/ci.yml` overrides
  `XDG_CACHE_HOME` with
  `/Users/gha-runner/.cache/nix-eval-cache/${{ github.job }}`.
- FR2: `.github/workflows/macos-boot-sync.yml` uses the same persistent,
  per-job cache root.
- FR3: The workflow-level Linux cache root in `.github/workflows/ci.yml`
  remains `/tmp/nix-eval-cache/${{ github.job }}`.
- FR4: Workflow syntax remains valid.

## Acceptance

- A focused checker must fail against the original workflow state and pass
  after the change.
- Actionlint must pass for both affected workflow files, allowing the
  repository's custom runner labels.
- The full local gate must pass before push.
- `Build Gate (macOS)` must complete successfully for three consecutive
  branch runs with no `could not find repository` error for `tarball-cache`.
