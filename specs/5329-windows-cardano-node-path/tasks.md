# Tasks — #5329

## Slice A — ship `cardano-node.exe` in the `wallet-unit` win64 bundle

- [X] T5329-S1 Record the RED baseline: `nix derivation show
      .#ci.artifacts.win64.tests.wallet-unit` references zero
      `cardano-node-exe-*` derivations; capture the raw output in `WIP.md`.
- [X] T5329-S2 Capture the pre-change `nix derivation show
      .#ci.artifacts.win64.integration` output to a file outside the repo, for
      the SC2 byte-identity comparison.
- [X] T5329-S3 Add `windowsPackages.cardano-node` to the `wallet-unit`
      `extraPkgs` list in `flake.nix`. No other attribute touched.
- [X] T5329-S4 GREEN: the SC1 probe now returns the identical drv path that
      `integration` returns; the SC2 comparison shows `integration` unchanged.
- [X] T5329-S5 `./gate.sh` green; commit as
      `ci(windows): bundle cardano-node.exe with the wallet-unit test artifact`
      with trailer `Tasks: T5329-S1, T5329-S2, T5329-S3, T5329-S4, T5329-S5`.

## Slice B — put the bundle directory on `PATH` in `unit-tests`

- [ ] T5329-S6 Record the RED-skip rationale in `WIP.md` (no test harness for
      workflow YAML; proof is the mechanical `gate.sh` assertion + the CI run).
- [ ] T5329-S7 In `.github/workflows/windows.yml`, add `shell: pwsh` and
      `$env:PATH = "$(Get-Location);$env:PATH"` to the `unit-tests` job's "Run
      tests" step, mirroring `integration-smoke`. No other job or step touched.
- [ ] T5329-S8 GREEN: `./gate.sh` green, including the YAML-parse and
      step-content assertions.
- [ ] T5329-S9 Commit as
      `ci(windows): put the test bundle on PATH for unit-tests`
      with trailer `Tasks: T5329-S6, T5329-S7, T5329-S8, T5329-S9`.

## Slice C — finalization (orchestrator-owned, no driver)

- [ ] T5329-S10 PR body audit: plain-language narrative, both slices, explicit
      residual-risk paragraph on `windows.yml` not running on `pull_request`.
- [ ] T5329-S11 Dispatch `windows.yml` against the branch
      (`gh workflow run windows.yml --ref fix/5329-windows-cardano-node-path`)
      and record the run URL + outcome in the PR body — green or not.
- [ ] T5329-S12 `chore: drop gate.sh (ready for review)`, then `gh pr ready`.
