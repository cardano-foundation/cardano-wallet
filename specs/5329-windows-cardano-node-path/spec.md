# Spec — #5329 Windows CI: `cardano-node.exe` never provided to the `wallet-unit` job

## Problem in one paragraph

On Windows CI the `wallet-unit / Shelley` shard runs a pre-built `unit.exe`
downloaded as a bare artifact onto a plain `windows-2025-vs2026` runner. One of
its tests — `NetworkLayer regression test #1708 / Parallel local socket
connections` — starts a real `cardano-node` child process. Nothing in that job
ever puts a `cardano-node.exe` on `PATH` or into the bundle directory, so the
test fails deterministically with
`ProcessDidNotStart "cardano-node" ... does not exist`. macOS and Linux do not
hit this because they wrap the run in `nix shell ... .#cardano-node -c ...`;
Windows has no nix on the test runner by design.

## P1 user story

As a cardano-wallet maintainer watching the Windows `push:master` workflow, I
want the `wallet-unit` Windows test bundle to ship the `cardano-node.exe` its
tests spawn, and the job to make that binary discoverable, so that
`wallet-unit / Shelley` reflects real wallet behaviour instead of failing on a
missing dependency.

## Supporting user stories

- As a CI maintainer, I want the fix to reuse the mechanism that already works
  for `ci.artifacts.win64.integration`, so there is one pattern to reason about
  rather than two.
- As a reviewer, I want the change verifiable without a multi-hour Windows
  cross-compile, so the PR can be checked cheaply and repeatedly.
- As a maintainer of the currently-green `integration-smoke` job, I want its
  bundle and its behaviour left byte-identical.

## Functional requirements

- **FR1** — The `ci.artifacts.win64.tests.wallet-unit` derivation MUST contain
  `cardano-node.exe` (the Windows cross-build already used by
  `ci.artifacts.win64.integration`) alongside `unit.exe`.
- **FR2** — The `unit-tests` job in `.github/workflows/windows.yml` MUST place
  the downloaded bundle directory on `PATH` before invoking the test command,
  mirroring `integration-smoke`'s `$env:PATH = "$(Get-Location);$env:PATH"`.
- **FR3** — `ci.artifacts.win64.integration` and
  `ci.artifacts.win64.tests.*` other than `wallet-unit` MUST be unchanged: same
  derivation hash inputs, same contents, same job behaviour.
- **FR4** — The `nix/windows-test-exe.nix` builder MUST NOT need new
  parameters; the existing `extraPkgs` mechanism is the intended extension
  point (`wallet-unit` already uses it for `cardano-cli`).
- **FR5** — All 16 matrix shards of `unit-tests` MUST keep working; the PATH
  change is job-wide and must not disturb shards that do not spawn a node.

## Non-goals

- macOS/Linux behaviour, `macos-unit-tests.yml`, `ci.yml` — owned by #5328.
- `ci.artifacts.win64.tests.wallet-network-layer` or any other shard that might
  separately want a node; only `wallet-unit` is in evidence in #5329.
- Changing which tests run, their match patterns, or their sharding.

## Success criteria

- **SC1** (locally checkable, eval-only) — `nix derivation show
  .#ci.artifacts.win64.tests.wallet-unit` references the same
  `cardano-node-exe-cardano-node-x86_64-w64-mingw32-*.drv` that
  `nix derivation show .#ci.artifacts.win64.integration` references. Baseline
  today: `wallet-unit` references it **0** times — this is the RED.
- **SC2** (locally checkable, eval-only) — the derivation-input sets of
  `ci.artifacts.win64.integration` before and after the change are identical.
- **SC3** — `.github/workflows/windows.yml` parses as valid YAML and the
  `unit-tests` "Run tests" step prepends the working directory to `PATH` under
  an explicit `pwsh` shell.
- **SC4** (real proof, CI-only) — a run of `windows.yml` against the branch
  shows `wallet-unit / Shelley` green and all other `wallet-unit / *` shards
  still green.

## Residual risk (recorded up front, must be restated in the PR body)

`windows.yml` triggers only on `push: branches: [master]` and
`workflow_dispatch`. A pull request does **not** run it, and
`workflow_dispatch` on this repo dispatches the workflow file **as it exists on
the selected ref** — so a dispatch against `fix/5329-windows-cardano-node-path`
is the only pre-merge route to SC4. If that dispatch is unavailable to the
orchestrator, SC4 remains unproven at merge time and the PR must say so
plainly rather than claim victory on SC1–SC3.
