# Plan — #5329

## Where the pieces live

| Thing | File | Lines (at `61cf408476`) |
|---|---|---|
| Windows test-bundle builder | `nix/windows-test-exe.nix` | whole file (26 lines) |
| `win64.tests.wallet-unit` definition | `flake.nix` | 484–496 |
| `win64.integration` definition (the working reference) | `flake.nix` | 520–539 |
| `unit-tests` job + "Run tests" step | `.github/workflows/windows.yml` | 144–309 (step at 306–309) |
| `integration-smoke` PATH pattern (the reference) | `.github/workflows/windows.yml` | 95–104 |

## The mechanism, already present

`nix/windows-test-exe.nix` takes `extraPkgs` and, for each, copies
`${pkg}/bin/*.exe` and `${pkg}/bin/*.dll` into the flat bundle with
`cp -RLnf` (`-n` = no-clobber, so the test exe's own DLLs always win).
`wallet-unit` already passes `extraPkgs = [ windowsPackages.cardano-cli ]`;
`integration` passes `[ local-cluster, cardano-wallet, cardano-node,
cardano-cli ]`. So the fix is to add the node to `wallet-unit`'s existing
list — **no change to `windows-test-exe.nix` itself, and no change to
`integration`**.

`windowsPackages.cardano-node` is
`cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-node`
(`flake.nix:461`), i.e. exactly the same derivation `integration` consumes.

On the workflow side, `integration-smoke` already demonstrates the runtime half:
`working-directory: smoke-bundle` + `shell: pwsh` +
`$env:PATH = "$(Get-Location);$env:PATH"`. The `unit-tests` "Run tests" step has
neither the explicit shell nor the PATH line.

`LOCAL_CLUSTER_CONFIGS: test\data\cluster-configs` is already set job-wide in
`unit-tests` and already resolves correctly (`working-directory: test-bundle`,
and the bundle ships `test/data/cluster-configs` via the existing
`testDataDirs = [ ./lib/unit/test/data, ./lib/local-cluster/test/data ]`).
That is the #5328 failure mode and it does **not** apply here — do not touch it.

## Verification strategy (the interesting part)

A full `nix build .#ci.artifacts.win64.tests.wallet-unit` needs **1007
derivations** built locally (measured: whole Haskell world cross-compiled to
mingw32). That is not a usable per-slice gate on a dev box.

Instead the gate works at **eval level**, which is seconds and just as
conclusive for the question "does the bundle contain cardano-node.exe":

```sh
nix derivation show .#ci.artifacts.win64.tests.wallet-unit \
  | grep -oE '/nix/store/[a-z0-9]+-cardano-node-exe-[^"]*\.drv'
```

Measured baseline on `master`: **0 matches** for `tests.wallet-unit`, **1
match** for `integration`
(`…-cardano-node-exe-cardano-node-x86_64-w64-mingw32-11.0.1.drv`). After the
fix the two must produce the *same* drv path. Because
`windows-test-exe.nix` unconditionally copies `${pkg}/bin/*.exe` for every
`extraPkgs` entry, a referenced node drv is a sound proxy for
`cardano-node.exe` landing in `$out`.

SC2 (integration untouched) is checked the same cheap way: the full
`nix derivation show .#ci.artifacts.win64.integration` output must be
byte-identical before and after.

## Slices

Two commits, in this order. Each is bisect-safe: slice A alone only makes the
artifact bigger (harmless), slice B alone would be a no-op PATH prepend, and
B-after-A is the working fix.

### Slice A — `nix`: ship `cardano-node.exe` in the `wallet-unit` win64 bundle

- Edit: `flake.nix`, the `wallet-unit` attribute only — append
  `windowsPackages.cardano-node` to its existing `extraPkgs` list, and adjust
  the neighbouring comment if one is warranted.
- RED: the SC1 probe returns 0 matches (record the raw output).
- GREEN: the SC1 probe returns the same drv path as `integration`'s, and the
  SC2 byte-identity check on `integration` passes.
- Owned files: `flake.nix`.

### Slice B — `ci`: put the bundle directory on `PATH` in `unit-tests`

- Edit: `.github/workflows/windows.yml`, the `unit-tests` job's "Run tests"
  step only — add `shell: pwsh` and a `$env:PATH = "$(Get-Location);$env:PATH"`
  line before `${{ matrix.command }}`.
- RED: no test harness for workflow YAML; RED is skipped and replaced by a
  mechanical assertion carried in `gate.sh` (YAML parses; the step has both the
  shell and the PATH line). This is a documented `resolve-ticket` exception.
- Owned files: `.github/workflows/windows.yml`.

### Orchestrator-owned finalization (no driver)

- PR body audit incl. the residual-risk paragraph.
- `gh workflow run windows.yml --ref fix/5329-windows-cardano-node-path` for
  SC4. `workflow_dispatch:` takes no inputs and dispatches the workflow file as
  it exists on the chosen ref, so this **is** reachable pre-merge; the run's
  result goes in the PR body either way.
- `chore: drop gate.sh (ready for review)`.

## Explicitly forbidden in every slice

`nix/windows-test-exe.nix`, the `integration` / `e2e` / `release` attributes,
any other `tests.*` attribute, `macos-unit-tests.yml`, `ci.yml`, `flake.lock`,
`cabal.project`, any `lib/` source, `gate.sh`, `specs/`.
