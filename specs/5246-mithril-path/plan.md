# Plan: Use mithril-client from PATH when available

## Tech Stack

- Haskell (`Cardano.Launcher.Mithril` module)
- `System.Directory.findExecutable` for PATH lookup
- Existing HTTP download logic as fallback

## Architecture

The change is localized to `lib/launcher/src/Cardano/Launcher/Mithril.hs`:

1. Add PATH lookup at the start of `downloadMithril`
2. If found, log and return immediately (no download)
3. If not found, proceed with existing download logic
4. Add stdout log output so the path taken is observable

Production wiring:

```haskell
downloadMithril =
    downloadMithrilWith findExecutable downloadMithrilFromGitHub
```

## Task ID map (canonical)

| ID | Meaning |
|----|---------|
| T001 | Planning/bootstrap (spec, plan, tasks) |
| T002 | PATH-hit unit proof (exact `"mithril-client"` + no download) |
| T003 | Fallback/working-directory unit proof |
| T004 | Implementation + observable branch logs |
| T005 | Focused gate/format |
| T006 | Production-wiring live PATH proof |
| T007 | Finalization |

## Slice Breakdown

### Slice 1: PATH lookup with fallback (T002–T005)

**Goal**: Implement PATH-first lookup with download fallback, with observable logging and non-vacuous unit proofs.

**Changes**:
- Import `System.Directory.findExecutable`
- At the start of resolution, call finder with `"mithril-client"`
- If `Just path`, log `Using mithril-client from PATH: <path>` and return `MithrilExePath path`
- If `Nothing`, log `mithril-client not found on PATH, downloading...` and call download with the supplied working directory
- Unit tests inject finder/download and assert exact lookup name, workdir forwarding, and fail-if-download-called on PATH hit

**Owned files**:
- `lib/launcher/src/Cardano/Launcher/Mithril.hs`
- `lib/launcher/test/unit/Cardano/Launcher/MithrilSpec.hs`
- `lib/launcher/cardano-wallet-launcher.cabal` (test module registration only)

**Gate**:
```bash
nix develop --quiet -c cabal test cardano-wallet-launcher:unit --test-show-details=direct
nix develop --quiet -c just check-fmt
```

**Commit**: `feat(launcher): check PATH for mithril-client before downloading`  
**Trailer**: `Tasks: T002, T003, T004`

### Slice 2: Production-wiring / boundary verification (T006)

**Goal**: Prove production `downloadMithril` uses PATH when available, without claiming a full E2E snapshot download.

**Truthful boundary**:

1. **Static E2E call site** (unchanged product wiring):

   ```haskell
   downloadLatestSnapshot dir =<< downloadMithril dir
   ```

   at `lib/integration/exe/e2e.hs` (launch path inside `launchNodeAndWalletViaMithril` / `configureContext`).

2. **Live production probe** (not `e2e -- --help`):

   Call the real library entrypoint:

   ```haskell
   downloadMithril
       "/definitely/nonexistent/t5246-download-fallback-must-not-run"
   ```

   Expected under `nix develop` with flake-provided `mithril-client` on PATH:

   - stdout contains `Using mithril-client from PATH: …`
   - resolved path is under the nix store (flake pin)
   - impossible working directory proves fallback did **not** run (fallback would fail before HTTP)

3. **Do not claim** a full E2E preprod snapshot download was executed.

**Invalid (vacuous) command — do not use**:

```bash
# WRONG: hspec --help never enters aroundAll / downloadMithril
cabal run e2e -- --help
```

**Owned files**: None for code (verification + docs only).

**Commit**: No code commit required for T006 when probe is recorded in WIP/desk evidence.

## Dependencies

- Slice 2 (T006) depends on Slice 1 implementation (T004)
- No external dependency changes

## Risks

- **Risk**: `findExecutable` may not find mithril-client in some environments  
  - **Mitigation**: Fallback download remains
- **Risk**: PATH hit trusts any `mithril-client` without version check  
  - **Mitigation**: Acceptable under FR5; flake pin is SoT inside `nix develop`
