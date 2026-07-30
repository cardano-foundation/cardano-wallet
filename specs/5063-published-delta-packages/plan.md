# Plan — #5063 published delta packages

## Approach

One vertical, bisect-safe slice: remove the four vendored trees, rewire the
build to Hackage `delta-types-1.0.0.0` / `delta-store-1.0.0.0`, adapt the
small consumer import rename, and drop CI/nix unit targets that only existed
for the vendored packages.

## Version / index decisions (evidenced)

| Decision | Choice | Evidence |
|---|---|---|
| Source index | Hackage only | CHaP returns 404 for both packages |
| Versions | `delta-types == 1.0.0.0`, `delta-store == 1.0.0.0` | sole published versions; local types already 1.0.0.0 |
| `index-state` hackage | keep `2026-03-26T20:21:33Z` | packages uploaded 2025-03-27; already visible |
| `index-state` CHaP | keep `2026-07-20T14:49:58Z` | no change needed |
| Bound conflict | `allow-newer: delta-store:io-classes` | published cabal has `io-classes <1.8`; project pins `1.8.0.1`; local tree already builds against 1.8 |
| No source-repository-package | prefer published Hackage tarballs | issue asks for published versions |

Escalate only if: Hackage packages fail to build under GHC 9.12.3 with the
allow-newer relaxation, or production API symbols diverge.

## Owned files (implementation slice)

Exact list for the driver:

### Deletes
- `lib/delta-types/` (entire tree)
- `lib/delta-store/` (entire tree)
- `lib/delta-chain/` (entire tree)
- `lib/delta-table/` (entire tree)

### Manifest / build wiring
- `cabal.project` — drop four `packages:` lines; drop four `package delta-*`
  stanzas; add `allow-newer: delta-store:io-classes`; add constraints
  `delta-types ==1.0.0.0`, `delta-store ==1.0.0.0`
- `nix/project-package-list.nix` — remove the four package name strings
- `flake.nix` — remove `unit-delta-*` attrs and windows `delta-*` test map entries
- `justfile` — remove `delta-*:unit` from `unit-tests-cabal-match`
- `.github/workflows/ci.yml` — remove delta unit build/run matrix entries
- `.github/workflows/windows.yml` — same
- `.github/workflows/macos-unit-tests.yml` — same

### Consumer bounds / imports
- `lib/wallet/cardano-wallet.cabal` — bound `delta-types` / `delta-store` to `^>= 1.0` (or `==1.0.0.0`)
- `lib/primitive/cardano-wallet-primitive.cabal` — same for `delta-types`
- `lib/unit/cardano-wallet-unit.cabal` — same for both
- All unit specs importing `Test.Store` → `Test.Data.Store`:
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Info/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Wallets/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Wallets/LayerSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Delegations/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Meta/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/PrivateKey/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Transactions/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Submissions/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/Rewards/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/UTxOHistory/StoreSpec.hs`
  - `lib/unit/test/unit/Cardano/Wallet/DB/Store/WalletState/StoreSpec.hs`

## Forbidden scope

- Any cardano-api / drop-cardano-api work
- Changing CHaP rev, cardano-node pin, or unrelated constraint pins
- Editing `specs/`, `gate.sh`, PR metadata (orchestrator-owned)
- Pushing

## Slice shape

### Slice A — `published-delta` (single commit)

**RED (prove instruments + compile break):**

1. Add gate-local negative-control scripts under the worktree only if needed
   for development; the ticket `gate.sh` (untracked) already defines the
   absence/search instruments. Prefer demonstrating RED by:
   - Running the gate's "local path must not appear" checks against **current
     HEAD** (they fail while trees exist) — record that failure as RED.
   - Optionally a focused consumer unit pattern that still imports
     `Test.Store` once packages are removed but before the rename (ordered
     carefully so RED is observed).

   Practical RED sequence the pair should follow:
   1. Delete the four trees + cabal.project package entries first →
      `cabal build cardano-wallet-unit -O0` fails (missing packages /
      missing `Test.Store`).
   2. Freeze that failure evidence (`RED`).

**GREEN:**

1. Add Hackage constraints + `allow-newer: delta-store:io-classes`.
2. Bounds in the three consumer cabal files.
3. Rename all `Test.Store` imports to `Test.Data.Store`.
4. Drop nix/flake/justfile/CI unit wiring for the four packages.
5. Prove:
   - `cabal freeze` (or `cabal build --dry-run` plan) shows Hackage
     `delta-types-1.0.0.0` and `delta-store-1.0.0.0` with no local path.
   - `test -d lib/delta-types` etc. all fail.
   - Grep for `lib/delta-` package paths in `cabal.project` is empty
     (after positive control that the grep finds a seeded path).
   - Focused unit: `just unit-tests-cabal-match "Store"` or narrower
     pattern covering renamed imports.
   - Best-effort: `nix build .#ci.artifacts.linux64.release` and full unit
     suite; if either fails for an **unrelated** reason, stop with
     BLOCKED + durable evidence rather than widening scope.

**Commit subject (verbatim):**

```
chore: use published delta-types and delta-store

Tasks: T5063-S1
```

## Evidence the orchestrator will re-derive

- `git show --stat <sha>` file list matches owned-files
- Freeze/plan snippet for the two package ids
- Gate exit 0 on the exact commit
- Navigator `NAVIGATOR-VERIFIED <sha>`

## Queue position

Draft PR stays behind #5349, #5355, #5356 unless the parent desk reorders.
