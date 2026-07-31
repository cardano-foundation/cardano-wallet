# Spec — #5063 Use published `delta-types` and `delta-store`

## Problem

`cardano-wallet` still vendors four packages under `lib/delta-*` even though
`delta-types` and `delta-store` are maintained upstream and published on
Hackage, and `delta-chain` / `delta-table` (upstream: `sqlite-table`) are no
longer consumed in-tree.

## Out of scope

- Milestone #113 / Drop cardano-api and all cardano-api removal work.
- Silent CHaP, cardano-node, or unrelated dependency bumps.
- Publishing a new delta-store release (bounds fixes belong upstream).

## Proven dependency facts (2026-07-30)

| Package | In-tree | Published | Index |
|---|---|---|---|
| `delta-types` | `lib/delta-types` v1.0.0.0 | Hackage **1.0.0.0** (uploaded 2025-03-27) | visible at `hackage.haskell.org 2026-03-26T20:21:33Z` |
| `delta-store` | `lib/delta-store` v0.2026.7.23 | Hackage **1.0.0.0** (uploaded 2025-03-27) | same |
| `delta-chain` | `lib/delta-chain` | not required by wallet | — |
| `delta-table` | `lib/delta-table` | not required by wallet | — |

Neither package is on CHaP (HTTP 404). Resolution path is **Hackage only**.
The reviewed Hackage archives are:

- `delta-types-1.0.0.0.tar.gz`
  (`sha256:0fdb3b6572487c6a4ec407cb7c09b4688343b8979168c846f23d0890cbbb0bed`)
- `delta-store-1.0.0.0.tar.gz`
  (`sha256:d8048e15430e6e6eb667fd4d935ce7f17907f3449ce3935e5272d06fc8c504a8`)

### Compatibility notes that shape the plan

1. **Module rename:** published `delta-store` exposes `Test.Data.Store`;
   in-tree consumers import `Test.Store`. Unit tests must rename imports.
2. **`io-classes` bound:** published `delta-store` requires
   `io-classes >=1.4 && <1.8`; wallet pins `io-classes ==1.8.0.1`.
   Narrow `allow-newer: delta-store:io-classes` is required (same shape as
   existing `allow-newer` entries). No other package versions change.
3. **Production API:** consumer imports of `Data.Delta`, `Data.Store`,
   `Data.DBVar`, `Data.Delta.Update` remain valid against published modules.
4. **No in-tree consumers** of `delta-chain` / `delta-table` packages
   (local `DeltaChain` types in network-layer tests are unrelated).

### Semantic compatibility audit

The published sources were compared with the vendored sources at the original
PR merge base (`6e3baf8421`). After normalizing formatting and documentation:

1. `Data.Store`, `Data.DBVar`, and `Data.Delta.Update` have equivalent
   executable code for the APIs used by wallet.
2. Published `Data.Delta.List` adds `Semigroup` and `Monoid` instances for
   `DeltaList`; wallet has no consumer of that type, and the change is
   additive.
3. Published `Data.Delta.Embedding` no longer re-exports
   `Data.Semigroupoid`. Wallet does not import that re-export, and the
   umbrella `Data.Delta` module still exports it.
4. The published test helper changes from `Test.Store` to
   `Test.Data.Store`, drops an unnecessary `Buildable` constraint, and wraps
   its test monad in a newtype. The wallet's eleven consumer specs use the
   same exported law/DSL operations after the import rename.

The narrow `allow-newer` is therefore supported by compilation and consumer
tests against `io-classes ==1.8.0.1`, not merely by the vendored package's
formerly unbounded dependency.

### Test coverage ownership

Externalizing a package also externalizes its package-internal tests. This PR
does not claim that Cabal runs dependency test suites:

- the deleted `delta-types` suite contained one dummy `True` assertion;
- the deleted `delta-chain` and `delta-table` suites discovered no specs;
- the deleted `delta-store` suite covered `CachedStore` laws and equivalence;
  the published archive retains equivalent package tests and adds DBVar tests;
- wallet retains eleven real Store consumer specs. The focused Store run
  executes 39 examples, including `prop_StoreUpdate` against wallet stores,
  with no assertion or generator changes in this PR.

Thus wallet-specific semantic coverage remains in wallet CI. Package-internal
coverage belongs to the published delta project and is not silently counted
as part of wallet CI.

## User stories

1. As a maintainer, I depend on published delta packages so wallet no longer
   ships a private fork of libraries owned by
   https://github.com/cardano-foundation/delta-types.
2. As a reviewer, I can prove resolution from Hackage (freeze/plan evidence)
   and prove local trees are gone (negative controls).

## Functional requirements

- **FR1** Delete `lib/delta-types`, `lib/delta-store`, `lib/delta-chain`,
  `lib/delta-table` entirely.
- **FR2** Remove the four `packages:` entries and four `package delta-*`
  stanzas from `cabal.project`.
- **FR3** Resolve `delta-types` and `delta-store` from Hackage at the
  existing index-state; pin versions via constraints and/or cabal bounds
  (`==1.0.0.0` or equivalent).
- **FR4** Add the minimal `allow-newer: delta-store:io-classes` needed for
  the published package against the existing `io-classes ==1.8.0.1` pin.
- **FR5** Update consumer `build-depends` and rename `Test.Store` imports
  to `Test.Data.Store` in unit tests.
- **FR6** Remove project-local unit-test wiring for the four packages
  (`nix/project-package-list.nix`, `flake.nix` unit/windows outputs,
  `justfile` unit list, `.github/workflows/{ci,windows,macos-unit-tests}.yml`).
- **FR7** Leave CHaP index-state, cardano-node, and cardano-api pins
  unchanged.

## Success criteria

1. Four `lib/delta-*` directories absent at HEAD.
2. `cabal.project` has no `lib/delta-*` package paths and no
   `package delta-*` stanzas.
3. Freeze/plan evidence shows `delta-types-1.0.0.0` and
   `delta-store-1.0.0.0` from Hackage (not a local path).
4. Consumers build without local-source leakage.
5. Ticket gate passes: absence greps (with positive+negative controls),
   focused consumer unit tests, and
   `nix build .#ci.artifacts.linux64.release`.
6. Negative controls prove the absence instruments can fail (seeded
   local package / path reintroduction is detected).
7. Required exact-head CI is green after rebasing to the current target
   branch. An unrelated failure may be diagnosed and rerun, but is not
   silently waived at merge handoff.

## Acceptance mapping (issue checkboxes)

| Issue checkbox | Covered by |
|---|---|
| Four directories gone | FR1, SC1 |
| cabal.project cleaned | FR2, SC2 |
| Resolved from Hackage/CHaP + freeze | FR3–FR4, SC3 |
| release artifact + unit/integration | SC5 |
