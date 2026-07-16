# Implementation Plan: Bound Unit Test Memory

**Branch**: `fix/5309-unit-memory-leak` | **Date**: 2026-07-15 | **Spec**:
[spec.md](spec.md)
**Input**: [GitHub issue #5309](https://github.com/cardano-foundation/cardano-wallet/issues/5309)

## Summary

Make the list-monad `applyNM` property resource-bounded without weakening its
semantic coverage, then restore the intended 2 GiB heap ceiling and four RTS
capabilities in all six affected test executables. Verify the original focused
seed goes RED then GREEN and run the monolithic wallet unit suite under
`-M2G -N4` as the final memory smoke.

## Technical Context

**Language/Version**: Haskell, GHC 9.12.3
**Primary Dependencies**: Hspec 2.11.17, QuickCheck 2.15.0.1, Cabal 3.16, Nix
**Storage**: N/A
**Testing**: Hspec/QuickCheck plus compiled RTS configuration inspection
**Target Platform**: Linux acceptance proof; Cabal defaults shared with Windows
and macOS test bundles
**Project Type**: Multi-package Haskell monorepo
**Performance Goals**: Full wallet unit suite completes under a 2 GiB heap cap
with four RTS capabilities
**Constraints**: Preserve all examples, list-monad branching coverage, and
explicit command-line RTS overrides
**Scale/Scope**: One property module and six Cabal test stanzas; no production
wallet behavior

## Constitution Check

- **Maintenance-first stability**: PASS — fixes a destructive test/CI failure.
- **Reproducible builds**: PASS — all build and test evidence uses the Nix-owned
  development environment.
- **Comprehensive testing**: PASS — deterministic RED/GREEN, focused group,
  compiled defaults, and monolithic-suite smoke are required.
- **Code quality gates**: PASS — Fourmolu/Cabal formatting and affected builds
  remain in `gate.sh`.
- **Risk boundary**: PASS — no production modules, APIs, persistence, or ledger
  behavior change.

The post-design check is unchanged: the design touches only test generation
and test-executable configuration and introduces no constitutional exception.

## Root Cause

`prop_applyNM_iterate @[] @Int` combines an arbitrary `NonNegative Int` with
an arbitrary `Fun Int [Int]`. List bind grows multiplicatively, allowing one
QuickCheck case to create tens of millions of cons cells. Heap profiling shows
list cons occupying approximately 995 MB of a 1 GiB heap immediately before
failure. See [research.md](research.md) for commands and measurements.

## Slice Design

### Slice 1 — Bound list-property generation

Keep the displayed Hspec example names stable, but route the two list-monad
examples through specialized properties. Convert generated function outputs
from a bounded sum such as `Maybe (Either Int (Int, Int))` into lists of length
0, 1, or 2, and generate iteration depth only from 0 through 5. The generic
Identity and Maybe properties remain unchanged.

This commit is independently usable: it removes the runaway property while
leaving existing executable defaults untouched.

### Slice 2 — Restore RTS safety defaults

Replace each repeated pair of `-with-rtsopts` values with the single quoted
`"-with-rtsopts=-M2G -N4"` form in all six affected Cabal files. Rebuild and
inspect every corresponding executable.

This commit depends on Slice 1: applying the 2 GiB default first would turn the
existing runaway property into a deterministic heap-exhaustion failure.

## Project Structure

```text
lib/unit/test/unit/Control/Monad/UtilSpec.hs
lib/unit/cardano-wallet-unit.cabal
lib/cardano-wallet-read/cardano-wallet-read.cabal
lib/crypto-primitives/crypto-primitives.cabal
lib/primitive/cardano-wallet-primitive.cabal
lib/std-gen-seed/std-gen-seed.cabal
lib/wai-middleware-logging/wai-middleware-logging.cabal
specs/5309-unit-memory-leak/
├── spec.md
├── research.md
├── plan.md
├── quickstart.md
└── tasks.md
```

**Structure Decision**: Reuse the existing unit property and Cabal package
stanzas. No new production module, data model, or external contract is needed.

## Verification Strategy

1. RED: focused seed `1` exhausts a 256 MiB heap on the pre-fix property.
2. GREEN: the same named example and seed pass under the same cap.
3. Run the full `Control.Monad.Util` group.
4. Run `./gate.sh` and the monolithic suite under `-M2G -N4`.
5. Rebuild all six affected test executables and inspect `+RTS --info` for the
   exact `-M2G -N4` compiled default.
6. Re-run the monolithic suite without supplying `-M` or `-N`, proving the
   compiled defaults are both effective.

## Generated Artifacts

No data model or interface contract applies. No agent context update is needed
because the plan introduces no technology or project convention.
