# Spec — Keep focused Cabal test recipes limited to local test suites

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5358

## Problem

`just unit-tests-cabal-match` names twelve test suites explicitly. Eleven belong
to packages listed under `packages:` in `cabal.project`. One,
`cardano-balance-tx:unit`, belongs to a package that enters the build as a
`source-repository-package` (`cardano-balance-transaction`). Cabal cannot build
or run test suites of non-local dependencies, so the recipe aborts during target
resolution:

```text
Error: [Cabal-7043]
Cannot test the test suite 'unit' because the package cardano-balance-tx-0.1.0
is not local to the project, and cabal does not currently support building test
suites or benchmarks of non-local dependencies.
```

The failure happens before any suite runs, so the whole focused-match harness is
unusable: a developer asking for `"Store"` gets zero executed examples. This was
isolated while gating #5063, which had to work around it by invoking
`cabal test cardano-wallet-unit:unit` directly.

This is a developer-harness defect only. Repository CI does not use these
recipes — `.github/workflows/ci.yml` runs the Nix flake apps `.#unit-<pkg>` —
which is why CI stayed green while the recipe was broken.

## User story (P1)

As a cardano-wallet developer, I run a focused Cabal unit-test match and observe
matching local tests execute, without Cabal rejecting a non-local dependency's
test suite.

## Functional requirements

- **FR1** `just unit-tests-cabal-match <pattern>` selects only test suites of
  packages that `cabal.project` lists under `packages:`.
- **FR2** `just unit-tests-cabal-match "Store"` completes successfully and
  executes at least one matching `Store` example.
- **FR3** No locally testable suite that the recipe selected before this change
  is dropped. The selection changes by exactly one removal:
  `cardano-balance-tx:unit`.
- **FR4** The repository permanently detects a reintroduced non-local target.
  The detector is committed, runs in CI, and is derived from `cabal.project`
  rather than from a hardcoded package list, so it also catches a *new*
  non-local target that does not exist today.

## Rejection behavior

The detector must fail, with a message naming the offending `package:suite`,
when the recipe selects any suite whose package is not a local project package.
It must not fail for any selection that is entirely local.

## Observable success criteria

Mapped from the frozen issue body's acceptance criteria:

- **AC1** `nix develop --quiet -c just unit-tests-cabal-match "Store"` exits 0,
  executes at least one matching `Store` example, and emits no `Cabal-7043`.
- **AC2** Every suite selected by `unit-tests-cabal-match` is a locally testable
  project suite; `cardano-balance-tx:unit` is not selected while the package
  remains a non-local `source-repository-package` dependency.
- **AC3** The unfiltered `just unit-tests-cabal` recipe retains coverage of
  every locally testable suite the focused recipe covered before this change.
- **AC4** A negative control that restores a non-local test target fails the
  focused gate, proving the regression check can detect the original defect.
- **AC5** Formatting, lint, the focused gate, and repository CI pass at the
  exact proposed head.

## Baseline facts (verified at 75cd99bd1754cbda5e255d4bd38d0c6c7bc65c13)

Selected by the recipe today — 11 local, 1 non-local:

| Suite | Package location | Local? |
|---|---|---|
| `cardano-wallet-application-tls:unit` | `lib/application-tls` | yes |
| `cardano-balance-tx:unit` | `source-repository-package` | **no** |
| `cardano-numeric:unit` | `lib/numeric` | yes |
| `cardano-wallet-blackbox-benchmarks:unit` | `lib/wallet-benchmarks` | yes |
| `cardano-wallet-launcher:unit` | `lib/launcher` | yes |
| `cardano-wallet-network-layer:unit` | `lib/network-layer` | yes |
| `cardano-wallet-primitive:test` | `lib/primitive` | yes |
| `cardano-wallet-secrets:test` | `lib/secrets` | yes |
| `cardano-wallet-test-utils:unit` | `lib/test-utils` | yes |
| `cardano-wallet-unit:unit` | `lib/unit` | yes |
| `std-gen-seed:unit` | `lib/std-gen-seed` | yes |
| `wai-middleware-logging:unit` | `lib/wai-middleware-logging` | yes |

The repository declares 20 test suites across local packages. The recipe
deliberately selects a subset of 11; the other 9 need a node, a cluster, or are
otherwise out of the focused-unit-test scope. **FR3 forbids dropping any of the
11 — it does not require adding the other 9.** Widening the selection is out of
scope.

## Non-goals

- Changing, vendoring, or publishing `cardano-balance-tx`.
- Changing dependency pins, `source-repository-package` entries, or package
  semantics.
- Removing or redesigning `cardano-api`.
- Splitting or renaming the cardano-wallet unit-test suites.
- Adding the 9 currently unselected local suites to the recipe.
- Redesigning CI. The detector is added as one entry in the existing
  `quality-checks` matrix, matching how every other repository guard is wired.

## Review requirement

Pawel review is not required. This is a mechanical developer-test harness
correction with no product or wallet semantic change.
