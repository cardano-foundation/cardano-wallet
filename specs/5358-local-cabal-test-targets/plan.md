# Plan — Keep focused Cabal test recipes limited to local test suites

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5358
Base: `75cd99bd1754cbda5e255d4bd38d0c6c7bc65c13`

## Shape of the change

Two things must land together, in one bisect-safe commit:

1. **The detector** — a committed script that reconciles the suites named in
   `justfile`'s Cabal test recipes against the set of test suites declared by
   local project packages, and fails naming any non-local selection.
2. **The fix** — removing `cardano-balance-tx:unit` from
   `unit-tests-cabal-match`.

They land together because either alone leaves the tree red: the detector alone
fails CI on the unfixed recipe, and the fix alone leaves the defect undetected
and free to return. Within the slice the detector is written *first* and
observed failing on the unfixed recipe — that failure is the slice's RED.

## Why a derived detector rather than an assertion about one package

A check that hardcodes "`cardano-balance-tx:unit` must be absent" would pass
forever after the fix and could never fail again for the reason we care about.
It would be a check that cannot fail — manufactured confidence.

The detector instead computes both sides:

- **declared-local** — parse `packages:` from `cabal.project`, then read each
  directory's `.cabal` file for `test-suite` stanzas, producing the set of
  `package:suite` pairs Cabal can actually run locally;
- **selected** — parse the `package:suite` arguments out of the `cabal test`
  invocation in the `unit-tests-cabal-match` recipe;

and asserts `selected ⊆ declared-local`. This is
`reconcile(declared, observed)` with a comparator that fails whenever anyone
adds a target Cabal cannot run — including a target that does not exist today.
It stays live because both sides are recomputed from the repository, not frozen
into the script.

The detector must also refuse to pass vacuously: if either side parses to the
empty set, that means the parse broke, not that the invariant holds, and it must
exit non-zero. Both directions get a negative control (see the gate).

## Reachability

A detector that exists but is never executed reports green regardless of what it
asserts. It is therefore wired into `.github/workflows/ci.yml`'s existing
`quality-checks` matrix, the same place `check-code-format.sh`,
`check-haskell-nix-cabal.sh`, `enforce-eol.sh` and `shellcheck.sh` are wired.
That is one added matrix entry, not a CI redesign.

`scripts/shellcheck.sh` already lints repository shell scripts, so the new
script inherits shell linting automatically — confirm this rather than assume
it.

## Invariants

- **INV1** Every `package:suite` the recipe names is runnable by Cabal from this
  project. Enforced by the detector; falsified by the negative control.
- **INV2** The focused match actually runs examples. Exit 0 is not sufficient:
  the recipe passes no `--fail-on=empty`, so a pattern matching nothing would
  exit 0 and look identical to success. The gate must observe a non-zero
  executed-example count for `"Store"`.
- **INV3** The set of locally testable suites the recipe covers does not shrink.
  Enforced by comparing the post-change selection against the frozen baseline
  list of 11 in `spec.md`.
- **INV4** The detector cannot pass by parsing nothing.

## Live boundary

The real boundary is Cabal's target resolution — the thing that emitted
`Cabal-7043`. Static reconciliation (INV1) is fast and general but is a *model*
of that boundary; it cannot prove Cabal accepts the result. So the gate also
runs the real recipe end to end (AC1/INV2). Both are required: the static check
generalizes, the live run proves the model matches reality.

## Slices

One behavior slice. It is small but relational — it spans a shell script, a
justfile recipe, and a CI workflow — so it runs as PAIR, not LIGHT.

### Slice `local-test-targets`

Owned files:

- `scripts/ci/check-local-test-targets.sh` (new)
- `justfile` (remove exactly one selection line)
- `.github/workflows/ci.yml` (add exactly one `quality-checks` matrix entry)

Forbidden: `cabal.project`, any `.cabal` file, any Haskell source, any other
recipe, any other workflow or workflow job, dependency pins.

Order inside the slice:

1. Write the detector.
2. Run it against the **unfixed** justfile → must fail naming
   `cardano-balance-tx:unit`. This is RED.
3. Run the detector's negative controls → must fail (see gate section 4).
4. Remove `cardano-balance-tx:unit` from the recipe.
5. Run the detector → passes.
6. Run the real focused recipe → exits 0 with non-zero `Store` example count.
7. Wire the detector into `quality-checks`.

## Risks

- **Cost.** Post-fix, the focused recipe must actually build 11 suites under
  `cabal` in the dev shell. The failing baseline reproduction alone consumed
  3.66 GiB on `/`. Every build command is metered per the machine-resource
  contract; the expensive live section is run by the driver once for GREEN and
  by the ticket owner once at acceptance, never concurrently.
- **Pre-existing hang.** A project note records `cardano-wallet-unit:unit`
  hanging after all specs pass on master. If the focused run reproduces that,
  it is a pre-existing condition, recorded honestly and escalated, not a ticket
  failure — and the `"Store"` match is a small subset, which should avoid it.
- **Parser brittleness.** The detector parses `justfile` and `.cabal` text. INV4
  and its negative controls exist precisely because a silently-broken parser
  would otherwise report green.

## Verification

See the frozen gate under the ticket runtime root. In summary: static
reconciliation, live focused run with a non-vacuity assertion, unfiltered
selection comparison against the frozen baseline of 11, the negative control,
and the repository's format/lint/quality checks.

### There is no `just ci` in this repository

Planning documents from older tickets refer to a `just ci` recipe. It does not
exist in `justfile` at this ticket's base, and `specs/007-ledger-minting`
already records that `just ci` was never equivalent to the flake checks.
Repository CI is `.github/workflows/ci.yml`, which has no single-command local
form.

The local full-CI stand-in for this ticket is therefore the set of checks the
`quality-checks` matrix actually invokes and that this diff can affect:

- `nix develop --quiet -c scripts/ci/check-code-format.sh`
- `nix develop --quiet -c bash -c 'hlint lib'`
- `nix develop --quiet -c scripts/ci/check-haskell-nix-cabal.sh`
- `./scripts/shellcheck.sh`
- `nix develop --quiet -c scripts/enforce-eol.sh`

plus the ticket-focused Cabal proof, the coverage oracle, and the falsified
negative control. The fresh exact-head GitHub check rollup remains the
authoritative full repository CI result. Nothing here adds a new recipe.
