# Tasks — Keep focused Cabal test recipes limited to local test suites

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5358

## Planning (ticket owner)

- [x] T001 Verify the lane, refresh the recipe facts, and reproduce Cabal-7043
      on the untouched base
- [x] T002 Derive the locally-declared test-suite oracle from `cabal.project`
      and the local `.cabal` files
- [x] T003 Author `spec.md` and `plan.md`
- [x] T004 Record the `just ci` substitution ruled by A-001
- [x] T005 Freeze the executable slice gate and prove it can fail

## Slice `local-test-targets` (PAIR)

- [ ] T010 Add `scripts/ci/check-local-test-targets.sh` reconciling the suites
      selected by the Cabal test recipes against the suites declared by local
      project packages
- [ ] T011 Observe the detector fail on the unfixed `justfile`, naming
      `cardano-balance-tx:unit` — the slice RED
- [ ] T012 Prove the detector refuses to pass vacuously when either side parses
      to the empty set
- [ ] T013 Remove `cardano-balance-tx:unit` from `unit-tests-cabal-match`
- [ ] T014 Observe the detector pass and
      `just unit-tests-cabal-match "Store"` exit 0 with a non-zero executed
      example count and no `Cabal-7043`
- [ ] T015 Confirm the selection still covers all 11 locally testable suites
      listed in `spec.md`
- [ ] T016 Wire the detector into the existing `quality-checks` matrix in
      `.github/workflows/ci.yml`
- [ ] T017 Confirm `scripts/shellcheck.sh` covers the new script and passes

## Acceptance (ticket owner)

- [ ] T020 Re-run the frozen gate independently, including the negative control
- [ ] T021 Run the A-001 local check set at the accepted head
- [ ] T022 Rebase on current master, re-run the gate and local checks at the
      exact rebased head
- [ ] T023 Confirm a fresh exact-head GitHub rollup has zero failing or pending
      required checks
