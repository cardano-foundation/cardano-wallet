# 5423 — Tasks

## S-1 — `cardano-api` closure and suppression ratchet

- [ ] **T-1** RED: a proof that the three rows can each be made to go red, and
      that the instrument's self-checks can return FAIL, executed and captured
      before any of M-1 exists in a form that could pass it.
- [ ] **T-2** `scripts/ci/cardano-api-closure-gate.sh` (M-1): population
      discovery, stanza-aware edge extraction with `common`/`import:`
      attribution, the two closure rows, the suppression row, per-row licences,
      the fixture self-check, and the ratchet with its one-directional exit.
- [ ] **T-3** `scripts/ci/cardano-api-closure-negative-control.sh` (M-2): three
      independent seedings, per-row measured deltas, exit bound to those deltas.
- [ ] **T-4** `.github/workflows/cardano-api-closure.yml` (M-3): own concurrency
      group, no `needs:`, control before gate, no secrets reference.
- [ ] **T-5** Land-time measurement of all three rows, mechanically captured,
      each `MAX` set to its measured value with no slack, and the measurement
      recorded for the PR body.
- [ ] **T-6** Evidence bundle: pristine green, three seeded reds, three slack
      runs green, self-check forced to FAIL, shellcheck, YAML parse, fence
      check, and the INV-17 no-build assertion — each with its own exit status
      read immediately.

## S-1 repair — submission 1 findings

- [ ] **T-7** F-1 / INV-18: `build_fixture` instantiates all six (file class ×
      spelling) cells, and the fixture self-check fails if any single matcher or
      file class is dropped.
- [ ] **T-8** F-2 / INV-19: `cardano-api-closure-negative-control.sh` asserts
      every pinned stdout line on the pristine run (V2-1), exercises the
      advisory fall branch per row requiring exit 0 and a `RATCHET SLACK` line
      naming that row (V2-2), and binds the `GATE RED` line to the row that rose
      (V2-3).
- [ ] **T-9** V2-5: M-1 prints `excluded build-tool-depends = <n>`, asserted by
      M-2 under V2-1.
- [ ] **T-10** Re-measure all three rows on the repaired tree and confirm each
      `MAX` still equals its measured value.

## S-2 — close the second audit's findings (fresh owner campaign)

- [ ] **T-11** V3-1 / INV-4: the `ghc-options:` matcher reads the **field**, not
      the head line; the fixture carries both shapes per `.cabal`-class cell.
- [ ] **T-12** V3-2 / INV-20: one declaration of file classes and spellings; the
      matchers are built from it, so `cells` covers a matcher's whole extent in
      both directions.
- [ ] **T-13** V3-3 / INV-21: M-2 binds exit 2 to an input — both self-test break
      families and at least two refusal populations — against a positive control.
- [ ] **T-14** V3-4 / INV-22: M-2 asserts each conditional line absent for the
      rows whose condition does not hold.
- [ ] **T-15** V3-5 / INV-23: M-2 asserts each licence line's text.
- [ ] **T-16** Re-measure all three rows field-scoped and confirm each `MAX`
      still equals its measured value.
