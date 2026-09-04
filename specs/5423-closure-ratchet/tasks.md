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
