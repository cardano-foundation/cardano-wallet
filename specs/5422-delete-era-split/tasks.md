# Tasks — 5422

Slice **S1** (one bisect-safe commit).

- [ ] **T5422-01** — RED: a test reaching `mkLedgerTx` at `RecentEraDijkstra`
      executes and fails against the unmodified base, with the failure and its
      exit code captured. (R-2, INV-3)
- [ ] **T5422-02** — RED: the extent guard over `allRecentEras` is shown able to
      fail. (INV-4)
- [ ] **T5422-03** — Delete the `case era of` in `mkLedgerTx` and return the
      body directly; trim exactly what the deletion orphans. (R-1, D-1, D-2)
- [ ] **T5422-04** — GREEN: the T5422-01 test passes and the T5422-02 guard
      passes. (R-2)
- [ ] **T5422-05** — Lower the ratchet default in
      `scripts/ci/dijkstra-stub-gate.sh` from 39 to 38, and show the census
      reporting `total = 38`, `MAX=38`, no `RATCHET SLACK`, exit 0 under
      `DIJKSTRA_STUB_STRICT=1`. (R-4, INV-1)
- [ ] **T5422-06** — Show the census can still go red: the negative control
      reports `delta=1`, `gate_exit=1`, exit 0. (INV-2 instrument)
- [ ] **T5422-07** — Sweep site 22 `joinStakePoolDelegationAction`: legs A and B,
      verdict recorded. (R-3, INV-5)
- [ ] **T5422-08** — Sweep site 23 `guardJoin`: legs A and B, verdict recorded.
- [ ] **T5422-09** — Sweep site 24 `guardEraIsConway`: legs A and B, verdict
      recorded.
- [ ] **T5422-10** — Sweep site 27 `installScriptWitnesses`: legs A and B,
      verdict recorded.
- [ ] **T5422-11** — Sweep site 28 `certificateFromDelegationActionLedger`:
      legs A and B, verdict recorded.
- [ ] **T5422-12** — Sweep site 29 `certificateFromVotingActionLedger`:
      legs A and B, verdict recorded.
- [ ] **T5422-13** — One pointer comment per non-deletable site, containing no
      occurrence of the token `error`. (US-2, plan.md's census constraint)
- [ ] **T5422-14** — The `-Werror` build, `hlint` and format-check legs are
      green on the candidate, and `hlint` is measured on the **base** as a
      control so a pre-existing hint is not attributed to this slice. (D-2)
