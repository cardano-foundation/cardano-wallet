# Tasks — 5422

One bisect-safe slice.

- [ ] **RED: the Dijkstra path fails first.** A test reaching `mkLedgerTx` at
      `RecentEraDijkstra` executes and fails against the unmodified base, with
      the failure and its exit code captured.
- [ ] **RED: the extent guard can fail.** The guard over `allRecentEras` is
      shown able to go red without being edited.
- [ ] **Delete the era match** in `mkLedgerTx` and return the body directly;
      trim exactly what the deletion orphans.
- [ ] **GREEN.** The Dijkstra test passes and the extent guard passes.
- [ ] **Move the ratchet.** Lower the default in
      `scripts/ci/dijkstra-stub-gate.sh` from 39 to 38, and show the census
      reporting `total = 38`, `MAX=38`, no `RATCHET SLACK`, exit 0 under
      `DIJKSTRA_STUB_STRICT=1`.
- [ ] **Show the census can still go red.** The negative control reports
      `delta=1`, `gate_exit=1`, exit 0.
- [ ] **Sweep `joinStakePoolDelegationAction`** (`Delegation.hs`): legs A and B,
      verdict recorded.
- [ ] **Sweep `guardJoin`** (`Delegation.hs`): legs A and B, verdict recorded.
- [ ] **Sweep `guardEraIsConway`** (`Delegation.hs`): legs A and B, verdict
      recorded.
- [ ] **Sweep `installScriptWitnesses`** (`Unsigned.hs`): legs A and B, verdict
      recorded.
- [ ] **Sweep `certificateFromDelegationActionLedger`** (`Unsigned.hs`): legs A
      and B, verdict recorded.
- [ ] **Sweep `certificateFromVotingActionLedger`** (`Unsigned.hs`): legs A and
      B, verdict recorded.
- [ ] **One pointer comment per non-deletable site**, containing no occurrence
      of the token the census counts.
- [ ] **Lint and format are green, with a base control.** The `-Werror` build,
      `hlint` and format-check legs pass on the candidate, and `hlint` is
      measured on the **base** as a control so a pre-existing hint is not
      attributed to this slice.
