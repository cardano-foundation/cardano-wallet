# Tasks — #5406

## SL-1 — land the instrument, its falsification harness, and the workflow

- [ ] **T-001** Land the frozen instrument at `scripts/ci/dijkstra-stub-gate.sh`,
      byte-identical to
      `sha256:6304802d788cd8371fd0ec0214e23e083c77e892966b34a7884663e4e66ae79f`,
      mode `0755`. (REQ-1, REQ-2, INV-INSTRUMENT-INTACT, INV-DENOMINATOR)
- [ ] **T-002** Add `scripts/ci/dijkstra-census-negative-control.sh`: seed one
      throwaway Dijkstra stub under `<root>/lib/`, require the gate to exit 1,
      remove the seed on every exit path. (REQ-4, INV-CENSUS-RED)
- [ ] **T-003** Add `.github/workflows/dijkstra-census.yml`: `pull_request` +
      `push` to `master` + `workflow_dispatch`, no `needs:`, no secret,
      own `concurrency` group; runs T-002 then T-001.
      (REQ-3, REQ-7, INV-CENSUS-RUNS, INV-NO-SECRETS)
- [ ] **T-004** Verify locally, in this order, and freeze the output:
      negative control exit 0; `./scripts/ci/dijkstra-stub-gate.sh` → 44/15,
      exit 0; `DIJKSTRA_STUB_MAX=0 ./scripts/ci/dijkstra-stub-gate.sh` → exit 1;
      `shellcheck -e SC1090 --external-sources` on both scripts → exit 0;
      `./scripts/enforce-eol.sh` → exit 0. (REQ-1, REQ-5)
- [ ] **T-005** Prove the committed tree changes no Haskell and no existing
      file: `git diff --stat` against the base shows exactly the three new files
      plus this `specs/` directory, and `git diff --name-only <base>..HEAD |
      grep -c '^lib/.*\.hs$'` is 0. (REQ-6, INV-NO-HASKELL)

## Invariants

| ID | holds when | fails visibly when |
|---|---|---|
| `INV-CENSUS-RUNS` | the `Dijkstra Stub Census` check appears in the PR's run log and is not gated on another job | the workflow is present but never executed |
| `INV-CENSUS-RED` | one extra Dijkstra stub under `lib/` makes the gate exit 1, re-proved on every CI run | the seeded control passes or is skipped |
| `INV-CENSUS-GREEN` | the unmodified tree exits 0 at `MAX=44` | `master` goes red without a stub being added |
| `INV-INSTRUMENT-INTACT` | landed sha256 equals `6304802d…e66ae79f`; both built-in controls still run every invocation | the instrument is edited, or its controls are removed |
| `INV-DENOMINATOR` | file selection is `find "$lib" -name '*.hs' -type f`, unconditional; `Cardano/Api/Extra.hs` still counted | any exclusion mechanism appears |
| `INV-RATCHET-ONE-LINE` | `44` appears as a ratchet value in exactly one line of the repository | the value is duplicated and can drift |
| `INV-NO-SECRETS` | the new workflow file contains no `secrets.` reference | a credential enters this ticket's surface |
| `INV-NO-HASKELL` | no `lib/**/*.hs` is added, changed or deleted in the commit | a stub is touched by child 0 |

## SL-1 repair — after audit submission 1 (report `1b711708…5f68314f`)

- [ ] **T-006** Bind the control's verdict to a measured delta: emit
      `seed=`, `pristine_total=`, `seeded_total=`, `delta=`, `gate_exit=` on
      stdout, and exit 0 only when the pristine run exited 0, `delta == 1`, and
      `gate_exit == 1`. The seeded file must introduce exactly one counted
      shape — none in its comments. (REQ-8, `INV-CONTROL-DELTA-ONE`)
- [ ] **T-007** Acquire ownership before arming cleanup: create the seed
      atomically, arm the trap only after that succeeds, and leave a rejected
      collision byte-identical on every exit path including `INT`/`TERM`/`HUP`
      and a census exiting 2. (REQ-9, `INV-CONTROL-COLLISION-SAFE`)
- [ ] **T-008** Make the census unsuppressable: no workflow-, job-, or
      step-level `if:`, and no `paths`/`paths-ignore` filter, on
      `.github/workflows/dijkstra-census.yml`.
      (REQ-10, `INV-CENSUS-UNSUPPRESSED`)
- [ ] **T-009** *(ticket owner)* Observe the census green in a live CI run at
      the exact pushed head, and paste the seeded-RED and tree-GREEN runs into
      the PR body. (REQ-11, `INV-LIVE-RUN`)

## Version 2 invariants

| ID | holds when | fails visibly when |
|---|---|---|
| `INV-CONTROL-DELTA-ONE` | the control measures the census total before and after seeding and requires the difference to be exactly 1 | the control passes while applying zero or two mutations, or while the census output was never parsed |
| `INV-CONTROL-COLLISION-SAFE` | a pre-existing file at the seed path survives byte-identical and the control exits non-zero | the control deletes a file it did not create |
| `INV-CENSUS-UNSUPPRESSED` | no `if:` or path filter can stop the census job or its steps | a suppressed job reports green by not running |
| `INV-LIVE-RUN` | the census check is green in the PR's run log at the exact pushed head | the workflow is present and the pipeline never ran it |
