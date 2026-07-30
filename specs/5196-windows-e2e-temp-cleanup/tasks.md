# Tasks: #5196 Windows E2E temp cleanup

**Input**: [spec.md](spec.md), [plan.md](plan.md)

Canonical numeric task IDs only (`T###`). Trailers must match
`Tasks: T###[, T###]`.

## Bootstrap (orchestrator)

- [X] T001 Write `specs/5196-windows-e2e-temp-cleanup/spec.md`
- [X] T002 Write `plan.md` and `tasks.md`
- [X] T003 Install untracked `./gate.sh` (+ runtime copy under
      `/tmp/ms-cw-tech-debt/t5196/gate.sh`)
- [X] T004 Open draft PR linked to #5196

## Slice A — Windows workflow scoping (driver+navigator)

- [X] T005 Demonstrate focused checker RED on baseline `windows-e2e.yml`,
      then implement FR1–FR3 (TEMP/TMP/TMPDIR → `${{ runner.temp }}`,
      `if: always()` cleanup under job-private root only, sibling-job safety
      comment), then GREEN + actionlint + `./gate.sh`; one bisect-safe commit
      `fix(ci): scope Windows E2E temp and always clean job-private dirs`

## Slice S3 — Fail-closed cleanup (driver+navigator)

- [X] T006 Demonstrate gate RED on soft cleanup, then make the
      `if: always()` cleanup fail-closed (missing RUNNER_TEMP fails;
      enumeration/deletion errors fail; re-enumerate `e2e*` /
      `test-cluster*` and fail on residuals; emit
      `Cleanup verified: 0 residual E2E temp directories`); GREEN +
      actionlint + `./gate.sh`; one bisect-safe commit
      `fix(ci): fail Windows E2E cleanup on residual directories`

## Orchestrator docs/gate remediation

- [X] T007 Correct spec/plan/tasks so SC4 live cancel proof is mandatory
      pre-merge with named artifact; extend untracked gate so soft cleanup
      is RED; commit docs+task correction only

## Finalization (orchestrator — incomplete until all hold)

- [X] T008 Stamp tasks, independently gate + final audit, push rebased
      branch via explicit SSH lease, refresh PR body, **leave draft**
- [X] T009 SC4 live cancel proof artifact on exact remote head
      (`sc4-cancel-proof.md`) + exact-head CI green (desk-owned live
      cancel; ticket owner records/verifies)
