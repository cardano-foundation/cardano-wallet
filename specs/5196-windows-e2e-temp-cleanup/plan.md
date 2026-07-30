# Plan — #5196 Windows E2E temp cleanup

## Scope

Behavior-changing work touches only:

- `.github/workflows/windows-e2e.yml`

Orchestrator-owned artifacts:

- `specs/5196-windows-e2e-temp-cleanup/{spec,plan,tasks}.md`
- untracked `./gate.sh` (runtime copy `/tmp/ms-cw-tech-debt/t5196/gate.sh`)

## Task model (canonical numeric IDs)

| ID | Role |
|----|------|
| T001–T004 | Bootstrap (spec, plan/tasks, gate, draft PR) |
| T005 | Scope TEMP/TMP/TMPDIR + always cleanup |
| T006 | Fail-closed cleanup + residual assertion |
| T007 | Docs/gate remediation (SC4 mandatory) |
| T008 | Finalization (stamp, push, draft) |
| T009 | Live cancel proof (desk-owned SC4) |

## Landed so far (T005)

Slice A scoped TEMP/TMP/TMPDIR to `${{ runner.temp }}` and added an
`if: always()` cleanup step. That cleanup is **soft**: it can exit 0 when
`RUNNER_TEMP` is missing, silences enumeration/deletion errors, and does not
assert zero residual directories. Issue #5196 requires a cancelled-run proof;
soft cleanup cannot satisfy that.

## Correction slice (T006)

One forward correction commit (not an amend of T005):

### Required cleanup behavior

1. Missing/empty `RUNNER_TEMP` → non-zero exit.
2. Enumeration and deletion errors fail the step.
3. After delete, re-enumerate `e2e*` / `test-cluster*` under the job-private
   root.
4. Fail if any residual match remains.
5. On success print exactly:
   `Cleanup verified: 0 residual E2E temp directories`
6. Keep job-private root and name fence; do not alter smoke command, runner,
   timeout, build job, or triggers.

### Proof strategy (RED → GREEN)

Local instrument: untracked `./gate.sh` contract checker.

After the orchestrator docs/gate extension (T007), current head MUST be RED for
missing fail-closed postcondition signals.

Driver: RED → GREEN → one local commit; no push.
Subject: `fix(ci): fail Windows E2E cleanup on residual directories`
Trailer: `Tasks: T006`

## Live-boundary proof (T009, desk-owned after push)

After owner acceptance + explicit SSH push:

1. Dispatch `windows-e2e.yml` on the exact remote branch head.
2. Wait until the Windows smoke step is running.
3. Cancel the workflow.
4. Require cleanup step log contains
   `Cleanup verified: 0 residual E2E temp directories`.
5. Record run URL + excerpt in
   `/tmp/ms-cw-tech-debt/t5196/sc4-cancel-proof.md`.
6. Exact-head CI green; final audit; leave draft until then.

## Forbidden in every slice

`lib/`, `src/`, `app/`, `test/`, `nix/`, `flake.nix`, `flake.lock`,
`cabal.project`, other workflows, `gate.sh` (orchestrator), `specs/`
(orchestrator), milestone #113 / cardano-api paths.
