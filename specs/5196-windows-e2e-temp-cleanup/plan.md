# Plan — #5196 Windows E2E temp cleanup

## Scope

Behavior-changing work touches only `.github/workflows/windows-e2e.yml`.

Orchestrator-owned: `specs/5196-windows-e2e-temp-cleanup/{spec,plan,tasks}.md`
and untracked `./gate.sh`.

## Task model (canonical numeric IDs)

| ID | Role |
|----|------|
| T001–T004 | Bootstrap (spec, plan/tasks, gate, draft PR) |
| T005 | Scope TEMP/TMP/TMPDIR + always cleanup |
| T006 | Fail-closed cleanup + residual assertion |
| T007 | Docs/gate remediation (SC4 mandatory) |
| T008 | Finalization (stamp, push, draft) |
| T009 | Live cancel proof (desk-owned SC4) |

## Slice A — scoping (T005)

Owned file: `.github/workflows/windows-e2e.yml`.
Subject: `fix(ci): scope Windows E2E temp and always clean job-private dirs`
Trailer: `Tasks: T005`

## Proof

Focused shell checker in `./gate.sh` is the RED/GREEN instrument for YAML.
