# Tasks: Bound Unit Test Memory

**Input**: [spec.md](spec.md), [research.md](research.md), [plan.md](plan.md)
**Organization**: Two ordered, bisect-safe slices; one reviewed commit per
slice. Slice 2 depends on Slice 1.

## Slice 1 — Bound list-property generation (US1, US3)

**Independent test**: The focused seed-1 property completes under a 256 MiB
heap cap, the `Control.Monad.Util` group passes, and the monolithic suite
completes under an explicit 2 GiB cap.

- [X] T001 [US1] Record the deterministic heap-exhaustion RED command in WIP.md for `lib/unit/test/unit/Control/Monad/UtilSpec.hs`
- [X] T002 [US1] Replace unbounded list-valued generation with 0/1/2-result branching and iteration depth 0..5 in `lib/unit/test/unit/Control/Monad/UtilSpec.hs`
- [X] T003 [US3] Run focused GREEN, `Control.Monad.Util`, `./gate.sh`, and the monolithic bounded smoke from `specs/5309-unit-memory-leak/quickstart.md`
- [X] T004 [US1] Commit Slice 1 as `test: bound applyNM list properties` with `Tasks: T001, T002, T003, T004`

## Slice 2 — Restore RTS safety defaults (US2, US3)

**Independent test**: All six rebuilt test executables report the compiled
`-M2G -N4` default, and the monolithic suite passes using those defaults.

- [X] T005 [US2] Record the pre-fix compiled-default RED evidence for the six affected Cabal test components in WIP.md
- [X] T006 [US2] Collapse repeated `-with-rtsopts` declarations in the six Cabal files listed in `specs/5309-unit-memory-leak/plan.md`
- [X] T007 [US2] Rebuild all six test components and verify each `+RTS --info` reports `-M2G -N4`
- [X] T008 [US3] Run `./gate.sh` and the monolithic default-options smoke from `specs/5309-unit-memory-leak/quickstart.md`
- [X] T009 [US2] Commit Slice 2 as `test: restore bounded RTS defaults` with `Tasks: T005, T006, T007, T008, T009`

## Dependencies and Execution Order

1. Slice 1 removes the runaway generated case while preserving list semantics.
2. Slice 2 may then make the intended 2 GiB ceiling effective without turning
   the existing property into a new deterministic failure.
3. Finalization follows only after both commits pass fresh orchestrator gates
   and every task is checked in the commit that implements it.

There is no safe parallel implementation opportunity because Slice 2's runtime
ceiling depends on Slice 1's bounded property.
