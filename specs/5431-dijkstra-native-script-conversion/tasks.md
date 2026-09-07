# Tasks

## Slice 1 — widen the conversion

- [ ] T001 `toWalletScript` accepts `NativeScript era` under `AllegraEraScript era`, with the `Timelock` equality constraint removed and the result type unchanged.
- [ ] T002 Its unmatched branch names the Dijkstra era and states that the script has no wallet representation, without claiming to identify which shape it caught.
- [ ] T003 Existing callers compile unchanged and the eras already served keep their behaviour.

## Slice 2 — route both readers through it

- [ ] T004 The Dijkstra arm of the mint/burn script map decoder calls the shared conversion; its `error` is deleted.
- [ ] T005 The Dijkstra arm of the explicit script decoder calls the shared conversion with the witness-count context it currently ignores; its `error` is deleted.
- [ ] T006 A Dijkstra native script built from the six shapes shared with `Timelock`, including nested shapes, converts to the same wallet script its Conway counterpart converts to.
- [ ] T007 The generated population for T006 is shown to contain Dijkstra native scripts of each shared shape, and to contain nested ones.
- [ ] T008 A Dijkstra guard script is shown to be constructible over both credential forms and to reach the named failure.
- [ ] T009 The census total on the tree is one below the merge base's and the declared ratchet equals it.
