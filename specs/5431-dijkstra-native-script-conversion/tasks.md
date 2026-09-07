# Tasks

## Slice 1 — widen the conversion

- [ ] T001 `toWalletScript` accepts `NativeScript era` under `AllegraEraScript era`, with the `Timelock` equality constraint removed.
- [ ] T002 Existing callers compile unchanged and the eras already served keep their behaviour.

## Slice 2 — the guard case (blocked on the open decision in spec.md)

- [ ] T003 A Dijkstra guard script has an explicit total outcome; no reachable `error` remains on the path.
- [ ] T004 If the outcome requires it, `AnyScript` and `AnyExplicitScript` carry it and the swagger is regenerated.

## Slice 3 — delete both stubs

- [ ] T005 The Dijkstra arm of the mint/burn script map decoder calls the shared conversion; its `error` is deleted.
- [ ] T006 The Dijkstra arm of the explicit script decoder does the same; its `error` is deleted.
- [ ] T007 A Dijkstra native script built from the shapes shared with Timelock converts to the same wallet script its Conway counterpart does.
- [ ] T008 The exercise covers a guard script, and where it is a property its generated population is shown to contain one.
- [ ] T009 The census total on the tree is two below the base's and the declared ratchet equals it.
