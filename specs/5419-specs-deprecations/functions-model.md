# functions-model.md — #5419

Ceiling: 35 lines.

## New or changed production signatures

**None.** No production function is added, removed, or has its name, argument
names, argument types, result type, constraints, or effects changed.

## Test-local helpers

The repair centralizes era handling that the rejected candidate had specialized
per property. Those helpers are **test-local to the two spec files**.

Their exact names, arguments, and types are **deliberately not specified here.**
That is the commit owner's to design and own. The ticket owner ruled the era
dispatch incidental rather than forced (Q-001/A-001) and explicitly declined to
ratify a design; specifying signatures here would be the ticket owner writing
the implementation it just refused to write.

What is binding on those helpers is not their shape but three constraints,
which live where they can be enforced:

- they live **inside** the two spec files — `modules-model.md`, INV-2;
- they must not collapse the construction path and the observation path of an
  assertion onto one source, which would make the property unable to
  distinguish correct from broken — INV-3;
- they must not reduce the stub census by removing an arm that still represents
  undone work — INV-4 and `spec.md` R-4.

## Signatures relied upon, not changed

Named in the commit owner's `handoffs/reliance.md` and re-verified in its
Q-001 evidence: `mkShelleyWitnessLedger` and `sealWriteTx` are already
polymorphic under `Write.IsRecentEra`, and `fromCardanoApiTx` is typed
polymorphically with one pre-existing centralized Dijkstra stub. This ticket
consumes them; it does not alter them.
