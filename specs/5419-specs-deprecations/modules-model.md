# modules-model.md — #5419

Ceiling: 40 lines.

## New or changed production modules

**None.** This is the complete statement, not an omission.

No production module changes responsibility, gains a dependency, loses one, or
is promoted to a different owner. The two changed modules are unit-test specs
and are leaves: nothing imports them.

## Dependency direction

The intended change **removes** edges rather than adding any. Both spec modules
depend today on the deprecated `cardano-api` transaction-body surface via
`Cardano.Api.Extra`'s converters. Retiring those call sites moves the modules
onto the ledger-native accessors they already reach for elsewhere, so the
`cardano-api` edge weakens. It cannot be removed outright here: `Gen.hs` and
other pre-existing bridge stubs keep it alive until #5290 deletes the shim.

Direction is one-way and unchanged: spec modules -> `lib/wallet` production
modules -> ledger. Nothing in this ticket may invert that.

## Promotion

**Forbidden in this ticket.** If retiring a site appears to require a shared
abstraction promoted into a production module — a new type modelling the
transaction-body boundary, or a helper lifted out of a spec into
`lib/wallet/src` — that is M1 architecture and it stops here and escalates
(`spec.md` R-6, INV-2). Any centralized helper this ticket needs lives **inside
the spec file that uses it**.

## Referenced, not duplicated

Fields and state: `data-model.md`. Signatures: `functions-model.md`.
