# Data model

## Consumed

`NativeScript era` for an era with an `AllegraEraScript` instance. For eras up
to Conway this is `Timelock era` with six shapes. For Dijkstra it is
`DijkstraNativeScript era`, which has those six and one more, `RequireGuard`,
carrying a credential that may be either a key hash or a script hash.

## Produced

`Script KeyHash` from `cardano-addresses`, whose constructors are
`RequireSignatureOf`, `RequireAllOf`, `RequireAnyOf`, `RequireSomeOf`,
`ActiveFromSlot`, `ActiveUntilSlot`. None of them can carry a guard credential,
and the available key roles contain no guard role.

## State invariants

- The six shared shapes round-trip to the same wallet script regardless of which
  era's native script they came from.
- A guard script has exactly one outcome, and it is total. Which outcome is the
  open decision recorded in `spec.md`; the fields it may add to `AnyScript` and
  `AnyExplicitScript` are specified once that is settled, not before.
