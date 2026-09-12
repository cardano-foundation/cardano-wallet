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
and the available key roles contain no guard role. The type is not changed by
this ticket and no field is added to `AnyScript` or `AnyExplicitScript`.

## State invariants

- The six shared shapes convert to the same wallet script regardless of which
  era's native script they came from. Nesting is part of this: a shape holding
  sub-scripts converts its children by the same rule.
- A guard script has exactly one outcome, a named failure, and it occurs in one
  place.
