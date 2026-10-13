# Functions model

## Changed

```
toWalletScript
    :: AllegraEraScript era
    => (Hash "VerificationKey" -> KeyRole)   -- tokeyrole
    -> NativeScript era                      -- script
    -> Script KeyHash
```

The `NativeScript era ~ Timelock era` equality is removed and the argument
becomes `NativeScript era`. The result type is unchanged. Callers whose era
predates Dijkstra are unchanged, because `NativeScript era` is already
`Timelock era` for them.

Its unmatched branch is reachable for Dijkstra and raises an error naming the
era. There is exactly one such branch in the codebase after this change.

## Unchanged signatures whose bodies change

`fromLedgerScriptToAnyScriptDijkstra` and `dijkstraAnyExplicitScript` keep
their types and call the conversion. `dijkstraAnyExplicitScript` uses the
witness-count context argument it currently ignores.

## New, in the test suite only

```
genDijkstraSharedNativeScript :: Gen (NativeScript DijkstraEra)
genDijkstraGuardNativeScript  :: Gen (NativeScript DijkstraEra)
```

Standalone generators, not `Arbitrary` instances. The first produces only the
six shapes shared with `Timelock`, including nested ones. The second produces
guard scripts over both credential forms.
