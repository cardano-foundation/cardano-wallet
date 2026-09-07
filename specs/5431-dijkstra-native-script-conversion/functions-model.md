# Functions model

Changed signature:

```
toWalletScript
    :: AllegraEraScript era
    => (Hash "VerificationKey" -> KeyRole)   -- tokeyrole
    -> NativeScript era                      -- script
    -> Script KeyHash
```

The `NativeScript era ~ Timelock era` equality is removed and the argument
becomes `NativeScript era`. Callers whose era predates Dijkstra are unchanged,
because `NativeScript era` is already `Timelock era` for them.

The result type is provisional: the open guard decision may replace
`Script KeyHash` with a type that can also express an unconvertible script. No
other signature in this ticket is settled until then, and none is recorded here
on speculation.
