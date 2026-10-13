# functions-model — #5288 / #5427

New and changed signatures only.

| ID | Signature | Constraint |
|---|---|---|
| F1 | `buildCardanoApiParityTx :: RecentEra Write.Conway -> SelectionOf TxOut -> ScriptWitnesses -> ScriptParityCtx -> Write.Tx Write.Conway` | Reaches its result through `Cardano.Api.Experimental.makeUnsignedTx`. Its call graph must not reach `buildLedgerTx`, `buildLedgerTxRaw`, `mkLedgerTx` or `mkUnsignedTx`. Replaces `buildLegacyParityTx`. |
| F2 | `buildNewParityTx` — unchanged signature | Remains the wallet-ledger arm. |
| F3 | `reviewResponseTx :: Coin -> Write.Tx Write.Conway` — unchanged signature | Changed body: builds via F2, not via the cardano-api arm. |
| F4 | `shouldHaveBodyParity`, `propParity` — unchanged signatures | Changed bodies: the first argument arm becomes F1. Comparison operators unchanged. |

`ScriptParityCtx`, `parityTtl`, `parityFee`, `parityBodyCbor`,
`parityScriptWits` keep their current signatures.

Deleted with F1's arrival: `buildLegacyParityTx` and `mintBurnFromMintingSources`
if it loses its last caller. A caller sweep is part of the slice, not a
follow-up.
