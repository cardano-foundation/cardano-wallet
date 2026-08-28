# functions-model — #5411

Only signature-level changes. No bodies, no algorithms, no helpers.

The result type below is written `Write.Tx era` because that is the type the
producers already hold before wrapping and the consumers already hold after
unwrapping. **If the resolved artifacts show a different ledger type is the
correct one at that boundary, that is a signature discovery: version this
document before the work continues — do not improvise a different type and do
not introduce a new one.**

## Changed signatures

| function | module | from | to |
|---|---|---|---|
| `constructTransaction` | `Cardano.Wallet` | `… -> ExceptT ErrConstructTx IO (Cardano.TxBody (CardanoApiEra era))` | `… -> ExceptT ErrConstructTx IO (Write.Tx era)` |
| `constructUnbalancedSharedTransaction` | `Cardano.Wallet` | `… -> ExceptT ErrConstructTx IO (Cardano.TxBody (CardanoApiEra era))` | `… -> ExceptT ErrConstructTx IO (Write.Tx era)` |
| `mkUnsignedTransaction` | `Cardano.Wallet.Shelley.Transaction` | `… -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))` | `… -> Either ErrMkTransaction (Write.Tx era)` |
| `mkUnsignedTx` | `Cardano.Wallet.Shelley.Transaction` | `… -> Either ErrMkTransaction (Cardano.TxBody (CardanoApiEra era))` | `… -> Either ErrMkTransaction (Write.Tx era)` |
| `constructUnsignedTx` | `Cardano.Wallet.Shelley.Transaction` | result carries `Cardano.TxBody (CardanoApiEra era)` | result carries `Write.Tx era` |

Argument names and argument types are unchanged throughout. Every change is to
the **result** type only.

## Unchanged, and load-bearing that they stay unchanged

| function | why it must not change |
|---|---|
| `buildCoinSelectionForTransaction` | already takes `Write.Tx era` and returns `CoinSelection`. **Its signature is already correct** — this ticket deletes work from its `where` clause, nothing else. A signature change here would mean the round-trip was being treated as a representation problem. |
| `toCardanoApiTx`, `fromCardanoApiTx` | `Cardano.Api.Extra` is not edited. Call sites go; the module stays. |

## No new function

No function is added, including no helper that reads the four fields. The four
values are bound directly where they are used.

**A new function whose result groups the four fields is the fenced-out design
wearing a different hat** — the fence forbids a new *type* at this boundary, and
a helper returning a tuple or record of exactly those four fields reintroduces
it. If reading the fields directly appears to require such a helper, stop and
escalate rather than adding one.
