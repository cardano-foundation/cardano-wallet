# modules-model — #5411

No module is created, deleted, moved, or renamed. No dependency edge is added.
The change is subtractive: two `cardano-api` wrap/unwrap round-trips are
deleted, and the responsibility each of them borrowed from `cardano-api`
returns to the ledger types the code already holds.

## Changed responsibilities

| module | change | dependency direction |
|---|---|---|
| `Cardano.Wallet` | stops converting a ledger transaction to `cardano-api` to read four fields; stops naming `Cardano.TxBody` in two signatures | loses `cardano-api` uses; gains none |
| `Cardano.Wallet.Shelley.Transaction` | the unsigned-transaction builders stop producing a `cardano-api` body and produce the ledger transaction they already hold | loses `cardano-api` uses; gains none |
| `Cardano.Wallet.Api.Http.Shelley.Server` | stops applying the inverse converter to a value that was only ever wrapped for transport | loses one import and two applications |
| `Cardano.Api.Extra` | **unchanged** | — |

## Boundary that moves

`Cardano.Wallet` ⇄ `Cardano.Wallet.Api.Http.Shelley.Server` currently exchange a
`cardano-api` `TxBody` that the producer wraps and the consumer immediately
unwraps. After this ticket they exchange the ledger transaction directly.

**No abstraction is promoted and no carrier is introduced.** The type on that
boundary is one the ledger already defines and both sides already use; the
deleted converter application was not modelling anything.

## Explicitly not changed

- `Cardano.Api.Extra` keeps both converters and both keep other callers. It
  retires with #5290, not here.
- No package's `build-depends` changes. `cardano-api` leaves these call sites,
  not the dependency closure — that is #5290's outcome, and claiming it here
  would overstate the diff.
- Nothing under `lib/integration/**`; that surface is #5412's.
