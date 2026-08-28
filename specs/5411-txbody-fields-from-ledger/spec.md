# spec — #5411 retire the cardano-api transaction-body deprecation sites

Issue: #5411. Parent: #5243 → #5237. Milestone: M1 — Drop cardano-api (#113).
Sibling: #5412 (the `lib/integration` suppression, fenced out of this ticket).

## Observable outcome

The `cardano-api` transaction-body round-trip in
`lib/wallet/src/Cardano/Wallet.hs` is deleted in favour of reading the four
fields — inputs, outputs, collateral, withdrawals — from the ledger
transaction, and the `-Wno-deprecations` pragmas that covered it are gone,
verified by a build under a named sanctioned path.

## Requirements

- **R-1** `buildCoinSelectionForTransaction` reads `txIns`, `txOuts`,
  `txInsCollateral` and `txWithdrawals` from its `Write.Tx era` argument
  without converting to `cardano-api`.
- **R-2** The named files carry no deprecation-suppression pragma afterwards.
- **R-3** The values the migrated sites read are unchanged.

## Invariants

Each has a failure meaning and a success meaning. None is satisfied by the
absence of a warning.

| ID | invariant | fails when | passes when |
|---|---|---|---|
| **INV-1** | No `Cardano.Api.Experimental` import is added anywhere in the diff | the diff adds such an import | the diff adds none |
| **INV-2** | No new type models the transaction-body boundary | the diff introduces a record/newtype/data carrying those fields | the four values are bound directly |
| **INV-3** | Differential equality: for the same input transaction, the four values read from the ledger equal the four values read through `cardano-api` | any field differs | all four agree over the generated population |
| **INV-4** | The named files carry no deprecation-suppression pragma, in **any** spelling | a pragma remains, or the build fails when it is removed | the build is green under a named sanctioned path with the pragmas absent |
| **INV-5** | No file under `lib/integration/**` is modified | any such path appears in the diff | none does |
| **INV-6** | Both suppression populations are counted separately, each with its instrument named | a single total is reported | pre-existing and bump-widened are reported apart |
| **INV-7** | The PR body carries the base SHA from the first commit, and the orphan check distinguishes all four outcomes | a transport error resolves to a verdict | `0` / `1` / `75` are distinct and a transport error is `75` |

**INV-1 is a distinct invariant, not a consequence of the others.** Satisfying
the deprecation warnings is not the goal; an `Experimental` import satisfies
every other gate here while moving the milestone backwards.

**INV-3 replaces a byte-preservation criterion, which could not fail at this
site.** `buildCoinSelectionForTransaction` returns a `CoinSelection` and
serialises nothing — no `ByteString`, no CBOR, no encode. A byte criterion
would have to be satisfied by exercising a different path.

## Definition of done — executable form

These files carry no `-Wno-deprecations` pragma, verified by build under a
named sanctioned path:

- `lib/wallet/src/Cardano/Wallet.hs`
- `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`
- `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs`

plus whatever this ticket's own build-based measurement adds — no number is
inherited, including from the issue.

**Excluded, and neither is a gap:** `lib/cardano-api-extra/lib/Cardano/Api/Gen.hs`
retires with #5290; `lib/integration/scenarios/…/Shelley/TransactionsNew.hs`
retires with #5412.

## Out of scope

1. No `Cardano.Api.Experimental` import.
2. No new type modelling the transaction-body boundary.
3. No file under `lib/integration/**`.
4. `lib/wallet/src/Cardano/Api/Extra.hs` is not deleted.
5. Bisect-safety, inherited from #5237.

## Sanctioned build paths

Exactly two. A bare `cabal build` is not a gate, because `-Werror` comes from
the release flag and not from the default cabal invocation.

| path | where `-Werror` comes from |
|---|---|
| CI / nix, `--flags=release` | `nix/haskell.nix:277-278`, `flags.release = true` — "Enable release flag (optimization and -Werror)" |
| `just build` | `justfile:50-51`, `--ghc-options="-Werror "` |

Every receipt names which one it used.

Under either path `-Werror` is on, so **removing a live suppression fails the
build rather than warning**. That diagnostic is the evidence — or the build
succeeds and the pragma suppressed nothing.
