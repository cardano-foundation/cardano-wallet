# plan — #5411

## Base and stack

```
master
 └── chore/drop-iohk-monitoring          #5402  2f2e5ed99f47969bf41b11fcc7e9044103b22f65
      └── chore/issue-5397-node-11-1-0   #5399  33ccafc23c571fe31f09dceef3b091c0b5161cc7
           └── refactor/5411-txbody-fields-from-ledger
```

**Base SHA at branch time: `33ccafc23c571fe31f09dceef3b091c0b5161cc7`**
(`origin/chore/issue-5397-node-11-1-0`, read 2026-08-28; `origin/master` was
`0a7332482c49537d8169a7a3424a6ce72329c6d5` at the same reading).

Recorded here in the first commit, and in the PR body, so the orphan check
below has an input from the start rather than retrofitted.

### The orphan check

This branch is orphaned by a **rebase of #5399**, not by its merge — the rebase
happens first and nothing announces it. The check has four outcomes and only a
cleanly distinguishable result is a verdict.

```sh
base=33ccafc23c571fe31f09dceef3b091c0b5161cc7

git ls-remote --exit-code origin refs/heads/chore/issue-5397-node-11-1-0 >/dev/null 2>&1
case $? in
  0)  git fetch -q origin chore/issue-5397-node-11-1-0 || exit 75
      if git merge-base --is-ancestor "$base" FETCH_HEAD 2>/dev/null
      then echo "INTACT";                                    exit 0
      else echo "TRIGGER FIRED: rebased";                     exit 1
      fi ;;
  2)  echo "TRIGGER FIRED: merged-or-deleted";                exit 1 ;;
  *)  echo "NO VERDICT: transport error — retry, do not act"; exit 75 ;;
esac
```

`75` is `EX_TEMPFAIL` (`sysexits.h`). It is not a spare number: `git ls-remote
--exit-code` already uses `2` for *ref absent*, which this script maps to
*trigger fired*, so reusing `2` for *no verdict* would put two different
meanings on one integer a layer apart. A transport error must never be read as
"merged-or-deleted".

Acting on a fired trigger is not this lane's unit; the lane records the input.

## Measurement — the two populations

**Instrument.** `git grep -nIE 'Wno-deprecat|no-warn-deprecat|Wno-warnings-deprecat'`
over all tracked files at each tree, then filtered **structurally** to
`OPTIONS_GHC` pragma lines in `.hs` files.

The structural filter is required, not tidiness: `ERA-CHANGES.md` and `TODO.md`
both match the raw pattern **as prose**, so a pattern-only instrument run over a
tree that documents its own subject matches the discussion.

The narrower pattern `Wno-deprecations` alone is **wrong**: it misses the
`-fno-warn-deprecations` spelling, which three pre-existing files use.

| population | count | files |
|---|---|---|
| **pre-existing** — suppressed on `origin/master@0a7332482c` | **7** | `Faucet/Addresses.hs`, `ApiSpec.hs`*, `Wai/Middleware/Logging.hs`*, `Cardano/DB/Sqlite.hs`*, `Shelley/Transaction.hs`, `Shelley/Transaction/Ledger.hs`, `Shelley/Transaction/Unsigned.hs` |
| **bump-widened** — suppressed only on `33ccafc2` | **5** | `Cardano/Wallet.hs`, `TransactionsNew.hs`, `TransactionLedgerSpec.hs`, `TransactionSpec.hs`, `Api/Gen.hs` |

`*` = spelled `-fno-warn-deprecations`.

Total on the branch is **12**, observed directly. `7 + 5 = 12` is two
independent observations agreeing, not a sum.

**This pragma census is not the deprecated-surface count.** Counting the
surface is done by build — removing a suppression and reading what `-Werror`
rejects — because the surface is dominated by record fields and accessors that
no type-name grep can see.

## What the pinned cardano-api actually deprecates

Read from the `DEPRECATED` pragmas of `cardano-api ==11.5.0.0`
(`cabal.project:175`), source realised from the store tarball, with
`getTxBodyContent` used as a positive control for the method.

Relevant to this ticket's sites:

| symbol | deprecated? |
|---|---|
| `TxBodyContent` | **yes** |
| `getTxBodyContent` | **yes** |
| `getTxBody` | **yes** |
| `TxBody` | **yes** |
| `createTransactionBody` | **yes** |
| `TxInsCollateral`, `TxInsCollateralNone` | no |
| `makeSignedTransaction`, `serialiseToCBOR`, `deserialiseFromTextEnvelope` | no |

The collateral constructors at the migration site are **not** deprecated, so
they are not what the pragma is carrying.

## The site

`lib/wallet/src/Cardano/Wallet.hs`, `buildCoinSelectionForTransaction`, in a
`where` clause — a record pattern on a `cardano-api` type, not a tuple bind:

```haskell
Cardano.TxBodyContent
    { txIns
    , txOuts
    , txInsCollateral
    , txWithdrawals
    } =
        Cardano.getTxBodyContent
            $ Cardano.getTxBody
            $ toCardanoApiTx tx
```

The function already takes `Write.Tx era`. `toCardanoApiTx` converts it *to*
`cardano-api`, destructures it, and each field is converted back. This is a
converter deletion, not a representation change — which is what makes the
no-new-type fence affordable.

Direction, to be verified against the resolved artifacts rather than assumed —
existence is not suitability:

| from | to |
|---|---|
| `fromCardanoTxIn` | `fromShelleyTxIn` |
| `fromCardanoTxOut` | `fromConwayTxOut` |
| `fromCardanoWdrls` | ledger reward-account accessors |

## The second round-trip — RESOLVED, in scope (A-001)

`Wallet.hs`'s pragma has a **second cause** the four-field deletion does not
reach. `TxBody` is deprecated in 11.5.0.0 and appears at two signatures:

- `:3441` `constructTransaction … -> ExceptT ErrConstructTx IO (Cardano.TxBody (CardanoApiEra era))`
- `:3464` `constructUnbalancedSharedTransaction … -> …`

Produced by `mkUnsignedTransaction` → `constructUnsignedTx` → `mkUnsignedTx`
(`Shelley/Transaction.hs`, owned), consumed at
`constructTransaction` and `constructSharedTransaction` in
`lib/api/.../Shelley/Server.hs`, both of which immediately do
`fromCardanoApiTx $ Cardano.Tx unbalancedTx []`.

In `Api/Extra.hs` the converters are an **identity pair** in Conway — wrap and
unwrap. So this is the same round-trip one layer up, through the same two
functions, needing no new type.

**Ruling A-001: in scope.** The owned set gains **exactly** the
`fromCardanoApiTx` import in `Server.hs` and its two call sites, for the
sole purpose of dropping the converter application. **Not general licence over
`lib/api/**`.** Five constraints bind:

1. `lib/wallet/src/Cardano/Api/Extra.hs` is **not** edited — call sites change,
   the module does not. It retires with #5290.
2. Converter orphaning is checked **after** the change as well as before.
   Neither converter is orphaned.
3. The return-type change and both consumers land in the **same commit**.
   Satisfied by construction: the accepted candidate is squashed into one
   behaviour commit.
4. Both `Wallet.hs` signatures and both `Server.hs` sites, or none.
5. #5411's published body states the widened scope — done and verified.

Slice definition, models and task IDs follow from this; see `tasks.md`.
