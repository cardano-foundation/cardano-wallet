# plan — #5288 / #5427

## Constraint that selects the approach

`scripts/ci/cardano-api-closure-gate.sh` at base reports
`closure-lib=12 (MAX=12)`, `closure-any=13 (MAX=13)`, `suppressions=9 (MAX=9)`:
zero slack on every row. `suppressions` counts *occurrences* of
`-Wno-deprecations` / `-fno-warn-deprecations` in `.hs`, `.cabal` and
`cabal.project*`, so one new pragma is `10 > 9`, GATE RED.

`Cardano.createTransactionBody`, `TxBodyContent`, `TxBody` and `ShelleyTxBody`
are `{-# DEPRECATED #-}` in cardano-api 11.5.0.0
(`src/Cardano/Api/Tx/Internal/Body.hs:864,1258,1441`,
`Tx/Internal/Sign.hs:302,304`). Restoring the pre-#5285 builder verbatim
therefore needs a suppression and is unavailable.

`Cardano.Api.Experimental.makeUnsignedTx` is not deprecated:

    makeUnsignedTx :: Era era -> TxBodyContent (LedgerEra era)
                   -> Either MakeUnsignedTxError (UnsignedTx (LedgerEra era))

It builds `bodyTxL` from `L.mkBasicTxBody` and the witness set from
`L.mkBasicTxWits & L.scriptTxWitsL .~ ...` itself
(`Experimental/Tx/Internal/BodyContent/New.hs:185-256`), independently of the
wallet's `mkLedgerTx`. `UnsignedTx era = UnsignedTx (Ledger.Tx TopTx era)`, so
the result is directly comparable with `Write.Tx Write.Conway`.
`lib/unit` already `build-depends: cardano-api` in both stanzas, so the arm adds
no closure member.

**Decision: the cardano-api arm is built on `Cardano.Api.Experimental`.**
Rejected alternative, and it stays rejected: restoring the deprecated
`createTransactionBody` path, because it costs a suppression the ratchet has no
room for.

## Live boundary

None. Both builders are pure; there is no node, socket or database on this path.

## Ordered, bisect-safe slices

- **S1 — #5427 fixture distinctness.** Independent of S2, smaller, and its red
  is cheap to demonstrate. Land it first so a bisect between S1 and S2 has a
  meaningful tree.
- **S2 — the cardano-api parity arm.** Replaces `buildLegacyParityTx` with a
  `Cardano.Api.Experimental` builder, repoints `reviewResponseTx` at the wallet
  builder, and demonstrates R2's red.

## Why S2 repoints `reviewResponseTx`

The `5413 review response` examples are about `balanceTx`'s treatment of a tx
built by the **wallet** builder; cardano-api is not their subject. Today
`buildLegacyParityTx` and `buildNewParityTx` bottom out in the same
`buildLedgerTxRaw` call with equal arguments, so repointing `reviewResponseTx`
at the wallet arm is behaviour-preserving at this base. S2 demonstrates that
equivalence before removing the legacy arm rather than asserting it.

## Risk the ticket owner accepts

The two builders may genuinely disagree on a scenario — that is the information
the oracle exists to produce. Report it; do not weaken the comparison. See
spec.md, "Rejection behaviour".
