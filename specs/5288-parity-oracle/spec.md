# spec — #5288 script-witness parity oracle, #5427 fixture distinctness

Issues: cardano-foundation/cardano-wallet#5288, #5427. Base `bd187ae44d`.

## Why one ticket

`reviewResponseTx` (`TransactionLedgerSpec.hs:1646`) calls `buildLegacyParityTx`
(`:1473`). #5427's fixture is built by the exact function #5288 must replace, so
two lanes in that file conflict by construction. Premise checked, not assumed.

## Problem, stated as observed fact

`buildLegacyParityTx` calls `mkUnsignedTx` (`Shelley/Transaction.hs:794`), whose
body is `buildLedgerTxRaw`. `buildNewParityTx` calls `buildLedgerTx`
(`Shelley/Transaction/Unsigned.hs:280`), whose body is `buildLedgerTxRaw`. The
two arms are the same function applied to equal arguments. The comparison
`parityBodyCbor legacy == parityBodyCbor new` holds by construction and is
green whatever either builder does.

The oracle did not decay. It was killed by `16e3089986` (`feat(5285)`), the
migration it existed to guard: before that commit `mkUnsignedTx` built through
`Cardano.createTransactionBody`. `#5411` later changed only the return type.

#5288 acceptance criterion 3 — "compare ledger-builder output against the
existing cardano-api builder" — is therefore unmet, and that is why #5288 is open.

#5427: the added-script test asserts `Set.member reviewAddedInputScriptHash`
in the balanced witness map. `reviewAddedInputScript = mkScript 93`; the
declared set already holds 91, 92, 94. Rewriting 93 to 91 leaves the test
green, so it cannot distinguish "a distinct script was added" from "an existing
one was reused". The reporter is explicit that this is a test-quality finding,
not a production defect.

## Requirements

- **R1** The parity oracle's two arms are built by independent builders. One
  arm constructs the transaction through `cardano-api`; the other through the
  wallet ledger builder. Neither arm's call graph reaches the other's builder.
- **R2** The parity comparison can fail. A seeded perturbation of the wallet
  ledger builder turns at least one enumerated scenario and the property red;
  the unperturbed tree is green.
- **R3** The added-script test fails under the `93 -> 91` collision the #5427
  report names, and passes with the original fixture.
- **R4** #5288 criteria 1, 2, 4, 5, 6 remain satisfied, each with a named check.
- **R5** No new deprecation suppression and no new cardano-api closure member.
- **R6** No file under `lib/integration/**`.

## Observable success

Green under a sanctioned build path with mechanically captured exit codes, plus
a demonstrated red for R2 and R3 from the perturbations those requirements name.

## Rejection behaviour

If a scenario cannot be made to agree across the two independent builders, the
divergence is **reported as a finding with evidence**. The comparison is never
weakened, narrowed, or made tolerant to restore green. A green obtained by
softening the oracle fails this spec outright.
