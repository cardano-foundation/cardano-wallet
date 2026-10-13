# tasks — #5288 / #5427

## S1 — fixture distinctness (#5427)

- [x] T1 RED: with `reviewAddedInputScript = mkScript 91`, the added-script
      example fails; capture the failing run.
- [x] T2 Assert `reviewAddedInputScriptHash` is absent from the declared
      witness set and present in the balanced set.
- [x] T3 GREEN with the original `mkScript 93` fixture; re-run the `93 -> 91`
      mutation and capture the red.

## S2 — cardano-api parity arm (#5288 AC3)

- [x] T4 RED: seed a perturbation in the wallet ledger builder
      (`Shelley/Transaction/Unsigned.hs`) and show the current oracle stays
      green — the vacuity, demonstrated rather than argued.
- [x] T5 Add `buildCardanoApiParityTx` (F1) over
      `Cardano.Api.Experimental.makeUnsignedTx`.
- [x] T6 Show `buildLegacyParityTx` and `buildNewParityTx` agree at this base,
      then repoint `reviewResponseTx` at F2 and delete the legacy arm and any
      helper it orphans.
- [x] T7 Point `shouldHaveBodyParity` and `propParity` at F1.
- [x] T8 GREEN: all enumerated scenarios and the property pass. Any scenario
      that cannot agree is reported, not softened.
- [x] T9 Re-run T4's perturbation and capture the red.

## Ticket-wide

- [x] T10 Closure gate green; record all three rows; lower any `MAX` that falls.
- [x] T11 `git diff --name-only` shows no `lib/integration/**` path.
- [x] T12 #5288 criteria 1, 2, 4, 5, 6 each checked, with the command per row.
