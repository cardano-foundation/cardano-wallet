# modules-model — #5288 / #5427

All changed responsibility is inside one test module. No production module
changes, so no dependency direction moves and no abstraction is promoted.

| ID | Module | Responsibility after this ticket | Direction |
|---|---|---|---|
| M1 | `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` | Owns both parity arms, the comparison harness, the `5413 review response` examples and their fixtures | depends on `cardano-api` (already declared) and `cardano-wallet`; nothing depends on it |

- M1 gains a `cardano-api` **construction** responsibility it does not have
  today; it already imports `Cardano.Api` for conversion helpers.
- The cardano-api arm is a **migration-window oracle**. It carries, in its
  haddock, the condition under which it is deleted, in the manner of
  `roundTripThroughCardanoApi` (`TransactionLedgerSpec.hs:1635-1641`).
- Forbidden: moving either arm into `lib/wallet` or `lib/primitive`. A test
  oracle in production code re-adds the surface M1 is retiring.
