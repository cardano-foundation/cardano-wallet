# tasks.md — #5419

Ceiling: 45 lines. Stamped by the ticket owner on acceptance only.

## S1 — TransactionLedgerSpec

- [x] T1 Remove `{-# OPTIONS_GHC -Wno-deprecations #-}` from
      `TransactionLedgerSpec.hs`.
- [x] T2 Enumerate the resulting deprecation diagnostics under `--flags=release`
      and freeze that RED output as evidence. Causes from GHC, not grep.
- [x] T3 Retire every enumerated site by reading the four body fields from the
      ledger transaction.
- [x] T4 Show each affected assertion can still fail (INV-3), per property.
- [x] T5 Release-flag build green with the pragma absent; receipt frozen.
- [x] T6 One commit, this file only.

## S2 — TransactionSpec

- [x] T7 Remove `{-# OPTIONS_GHC -Wno-deprecations #-}` from `TransactionSpec.hs`.
- [x] T8 Enumerate diagnostics under `--flags=release`; freeze RED evidence.
- [x] T9 Retire every enumerated site the same way.
- [x] T10 Show each affected assertion can still fail (INV-3), per property.
- [x] T11 Release-flag build green with the pragma absent; receipt frozen.
- [x] T12 One commit, this file only.

## Cross-slice, proved on the final candidate tree

- [x] T13 Suppression census from the tree, `git grep`, **both** spellings,
      11 -> 9; the nine remaining are exactly the base set minus the two owned.
- [x] T14 Dijkstra stub census **does not exceed** 44 hits / 15 files, with the
      positive control proving the counter counts and the negative control
      proving it can return zero, plus the full added/removed site delta.
- [x] T15 Scope fence: `base..candidate` touches exactly the two files.
- [x] T16 Focused suites green under release flags: `Sign transaction`,
      `calculateBinary`, `SealedTx serialisation/deserialisation`.

## Repair carried from the pre-submission finding

- [x] T17 The ten per-property Dijkstra arms are replaced by era handling that
      does not grow the stub surface, without rewording any stub and without
      deleting an arm that still represents undone work.
