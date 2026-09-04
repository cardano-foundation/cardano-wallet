# spec.md — #5419 retire the `cardano-api` deprecations in the two Shelley spec modules

Ticket owner `%310`. Parent: M6 milestone owner `%4`. Ceiling: 120 lines.

## Why this exists

`cardano-api` deprecates the transaction-body surface these two unit-spec
modules are built on. Rather than move off it, each module carries an
`{-# OPTIONS_GHC -Wno-deprecations #-}` pragma that silences the warning. M6's
front on this is to retire suppressions rather than inherit them: seven already
sit on `master` that were introduced as temporary and did not retire.

This ticket removes the two suppressions **by removing their cause**.

## User stories

- **US-1** As a wallet maintainer, I can build both spec modules under the
  release flags with no deprecation suppression, so the compiler tells me the
  truth about the `cardano-api` surface these tests depend on.
- **US-2** As the M6 milestone, the deprecation-suppression count falls rather
  than holds, so the ratchet has something to ratchet.
- **US-3** As a reviewer of #5413, I get these pragmas removed inside a PR I am
  already reading, at no extra review cycle.

## Requirements

| ID | Requirement |
|---|---|
| REQ-1 | `TransactionLedgerSpec.hs` builds with no `-Wno-deprecations` and no deprecation warning under `--flags=release`. |
| REQ-2 | `TransactionSpec.hs` likewise. |
| REQ-3 | The deprecated call sites are retired by reading the transaction body from the ledger transaction, the direction #5413 established — not by suppressing, renaming, or deleting the assertions that depend on them. |
| REQ-4 | Every assertion the two modules made before this change still holds and can still fail afterwards. |
| REQ-5 | No production module is edited. The Haskell behaviour change is confined to the two named spec modules; test support extracted from them, this ticket's planning documents under `specs/5419-specs-deprecations/`, and the `cardano-wallet-unit` test-suite stanza that has to list such a module are in scope. |
| REQ-6 | The Dijkstra stub surface does not grow. |

## Observable success

- Structural suppression census over tracked `.hs` `OPTIONS_GHC` lines, both
  spellings, falls **11 -> 9** measured from the tree, never from prose.
- A release-flag build of `cardano-wallet-unit:unit` exits 0 with both pragmas
  absent.
- The focused suites the two modules own — `Sign transaction`,
  `calculateBinary` goldens, `SealedTx serialisation/deserialisation` — pass,
  and each can be made to fail by mutation.
- Normalized Dijkstra stub census **does not exceed 44 hits across 15 files**,
  with a positive control proving the counter counts and a negative control
  proving it can return zero.

## Rejection behaviour — what fails this ticket even if it is green

- **R-1** A build without `--flags=release`. Deprecations are not fatal without
  it, so a green result cannot distinguish "no deprecation" from "deprecation
  not fatal". It is a different gate and its PASS answers a different question.
- **R-2** A suppression count taken from a report, a comment, or prose rather
  than from `git grep` over the tree, in both spellings.
- **R-3** Retiring a deprecation by weakening or deleting the assertion that
  used it.
- **R-4** Satisfying the census by rewording a stub so the counter stops
  matching it. Renaming a stub is not retiring it; it moves the number without
  moving the debt. **Attempted on 2026-09-01 and refused — see `plan.md`.**
- **R-5** Any edit to a production module — anything under `lib/*/src/` or
  `lib/*/lib/` — and specifically `Gen.hs` (dies with the shim in #5290),
  `TransactionsNew.hs` (separate slice), and any pre-existing `master` pragma
  (M1's, #5418). The subject of this rejection is **production code, not a
  file count**: test support extracted from the two spec modules is not an
  edit outside them, it is the same code with one definition site instead of
  two.
- **R-6** A new type introduced to model the transaction-body boundary. That is
  M1 architecture, not a test fix — stop and escalate.

## Out of scope

`Gen.hs`; `TransactionsNew.hs`; the seven pre-existing `master` suppressions;
the `cardano-api` drop itself (milestone M1, #5290); the flaky
`Conway Integration Tests` job, which is pre-existing on this fleet — report it,
do not chase it.

## Invariants

Defined with severities in `../../handoffs/mandate.md`, sha256
`a20f46c0cc9189f51c32268f2b5401c267c3cdd359007495af9544845aec700a`:
INV-1 GATE-IDENTITY, INV-2 SCOPE-FENCE, INV-3 NON-VACUITY, INV-4
DIJKSTRA-CENSUS (all BLOCKING), INV-5 SUPPRESSION-RATCHET (ADVISORY).
