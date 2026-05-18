---
description: "Task list for dropping cardano-api from Shelley/Transaction.hs and cert helpers"
---

# Tasks: Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Input**: Design documents from `/specs/006-drop-api-shelley-tx/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/module-interface-invariants.md, quickstart.md

**Status**: One narrow API-removal slice is implemented in this PR. The
original Story 1 helper deletion is still deferred behind the unsigned body
builder migration — see plan.md §"Status".

## Completed implementation slice

- [x] Route `Cardano.Wallet.buildAndSignTransaction` through
  `Cardano.Wallet.Shelley.Transaction.Ledger.mkTransaction`, the ledger-native
  signed transaction constructor.
- [x] Remove the now-unused `mkTransaction` wrapper from
  `Cardano.Wallet.Shelley.Transaction`.
- [x] Prune imports and the local `txExtendedFromRecentTx` helper that were
  only used by that wrapper.
- [x] Verify the old wrapper is gone:
  `rg -n "\\bmkTransaction\\b" lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
  returns no matches, while `Cardano.Wallet.hs` imports and calls the ledger
  module's `mkTransaction`.

This slice is deliberately smaller than the original Story 1 plan. It removes
one reachable cardano-api body-construction path after #5287 made
the ledger-native `mkTransaction` carry the real mint value. It does not change
`mkUnsignedTransaction`, and it does not delete the cardano-api cert helper
modules.

## Why the original Story 1 tasks remain deferred

An earlier revision of this file (commit `73b1088113`, then patched at
`dce6abbbf1`) carried tasks T001–T014 implementing Story 1 as a single vertical
commit: switch the 5 cert-builder callsites in `Shelley/Transaction.hs` to the
`*Ledger` variants, delete `Voting.hs` + `Delegation.hs`, prune cabal
`exposed-modules`.

The implementation pre-flight on `dce6abbbf1` showed the switch does not compile. `mkUnsignedTransaction` flows the cert lists into `constructUnsignedTx` (the cardano-api body builder) at line ~834, which expects `[ApiCert.Certificate (CardanoApiEra era)]`. The `*Ledger` cert builders return `[Ledger.TxCert era]`. The two are not interchangeable. `Cardano/Wallet.hs` works on `*Ledger` only because it routes through `constructUnsignedTxLedger` — a different code path inside the same file family.

Full analysis: `research.md` §G.

The corollary is that Story 1 is blocked transitively on Story 2 (the body-construction migration). After #5287, mint plumbing is available; when the remaining script-witness parity lands, the helper deletion + cabal prune fold into Story 2's commit. There is no standalone Story 1 PR.

## Phase 1: Setup — COMPLETE

The branch is stacked on `007-ledger-minting`, so the ledger-native
`mkTransaction` now receives the real mint/burn value from `TransactionCtx`.

## Phase 2: Foundational — COMPLETE

The signed wallet transaction path now uses the ledger-native constructor.

## Phase 3: User Story 1 — DEFERRED

Folded into Story 2. See plan.md §"Vertical Slice Contract" and quickstart.md §"What to do when Story 2 unblocks".

## Phase 4: User Story 2 — PARKED on script-witness parity

The minting half of the prerequisite landed in #5287. When the remaining
script-witness parity lands:

1. Re-inventory cardano-api importers in `Shelley/Transaction.hs` (research §A) — line numbers will have drifted.
2. Replace `createTransactionBody` over `TxBodyContent` in `mkUnsignedTx` and `mkWithdrawalTx` with the ledger-native body builder; the cert-list type at the callsites becomes `[Ledger.TxCert era]`, which makes the Story 1 cert-builder switch type-compatible.
3. Switch the 5 cert callsites in `Shelley/Transaction.hs` to `*Ledger` variants; delete `Voting.hs` + `Delegation.hs`; prune the two cabal `exposed-modules` entries. (This is the Story 1 work, folded in.)
4. Single vertical commit. RED proof: existing property + golden body-bytes suite for `mkUnsignedTx` and `mkWithdrawalTx`, plus the signing-property suite that already covers the cert paths indirectly. Tighten with explicit per-era byte-equality assertions if the existing coverage is insufficient.

## Phase 5: User Story 3 — PARKED on #5243 (ledger-native `signTransaction`)

When the prerequisite lands:

1. Rewrite `signTransaction` directly on ledger types, removing the last cardano-api callsites in `Shelley/Transaction.hs` (~13 symbols per research §A "Story 3", data-model §"Story 3").
2. Single vertical commit. RED proof: existing signing-property suite plus on-chain verification tests.
3. If Story 2 has also landed and no other module under `lib/wallet/` still imports cardano-api at this point, drop the `cardano-api` `build-depends` entry from `lib/wallet/cardano-wallet.cabal` in this same commit (FR-006, SC-004). Otherwise, defer the build-depends removal to whichever later slice closes the last importer.

## Phase 6: Polish & Cross-Cutting — N/A

Nothing to land in this PR beyond the documentation correction itself.

## Implementation Strategy

Wait for the remaining script-witness and ledger-native signing prerequisites. When they land, regenerate the research §A inventory against the then-current tip, and proceed with Story 2 (which subsumes Story 1) and Story 3.

## Notes

- `research.md` §G is the authoritative explanation of why Story 1 cannot land in isolation; reference it before any future attempt to revive Story 1 as a standalone slice.
- The 5 cert callsites in `Shelley/Transaction.hs` (lines 400, 778, 784, 849, 855 on `dce6abbbf1` — drift expected) and the 4 already-migrated callsites in `Cardano/Wallet.hs` (lines 2729, 2738, 3533, 3542 on the same commit) are the inventory anchor for the next attempt.
- The `cardano-api` `build-depends` entry in `lib/wallet/cardano-wallet.cabal` is untouched by this PR.
