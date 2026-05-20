# Tasks: Unsigned Shelley ledger builder migration

**Input**: Design documents from `/specs/009-unsigned-ledger-builder/`  
**Prerequisites**: [spec.md](./spec.md), [plan.md](./plan.md), [research.md](./research.md), [data-model.md](./data-model.md), [quickstart.md](./quickstart.md)

**Tests**: Mandatory. This is a transaction-byte preservation migration and must follow RED -> GREEN in one bisect-safe implementation commit.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: parallelizable; unused for the behavior slice because this epic is serial
- **[Story]**: user story label from `spec.md`
- Paths are repository-root relative

## Phase 1: Setup

- [X] T000 Bootstrap branch, add `gate.sh`, open draft PR [#5292](https://github.com/cardano-foundation/cardano-wallet/pull/5292)
  ([commit `59b3bcd110`](https://github.com/cardano-foundation/cardano-wallet/commit/59b3bcd110))
- [X] T001 Commit `spec.md` and `checklists/requirements.md` for #5285
  ([commit `7eed1d3146`](https://github.com/cardano-foundation/cardano-wallet/commit/7eed1d3146))
- [X] T002 Commit `plan.md`, `research.md`, `data-model.md`, `quickstart.md`, `tasks.md`, and worker brief

## Phase 2: Foundational

No separate foundational implementation commit. The acyclic builder boundary is part of the single behavior slice because it is only useful together with the migrated unsigned builder.

## Phase 3: User Story 1 + 2 - migrated unsigned builder and helper deletion (Priority: P1)

**Goal**: `mkUnsignedTransaction` / `mkUnsignedTx` build through ledger-native code, and obsolete certificate helper modules are gone.

**Independent Test**: focused Shelley transaction unit specs plus `./gate.sh`.

- [X] T010 [US1] Add/adjust RED tests in `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs` and/or `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` so the desired ledger-cert `mkUnsignedTx` wrapper path is required and the pre-change code fails to compile or fails the test.
- [X] T011 [US1] Implement the acyclic shared unsigned ledger-builder boundary in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Build.hs` or a new sibling module, updating `Transaction.Ledger` to reexport/delegate without importing cycles.
- [X] T012 [US1] Migrate `mkUnsignedTransaction` and `mkUnsignedTx` in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` off `Cardano.createTransactionBody` / `Cardano.TxBodyContent`, preserving existing `Cardano.TxBody` return interfaces where still required.
- [X] T013 [US2] Switch the remaining certificate call sites in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` to ledger-native helpers, delete `lib/wallet/src/Cardano/Wallet/Transaction/Delegation.hs` and `lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs`, and prune both exposed modules from `lib/wallet/cardano-wallet.cabal`.

T010-T013 fold into one commit with subject `feat(5285): migrate unsigned Shelley tx builder` and trailer `Tasks: T010, T011, T012, T013`.

## Phase 4: Review refinements (Priority: P1)

**Goal**: Address Copilot review feedback without broadening the #5285 behavior scope.

**Independent Test**: focused Shelley transaction unit specs plus `./gate.sh`.

- [ ] T014 [US1] Refactor `buildLedgerTx` in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs` to delegate to `buildLedgerTxRaw ... (Right cs) ...`, removing the duplicate ledger-body construction while preserving the public signature and output bytes.
- [ ] T015 [US1] Replace the local `toLedgerCoin` alias in `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs` with the primitive `Cardano.Wallet.Primitive.Ledger.Convert.toLedgerCoin` import, and keep `toCardanoLovelace` only where still intentionally needed.
- [ ] T016 [US1] Add parity coverage for non-empty `assetsToBurn` in `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`, proving legacy `mkUnsignedTx` and new `buildLedgerTx` agree on the ledger `MultiAsset` when burn values are present.

T014-T016 fold into one review-response commit with subject `refactor(5285): address unsigned builder review feedback` and trailer `Tasks: T014, T015, T016`.

## Phase 5: User Story 3 - policy guard and finalization (Priority: P2)

- [ ] T020 Run `./gate.sh` at HEAD and record the passing result in the PR body.
- [ ] T021 Verify `git diff --name-only origin/master...HEAD | rg '^lib/integration/'` prints nothing.
- [ ] T022 Verify removed surface with `rg`: no remaining helper-module imports and no `createTransactionBody` / `TxBodyContent` in the migrated unsigned builder.
- [ ] T023 Run finalization audit over commits and task checkboxes.
- [ ] T024 Drop `gate.sh` in a dedicated `chore: drop gate.sh (ready for review)` commit, push, and mark PR #5292 ready.

## Dependencies & Execution Order

```text
T000 -> T001 -> T002 -> (T010 + T011 + T012 + T013, one worker commit) -> (T014 + T015 + T016, one review-response commit) -> T020 -> T021 -> T022 -> T023 -> T024
```

## Parallel Opportunities

None for implementation. The parent epic and tracker require serial behavior-changing work for #5288 -> #5285 -> #5289. Allowed parallel work is limited to read-only exploration and CI/metadata observation.

## Implementation Strategy

1. Orchestrator commits setup/spec/plan assets.
2. One tmux-controlled worker receives [briefs/T010-T013.md](./briefs/T010-T013.md).
3. Worker appends progress to `WIP.md`, performs RED -> GREEN -> `./gate.sh`, and returns exactly one commit.
4. Orchestrator reviews the diff, reruns `./gate.sh`, stamps T010-T013 in this file by amending the worker commit, and pushes.
5. If review feedback requires code changes, orchestrator restores `gate.sh`, records bounded follow-up tasks, and dispatches a fresh worker for the review-response commit.
6. Orchestrator completes finalization tasks and marks the PR ready.
