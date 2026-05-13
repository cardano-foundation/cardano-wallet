---
description: "Task list for dropping cardano-api from Shelley/Transaction.hs and cert helpers"
---

# Tasks: Drop cardano-api from Shelley/Transaction.hs and cert helpers

**Input**: Design documents from `/specs/006-drop-api-shelley-tx/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/module-interface-invariants.md, quickstart.md

**Tests**: No new tests authored. The existing signing-property and golden suites in `lib/unit/test/unit/Cardano/Wallet/Shelley/{Transaction,TransactionLedger}Spec.hs` are the RED proof — they exercise the cert helpers indirectly and pass before and after the switch because the `*Ledger` builders are byte-equivalent (FR-003, SC-003). Verifying the suite stays green pre-change and post-change is the regression proof.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story. Only Story 1 has work tasks in this PR. Stories 2 and 3 are parked entries flagged with their prerequisite acceptance criteria in #5243.

**Vertical commit contract**: Story 1 lands as a single bisect-safe commit (FR-005, plan §"Vertical Slice Contract"). All Story 1 tasks below fold into one commit when applied; do not stage them as separate commits.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies on incomplete tasks)
- **[Story]**: User story label (US1, US2, US3) — required for story-phase tasks
- File paths are relative to repo root `/code/cardano-wallet-5285`

---

## Phase 1: Setup

**Purpose**: Pre-flight checks. No project initialization needed (refactor inside an existing Cabal package).

- [ ] T001 Confirm clean working tree and that the branch is at the latest pushed `origin/006-drop-api-shelley-tx` head: run `git status` (clean) and `git log --oneline -1` (matches `30673f9ce8 docs(5285): plan, research, data-model, contracts, quickstart`)
- [ ] T002 Confirm the ledger replacements exist by grepping `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` for `certificateFromVotingActionLedger` and `certificateFromDelegationActionLedger` — expect two hits in the export list and one definition each (research §C)

---

## Phase 2: Foundational

**Purpose**: Prerequisite work shared across stories.

**Status**: For Story 1, none required — replacements landed in PR #5270 and are already exported from `Shelley/Transaction/Ledger.hs`. For Stories 2 and 3, see the parked entries in Phases 4 and 5.

- [ ] T003 Capture the baseline RED proof: run `nix develop --quiet -c cabal test cardano-wallet-unit:unit --test-options="--match \"Sign transaction\""` on the unmodified branch tip and record green. This is the byte-equivalence baseline that the post-Story-1 run must match (FR-003)

**Checkpoint**: Foundation ready — Story 1 work can begin.

---

## Phase 3: User Story 1 - Delete the cert helper modules (Priority: P1) 🎯 MVP

**Goal**: Delete `Voting.hs` and `Delegation.hs`, switch all 9 callsites to the `*Ledger` variants, and prune the deleted modules from the cabal `exposed-modules`. Single vertical commit.

**Independent Test**: After the slice, `git grep -nE "Cardano\.Wallet\.Transaction\.(Voting|Delegation)" lib/` returns empty; `git grep -nE "^import.*Cardano\.Api" lib/wallet/src/Cardano/Wallet/Transaction/` returns empty; `cabal build cardano-wallet` and `cabal test cardano-wallet-unit:unit --test-options="--match \"Sign transaction\""` are green; the test result matches the T003 baseline byte-for-byte over the property vectors.

### RED proof (existing suite, no new tests authored)

The signing-property suite (`TransactionSpec.hs` "Sign transaction" cases at `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs:377-412`) exercises both cert helpers indirectly. The T003 run captures it green pre-change; the T012 run must reproduce green post-change. If T012 surfaces a regression, the slice is wrong, not the test.

### Implementation for User Story 1 (single vertical commit)

> All tasks T004–T011 fold into one commit. Do NOT commit between them. The commit is created at T012 after the gate passes.

- [ ] T004 [US1] In `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`, update the import block: remove `import Cardano.Wallet.Transaction.Voting` and `import Cardano.Wallet.Transaction.Delegation`; add `certificateFromVotingActionLedger` and `certificateFromDelegationActionLedger` to the existing `import Cardano.Wallet.Shelley.Transaction.Ledger` group
- [ ] T005 [US1] In `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`, switch the 5 callsites to the `*Ledger` variants at lines 400, 778, 784, 849, 855 (3 delegation, 2 voting per research §A; verify exact form before editing). Confirm each result flows into `TxPayload._certificates`, whose field type is already `[TxCert era]` post-#5270 (data-model §"Story 1")
- [ ] T006 [P] [US1] In `lib/wallet/src/Cardano/Wallet/Wallet.hs`, update the import block: remove the two helper-module imports; add `certificateFromDelegationActionLedger` and `certificateFromVotingActionLedger` to the `Cardano.Wallet.Shelley.Transaction.Ledger` import group
- [ ] T007 [P] [US1] In `lib/wallet/src/Cardano/Wallet/Wallet.hs`, switch the 4 callsites to the `*Ledger` variants at lines 2729 (delegation), 2738 (voting), 3533 (delegation), 3542 (voting) per research §B
- [ ] T008 [P] [US1] Delete `lib/wallet/src/Cardano/Wallet/Transaction/Voting.hs` via `git rm`
- [ ] T009 [P] [US1] Delete `lib/wallet/src/Cardano/Wallet/Transaction/Delegation.hs` via `git rm`
- [ ] T010 [US1] In `lib/wallet/cardano-wallet.cabal`, remove the two lines `Cardano.Wallet.Transaction.Delegation` and `Cardano.Wallet.Transaction.Voting` from the `exposed-modules` block (around lines 256-257). Do NOT touch the `cardano-api` `build-depends` entry — `Shelley/Transaction.hs` is still a consumer (FR-006, plan §"Vertical Slice Contract")
- [ ] T011 [US1] Run the compatibility audit checklist from `contracts/module-interface-invariants.md`: `git grep -E "^import.*Cardano\.Api" lib/wallet/src/` produces no new entries; `git grep -lE "Cardano\.Wallet\.Transaction\.(Voting|Delegation)\b" lib/` is empty; the `exposed-modules` diff matches T010 and nothing else
- [ ] T012 [US1] Run the slice gate: `nix develop --quiet -c cabal build cardano-wallet 2>&1 | tail -30` clean; `nix develop --quiet -c cabal test cardano-wallet-unit:unit --test-options="--match \"Sign transaction\""` green and matching the T003 baseline. If green, stage everything and create the single commit using the message in `quickstart.md` §"Commit message" (verify `!:` necessity by running `cabal build all` first; drop the `!` if no downstream package outside `lib/wallet/` sees a removed export)

**Checkpoint**: Story 1 commit on the branch, single commit, bisect-safe; PR #5286 picks it up automatically on push.

---

## Phase 4: User Story 2 - Migrate body construction in `Shelley/Transaction.hs` (Priority: P2) — PARKED

**Status**: BLOCKED on prerequisite acceptance criterion in [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243). No work tasks in this PR.

**Prerequisite (tracked in #5243)**: Minting + script-witness support in `Cardano.Wallet.Shelley.Transaction.Ledger` — currently stubbed at `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs:433` and `:496` (`mempty -- TODO: minting support`); pervasive cardano-api coupling acknowledged at `Ledger.hs:47-49`.

**Scope when unblocked**: Replace `createTransactionBody` over `TxBodyContent` in `mkUnsignedTx` and `mkWithdrawalTx` with the ledger-native builder; remove ~25 cardano-api symbols (research §A "Story 2", data-model §"Story 2"). Single vertical commit; RED proof is the existing property + golden body-bytes suite for those functions, tightened with explicit per-era byte-equality assertions if the existing coverage is insufficient (plan §"Vertical Slice Contract").

**Pre-flight when work resumes**: re-run the Phase 0 inventory (`research.md` §A) against the current tip — line numbers will have drifted; any newly-introduced cardano-api importer in `Shelley/Transaction.hs` must be folded into the slice.

---

## Phase 5: User Story 3 - Migrate `signTransaction` off cardano-api (Priority: P3) — PARKED

**Status**: BLOCKED on prerequisite acceptance criterion in [#5243](https://github.com/cardano-foundation/cardano-wallet/issues/5243). No work tasks in this PR.

**Prerequisite (tracked in #5243)**: Ledger-native `signTransaction` rewrite. No `signTransactionLedger` exists today; `signTransaction` is re-exported from `Shelley.Transaction` at `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs:26` and still uses cardano-api internally. Witness builders `mkShelleyWitnessLedger` (`Ledger.hs:776-812`) and `mkByronWitnessLedger` (`Ledger.hs:819-844`) are already in place and ready to be composed into the rewrite.

**Scope when unblocked**: Rewrite `signTransaction` directly on ledger types, removing the last cardano-api callsites in `Shelley/Transaction.hs` (~13 symbols per research §A "Story 3", data-model §"Story 3"). Single vertical commit; RED proof is the existing signing-property suite plus on-chain verification tests. The `cardano-api` `build-depends` entry in `lib/wallet/cardano-wallet.cabal` is removed in this slice IF no other module under `lib/wallet/` still imports cardano-api at that point (FR-006, SC-004); otherwise it is removed in whichever later slice closes the last importer.

**Pre-flight when work resumes**: same as Story 2 — re-inventory cardano-api importers; the `NetworkId` symbol shared between Stories 2 and 3 (research §A footnote) means the order in which Stories 2 and 3 land affects which slice owns its removal.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Post-Story-1 housekeeping.

- [ ] T013 [P] [US1] Update PR #5286 description to reflect Story 1 landing per `feedback_pr_descriptions.md`: state Story 1 is shipped, list the 9 callsites switched and 2 modules deleted, link the spec, and explicitly note Stories 2 and 3 are parked on #5243 prerequisites and not in this PR
- [ ] T014 [US1] After the gate is green and the commit is pushed, follow the CI loop per `feedback_ci_loop.md` until checks complete green on the branch

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies.
- **Phase 2 (Foundational)**: Depends on Phase 1. T003 establishes the byte-equivalence baseline that T012 must match.
- **Phase 3 (US1)**: Depends on Phase 2. Self-contained single vertical commit.
- **Phases 4 & 5 (US2, US3)**: Parked. Out of scope for this PR. Resume only after the corresponding #5243 acceptance criterion is satisfied.
- **Phase 6 (Polish)**: Depends on Phase 3 commit being pushed.

### User Story Dependencies

- **US1**: Self-contained. Independent of US2 and US3. The replacements it depends on (`*Ledger` cert builders) are already merged via #5270.
- **US2**: Depends on `Transaction.Ledger` minting + script-witness support landing under #5243.
- **US3**: Depends on a ledger-native `signTransaction` rewrite landing under #5243.
- US2 and US3 are independent of each other once their respective prerequisites land — they may be picked up in either order or in parallel.

### Within Story 1

- T004 → T005 (must update imports before edits compile in `Shelley/Transaction.hs`).
- T006 → T007 (same, for `Wallet.hs`).
- T004/T005 (Shelley/Transaction.hs edits) and T006/T007 (Wallet.hs edits) and T008/T009 (file deletions) and T010 (cabal edit) are independent at the source level but interdependent for build success — none compile alone. They land as one commit.
- T011 (audit) and T012 (gate) run last, after all source edits are in place.

### Parallel Opportunities

- T006, T007, T008, T009 are marked `[P]`: they touch different files from T004/T005 and from each other. A solo author can apply them in any order; they MUST NOT be split across commits.
- Inside Phase 6, T013 and T014 are independent activities (PR description vs CI watch).

---

## Implementation Strategy

### MVP First (Story 1 only — this PR)

1. Phase 1 setup checks (T001, T002).
2. Phase 2 baseline capture (T003).
3. Phase 3 vertical slice (T004–T012) — single commit.
4. **STOP**: post slice to #5286, wait for review per the pr skill solo-mode review checklist. Do not start Story 2 or 3 work.
5. Phase 6 polish (T013, T014).

### Incremental Delivery (across PRs, after this one)

1. This PR ships Story 1 → cert helpers gone, 7 callsites moved, 2 imports out of `Shelley/Transaction.hs`.
2. When #5243 lands minting + script-witness in `Transaction.Ledger`: open a new spec branch (or extend) for Story 2 → body construction migrated; ~25 cardano-api symbols out.
3. When #5243 lands ledger `signTransaction`: open a new spec branch for Story 3 → signing migrated; the last `Cardano.Api*` import in `Shelley/Transaction.hs` is removed; if no other `lib/wallet/` consumer remains, `build-depends: cardano-api` is dropped in the same slice (FR-006, SC-004).

---

## Notes

- `[P]` tasks = different files, no incomplete-task dependencies. They still fold into the same vertical commit for Story 1 (FR-005).
- `[Story]` label maps tasks to user stories for traceability.
- The single-commit contract means: do not run `git commit` between T004 and T012. Stage incrementally; commit once at T012 after the gate is green.
- Avoid: splitting Story 1 into per-file commits (rejected in research §"Decisions, Rationale, Alternatives" — would break `git bisect` because intermediate states do not compile); removing `cardano-api` from `build-depends` in this slice (rejected — `Shelley/Transaction.hs` still consumes it).
- After the commit lands, follow `feedback_always_open_pr.md` and `feedback_ci_loop.md`: PR #5286 already exists, so the new commit lands on the existing branch and CI is monitored to green.
