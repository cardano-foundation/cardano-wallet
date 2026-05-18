---

description: "Task list for 007-ledger-minting"

---

# Tasks: Ledger Body Builder Minting Support

**Input**: Design documents in `/code/cardano-wallet-5243/specs/007-ledger-minting/`
**Prerequisites**: `plan.md`, `spec.md`, `research.md`, `data-model.md`, `contracts/ledger-mint-translation.md`, `quickstart.md`

**Tests**: Required for every behaviour-changing slice. Per the PR skill, the RED tests and GREEN implementation fold into **one** reviewed commit (no test-only behaviour commits, no impl-only behaviour commits). Each task below states the commit it belongs to. The three-commit shape comes from `quickstart.md`.

**Organization**: User Story 1 (the builders) and User Story 2 (downstream unblocker) are both P1. US2 has no implementation work in this feature; it is a verification-only phase whose acceptance is "the next migration PR can be opened without further changes to the builders."

## Format: `[ID] [P?] [Story?] Description`

- **[P]**: independent file, parallelisable.
- **[Story]**: maps task to a user story.
- File paths are absolute below; commit boundaries are explicit.

---

## Phase 1: Setup (Shared)

**Purpose**: confirm green baseline before any change.

- [ ] T001 Confirm baseline test green on `origin/master` HEAD in `/code/cardano-wallet-5243`: run `cabal build cardano-wallet:lib:wallet primitive:lib:primitive -O0 -v0` then `cabal test cardano-wallet-unit:unit primitive:test:unit-tests -O0 -v0 --test-show-details=streaming`. **No commit.** If red, stop and report — never start a feature over a red baseline.
- [ ] T002 [P] Locate the convert-side spec module: `find /code/cardano-wallet-5243/lib/primitive/test -name 'ConvertSpec.hs' -o -path '*Primitive/Ledger/*Spec*.hs'`. In `specs/007-ledger-minting/notes.md`, record either (a) the canonical path of an existing spec module to extend, or (b) the explicit observation "no spec module found — Commit 1 must scaffold `Cardano.Wallet.Primitive.Ledger.ConvertSpec` and register it in `primitive.cabal`'s `test-suite` stanza". Case (b) folds into Commit 1 (does not become a new commit), but T010/T011 must explicitly include the scaffolding + cabal-stanza edit; flag this dependency in the commit body. **No commit yet.**

---

## Phase 2: Foundational (Commit 1: converter)

**Purpose**: introduce the `toLedgerMintValue` translation with its full property suite. Blocks Phase 3 because Phase 3 callers translate via this function.

**Folding rule**: T010 (RED, properties) and T011 (GREEN, implementation) are authored in one editing session and committed together as a single reviewed commit titled
`feat(primitive): toLedgerMintValue (TokenMap, TokenMap) → MultiAsset`.
Do not push T010 alone; do not push T011 alone. Bisect-safety requires the property suite and the function to land atomically.

- [ ] T010 [US1] Write properties P1–P8 (`prop_total`, `prop_empty`, `prop_mint_only`, `prop_burn_only`, `prop_netting`, `prop_no_phantom_keys`, `prop_no_empty_buckets`, `prop_roundtrip_disjoint`) from `contracts/ledger-mint-translation.md` in the spec module identified by T002 (`/code/cardano-wallet-5243/lib/primitive/test/.../Ledger/ConvertSpec.hs`). Reuse `Arbitrary TokenMap` from existing primitive generators; if missing, add a re-export rather than re-rolling. Run the spec; properties must FAIL because `toLedgerMintValue` does not exist. Capture the failure output as evidence in the commit body of the eventual commit (see folding rule).
- [ ] T011 [US1] Implement `toLedgerMintValue :: TokenMap -> TokenMap -> MultiAsset` in `/code/cardano-wallet-5243/lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Convert.hs`, placed adjacent to `toLedgerTokenBundle`. Reuse `toLedgerTokenPolicyId`, `toLedgerAssetName`, and a new private `toLedgerSignedQuantity` (or inline `toInteger`). Implement the netting rule from `data-model.md` §"Translation rules": `q = toInteger m - toInteger b`, drop entries with `q == 0`, drop empty policy buckets. Export the symbol from the module's export list. Re-run the spec; all P1–P8 must PASS. Run `just check-fmt`. Stage **both** T010 and T011's edits and commit once with title `feat(primitive): toLedgerMintValue (TokenMap, TokenMap) → MultiAsset`; commit body must reference issue #5243 and quote the property names covered.

**Checkpoint**: Commit 1 is bisect-safe — the converter exists with full property coverage.

---

## Phase 3: User Story 1 (Commits 2 + 3: builder plumbing + Wallet.hs wiring)

**Story**: US1 — "Wallet's ledger body builder honours the mint requested by its caller."
**Independent test**: drive `buildLedgerTx` and `buildLedgerTxRaw` with a representative `(TokenMap, TokenMap)` and inspect `view mintTxBodyL` of the produced body; equal to `toLedgerMintValue mint burn`.

### Commit 2: `feat(wallet): plumb mint through buildLedgerTx / buildLedgerTxRaw`

**Folding rule**: T020 (RED, builder properties B1–B3) and T021–T024 (GREEN, builder + caller edits) fold into one commit.

- [ ] T020 [US1] In `/code/cardano-wallet-5243/lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` add a "Mint plumbing" describe block with properties B1, B2, B3 from `contracts/ledger-mint-translation.md`:
  - B1: `view mintTxBodyL (buildLedgerTx … mintVal) === mintVal` for `RecentEraConway`.
  - B2: same for `buildLedgerTxRaw` on both `Left PreSelection` and `Right (SelectionOf TxOut)` branches.
  - B3: empty-input regression — passing `mempty` for the new parameter yields a transaction body structurally equal (under `(==)` on `Tx era`) to the body the current `mempty -- TODO` builder produces. Compare structurally, not via wire-byte equality.
  Run the spec; B1/B2/B3 must FAIL because the builders still hardcode `mempty`.
- [ ] T021 [US1] In `/code/cardano-wallet-5243/lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs`, add an explicit `MultiAsset` parameter to `buildLedgerTx` (signature at lines 411–423). Thread it to the `mkLedgerTx` call site, replacing the `mempty -- TODO: minting support` at line 433. Do **not** invent a new caller-side translation; the parameter is already-translated.
- [ ] T022 [US1] In the same file, add the same `MultiAsset` parameter to `buildLedgerTxRaw` (signature at lines 465–477), replacing the `mempty -- TODO: minting support` at line 496. Both builders must end up with the parameter in the same positional slot — keep the surfaces symmetric.
- [ ] T023 [US1] Update `mkTransactionLedger` (around line 266, call site at line 306 in `Ledger.hs`) to extract `txAssetsToMint` and `txAssetsToBurn` from its `TransactionCtx` (`fst` of each pair — drop the `ScriptSource` map; script witnesses are out of scope) and call `toLedgerMintValue mint burn` to produce the new argument to `buildLedgerTx`.
- [ ] T024 [US1] Update `constructUnsignedTxLedger` (around lines 376–408 in `Ledger.hs`, call site at line 399) to take an explicit `(TokenMap, TokenMap)` parameter (mints, burns), translate via `toLedgerMintValue`, and pass the result to `buildLedgerTxRaw`. Stage T020–T024 together, run the spec — B1/B2/B3 must PASS — run `just check-fmt`, and commit once with title `feat(wallet): plumb mint through buildLedgerTx / buildLedgerTxRaw`.

**Checkpoint after Commit 2**: the builders accept and honour mint; the only remaining hardcoded site is the top-level wallet caller.

### Commit 3: `feat(wallet): pass mint/burn from Cardano.Wallet to constructUnsignedTxLedger`

- [ ] T030 [US1] In `/code/cardano-wallet-5243/lib/wallet/src/Cardano/Wallet.hs` around line 2746, the single call site of `constructUnsignedTxLedger`, locate the in-scope `TransactionCtx` (or equivalent) and pass `(fst txAssetsToMint, fst txAssetsToBurn)` to the new parameter added in T024. Rebuild; run the full unit suite — no regressions. Commit as a single reviewed commit titled `feat(wallet): pass mint/burn from Cardano.Wallet to constructUnsignedTxLedger`.

**Checkpoint after Commit 3**: every reachable call path into the ledger builder carries the real mint set; the two `mempty -- TODO: minting support` strings no longer exist in `lib/wallet/src`.

---

## Phase 4: User Story 2 (verification only — no implementation)

**Story**: US2 — "Migration of `mkUnsignedTransaction` to the ledger path is unblocked on minting." Acceptance is **observational**: after this feature, the follow-up migration PR can be opened with no further mint-related blockers in the ledger builder. There are no source edits in this phase.

- [ ] T040 [US2] Run the smoke commands from `quickstart.md` §"How to verify minting is plumbed":
  - `rg 'mempty -- TODO: minting support' lib/wallet/src` → must return zero matches.
  - `rg '^toLedgerMintValue' lib/primitive` → must return at least one definition match in `Convert.hs`.
  - `rg 'buildLedgerTx(Raw)?\b' lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` → output must show the new `MultiAsset` parameter in the signatures.
  - **FR-008 guard**: `git diff origin/master -- lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs | rg -i 'ScriptSource'` → must return zero matches. The slice must not introduce script-witness handling; if any line appears, fix it before the next commit.
  Record the output in the PR description. **No commit.**
- [ ] T041 [US2] Sanity-only: paste a one-paragraph note in the PR description confirming that script-witness work remains the documented next blocker, citing the relevant Out-of-Scope item in `spec.md`. This satisfies User Story 2's acceptance scenario ("a reviewer of the follow-up migration PR can verify that minting is no longer silently dropped without reading any code outside the migration diff"). **No commit.**

---

## Phase 5: Polish & Cross-Cutting Concerns

**Purpose**: housekeeping that does not change behaviour. Fold into commit 2 if reviewer prefers, but standalone is acceptable here because these are not behaviour-bearing.

- [ ] T050 [P] Run `just check-fmt` and `just check-hlint` across the touched files; fold any required edits into the commit that introduced the offending line (per the PR skill).
- [ ] T051 [P] Verify era coverage: `rg 'RecentEraConway' /code/cardano-wallet-5243/lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Build.hs` — confirm `mkLedgerTx`'s era restriction is unchanged and that the new code does not pretend to support Dijkstra. If a new era check is needed it is **out of scope** — open a follow-up issue.
- [ ] T052 Run the full unit suite once more end-to-end: `cabal test cardano-wallet-unit:unit primitive:test:unit-tests -O0 -v0`. **Before pushing**, run the flake gate locally: `nix build .#checks.* --quiet` — `just ci` and `cabal test` are not equivalent to the flake checks, and pushing past a red flake gate predictably surfaces as red CI (memory `feedback_local_ci_gap`). Only after the flake gate is green: push the branch and open a draft PR linked to issue #5243 (per the `feedback_always_open_pr` memory).
- [ ] T053 After CI runs, loop-check it green (per the `feedback_ci_loop` memory). Address any failures in the commit that introduced them; do not add fixup commits.

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: no dependencies.
- **Phase 2 (Commit 1, converter)**: depends on Phase 1. Blocks Phase 3.
- **Phase 3 (Commits 2 + 3, builders + Wallet.hs)**: depends on Phase 2. Commit 2 blocks Commit 3 because Commit 3 calls into the parameter added in Commit 2 (T024).
- **Phase 4 (US2 verification)**: depends on Phase 3 complete.
- **Phase 5 (Polish)**: depends on Phase 3 complete; runs alongside Phase 4.

### Within each commit

- RED test task is authored first, observed to fail, **then** GREEN implementation is added.
- Both are staged and committed together (folding rule). The commit message body cites which properties the commit makes pass.

### Parallel opportunities

- T001, T002 in Phase 1 are mutually independent ([P]).
- Inside Commit 2: T021 and T022 touch the same file (`Ledger.hs`) and cannot be parallelised. T023 and T024 also touch `Ledger.hs`. Author them sequentially.
- T050 and T051 in Phase 5 are independent of each other ([P]).

---

## Parallel Example: Phase 1

```bash
# T001 and T002 run independently:
cabal build cardano-wallet:lib:wallet primitive:lib:primitive -O0 -v0 \
  && cabal test cardano-wallet-unit:unit primitive:test:unit-tests -O0 -v0
find /code/cardano-wallet-5243/lib/primitive/test \
  \( -name 'ConvertSpec.hs' -o -path '*Primitive/Ledger/*Spec*.hs' \)
```

## Implementation Strategy

The three-commit shape and its rationale live in `quickstart.md` §"Expected commit shape" — that is the single source of truth. The phase headers in this file map onto it (Phase 2 = Commit 1, Phase 3 = Commits 2 + 3, Phases 4–5 introduce no behaviour commits). See `quickstart.md` for the canonical commit titles, bodies, and bisect contract.

### MVP scope

US1 is the MVP — Phase 1 + Phase 2 + Phase 3 deliver the entire feature. US2 is verification-only.

---

## Notes

- The PR skill's RED+GREEN fold rule applies to every behaviour-changing commit in this feature. Commits 1 and 2 are behaviour-changing; commit 3 is a one-line wiring change that exercises code already covered by Commit 2's properties (no new RED required, the existing builder properties carry forward).
- T040 and T041 produce no commits; they produce text for the PR description.
- The `feedback_pr_descriptions` memory applies: the PR body must reflect the final merged state, not the journey.
- Out of scope, never sneak in: script witnesses, `mkUnsignedTransaction` migration, Dijkstra era support, any changes to the `cardano-api` body builder path.
