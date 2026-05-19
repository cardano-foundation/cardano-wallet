---
description: "Task list for #5288 — script-witness parity in Transaction.Ledger"
---

# Tasks: Script-witness parity in `Transaction.Ledger`

**Input**: Design documents in `/specs/008-script-witness-parity/`
**Prerequisites**: [spec.md](./spec.md), [plan.md](./plan.md),
[research.md](./research.md), [data-model.md](./data-model.md),
[quickstart.md](./quickstart.md)

**Tests**: This PR is a parity proof, so test tasks are mandatory
(unit tests in `cardano-wallet-unit:unit`, including a QuickCheck
property — see T013).

**Organization**: The PR has one bisect-safe behavior-changing slice
(see plan D4). Within that slice, US1 (parity for the six
enumerated script scenarios), US2 (no regression on pre-existing
non-script scenarios), and US3 (property-driven parity over random
selections) are exercised by the same code change. Their tasks
**fold into a single commit** the subagent returns; the orchestrator
stamps `[X]` against this very file and amends the subagent's HEAD
commit (see plan "Proof strategy").

## Format: `[ID] [P?] [Story] Description`

- **[P]** — parallelisable (different files, no dependencies on
  incomplete tasks).
- **[Story]** — which user story this task belongs to (`US1`, `US2`,
  `US3`).
- File paths are absolute against the repo root.

## Path conventions

This is a Haskell monorepo. Production code lives in
`lib/wallet/src/`; unit tests live in `lib/unit/test/unit/`. No file
under `lib/integration/**` may be modified by this PR (parent #5243
policy; FR-006 in `spec.md`).

---

## Phase 1: Setup

- [X] T000 Bootstrap worktree, `gate.sh`, draft PR
  ([commit `0c778c42cc`](https://github.com/cardano-foundation/cardano-wallet/commit/0c778c42cc),
   [PR #5291](https://github.com/cardano-foundation/cardano-wallet/pull/5291))
- [X] T001 Commit `spec.md` + spec-quality checklist for the specs
  stop ([commit `039bc97e72`](https://github.com/cardano-foundation/cardano-wallet/commit/039bc97e72))
- [X] T002 Commit `plan.md`, `research.md`, `data-model.md`,
  `quickstart.md` for the plan stop
  ([commit `7fb23fb462`](https://github.com/cardano-foundation/cardano-wallet/commit/7fb23fb462))

---

## Phase 2: Foundational

No foundational tasks. The slice is self-contained inside two
existing modules; there is no shared infrastructure to land first.

---

## Phase 3: User Story 1 — Body+wits byte parity for six scenarios (Priority: P1) 🎯

**Goal**: For each of the six representative scenarios from
`spec.md` US1 (native-script input, staking script, local mint,
reference-input mint, output reference script, mixed), the body
CBOR of the new ledger builder equals the body CBOR of the legacy
`mkUnsignedTx`-built body (after `Read.Tx` conversion), AND the
ledger builder's `witsTxL . scriptTxWitsL` matches the scripts
embedded by the legacy route.

**Independent test**:

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
    --test-options \
        '--match="Cardano.Wallet.Shelley.TransactionLedger script-witness"'
```

Expect six `it`-cases under
`describe "ledger script-witness parity"` all green, plus the
property from T013.

### RED (test-first)

- [ ] T010 [US1] Add `describe "ledger script-witness parity"` with
  six failing scenarios to
  `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`.
  Each `it`-case constructs the same selection/ctx values, calls
  `mkUnsignedTx` (legacy) and the extended `buildLedgerTx` (new),
  and asserts equality of (a) body CBOR after `Read.Tx` conversion
  and (b) `witsTxL . scriptTxWitsL`. Run the focused `--match`
  pattern and observe FAILURE. Capture the failing-test tail in
  `WIP.md`.

### GREEN (implementation)

- [ ] T011 [US1] Extend
  `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs`:
  add `ScriptWitnesses`/`noScriptWitnesses` (export both), grow
  `buildLedgerTx` and `buildLedgerTxRaw` with a `ScriptWitnesses`
  parameter, plumb the body lenses listed in `data-model.md`
  "Body-field map" (`referenceInputsTxBodyL`, withdrawal
  script-credential, cert script-credential, first-output reference
  script), and set `witsTxL . scriptTxWitsL` on the returned
  `Write.Tx era`. Update the two production call sites in the same
  module (`mkTransaction`, `constructUnsignedTxLedger`) and the
  three pre-existing test call sites in
  `TransactionLedgerSpec` to pass `noScriptWitnesses`. Re-run the
  focused `--match` and observe PASS.

**T010 + T011 fold into a single bisect-safe commit** with T012 and
T013 below. See "Folding" at the end of this section.

---

## Phase 4: User Story 2 — No regression on pre-existing scenarios (Priority: P2)

**Goal**: After T011, the pre-existing `TransactionLedgerSpec`
suite (including `ledgerMintPlumbingSpec` from #5287) continues to
pass without any change to its assertions.

**Independent test**:

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
    --test-options \
        '--match="Cardano.Wallet.Shelley.TransactionLedger"'
```

This is the same suite the gate.sh runs; both the new
`script-witness` block and the pre-existing tests must be green.

### Regression check

- [ ] T012 [US2] Within the same uncommitted working tree as T010+T011,
  re-run the broader pattern above and observe all pre-existing
  cases (including `ledgerMintPlumbingSpec`,
  `binaryCalculationsSpec`, `feeEstimationRegressionSpec`, the
  Sign-transaction property suites, the witness round-trip
  property) PASS. Capture the passing-run tail in `WIP.md`. No
  assertion changes are permitted in this task — the production
  code change in T011 must keep them green by construction.

---

## Phase 5: User Story 3 — Property-driven parity over random selections (Priority: P3)

**Goal**: Parity holds not just on the six enumerated scenarios but
on randomly generated `(SelectionOf TxOut, ScriptWitnesses)` pairs
within the documented generator bounds. This is a confidence
multiplier that mechanically explores combinations the enumerated
cases cannot. Reversal of plan D6: the QuickCheck property is
**mandatory**, not optional.

**Independent test**:

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
    --test-options \
        '--match="Cardano.Wallet.Shelley.TransactionLedger script-witness prop_buildLedgerTx_matches_mkUnsignedTx_on_script_witnesses"'
```

### Property test

- [ ] T013 [US3] Add ONE QuickCheck property named exactly
  `prop_buildLedgerTx_matches_mkUnsignedTx_on_script_witnesses` to
  the same `describe "ledger script-witness parity"` block in
  `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`.
  The property's generator produces a random
  `(SelectionOf TxOut, ScriptWitnesses)` pair within these
  bounds:
  * One to three selection inputs.
  * Zero to three native-script inputs (a subset of the selection
    inputs, each carrying a random `Script KeyHash` of depth ≤ 3
    via `Cardano.Wallet.Gen.genScript`).
  * Zero or one staking script.
  * Zero to three mint/burn `ScriptSource` entries (mix of
    `Left script` and `Right ReferenceInput`).
  * Zero or one output reference script.
  The property asserts the same two equalities as the enumerated
  cases: body-CBOR equality, and `scriptTxWitsL` equality. Bound
  with `withMaxSuccess 100`. A shrunk counter-example is an
  implementation bug; do NOT weaken the generator or skip the
  property. Run the focused `--match` and observe PASS.

---

## Folding into one commit

**T010 + T011 + T012 + T013 fold into a single bisect-safe
commit.** The subagent performs them in order on the same
uncommitted working tree:

1. T010 (RED) — observe failing, record tail.
2. T011 (GREEN) — observe passing on the focused `--match`, record
   tail.
3. T012 (regression) — observe passing on the broader `--match`,
   record tail.
4. T013 (property) — observe passing on the property's `--match`,
   record tail.
5. `./gate.sh` — observe passing, record tail.
6. ONE commit with subject
   `feat(5288): script-witness parity in Transaction.Ledger` and
   trailer `Tasks: T010, T011, T012, T013`.

The brief that enforces this contract on the subagent lives at
[briefs/T010-T013.md](./briefs/T010-T013.md). The orchestrator
embeds that file in the `Agent` tool's `prompt` field at dispatch
time. The subagent loads **no** process skill.

**Checkpoint**: after the single slice commit lands, US1, US2 and
US3 are all satisfied. The PR is now functionally complete and
ready for finalisation.

---

## Phase 6: Polish & finalization

- [ ] T020 Verify `gate.sh` is green at HEAD on the orchestrator
  side. Re-run independently of the subagent's report
  ([gate.sh](https://github.com/cardano-foundation/cardano-wallet/blob/5288-script-witnesses/gate.sh)).
- [ ] T021 Update PR body to reflect delivered behavior (drop the
  "tasks stop" wording, replace with completed scope per
  `spec.md` Success Criteria).
- [ ] T022 Run the finalization audit
  (`gate-script` skill, `finalization_audit`): every commit on the
  branch passes the commit message gate; `tasks.md` has no open
  `[ ]` items in the behavior-changing phases.
- [ ] T023 Drop `gate.sh` in a dedicated `chore: drop gate.sh
  (ready for review)` commit; push; `gh pr ready 5291`.

T020–T023 are orchestrator-owned mechanical work. None of them
change behavior, so they do not require a subagent and do not
carry a `Tasks:` trailer (`commit_gate` exempts `chore:`/`docs:`).

---

## Dependencies & execution order

```text
T000 → T001 → T002 → (T010 ⊕ T011 ⊕ T012 ⊕ T013, one commit)
                  → T020 → T021 → T022 → T023
```

`⊕` denotes "folded into one commit". The folding is enforced by
the external brief (`Make exactly ONE commit`).

---

## Parallel opportunities

None within this slice. T010 → T011 is strict RED → GREEN ordering.
T012 strictly follows T011 (it asserts T011 did not regress
anything). T013 strictly follows T011 (it depends on the new
`buildLedgerTx` signature). All four live in one subagent run.

If the slice splits per plan "Vertical slices" fallback (e.g.
builder-API+scaffolding | scenario-tests | property), the second
and third slices each get their own subagent run with their own
briefs, derived from `briefs/T010-T013.md` but narrowed to the
post-split scope.

---

## Implementation strategy

1. Phase 1 done. Phase 2 empty.
2. Dispatch the implementation subagent with the brief at
   [briefs/T010-T013.md](./briefs/T010-T013.md).
3. Live-tail `/code/cardano-wallet-5288/WIP.md` per the
   resolve-ticket "Dispatch and Live Tail" protocol.
4. On subagent completion, review the diff + gate output; if
   approved, amend HEAD to stamp T010 / T011 / T012 / T013 as
   `[X]` in this file (single allowed amend).
5. Push the amended slice commit.
6. Phase 6: drop `gate.sh`, mark ready, run finalization audit.

---

## Notes

- T010 + T011 + T012 + T013 fold into one commit. The folding is
  enforced by the subagent brief (`Make exactly ONE commit`) and
  re-checked by the orchestrator's review.
- The `Tasks:` trailer on the single slice commit lists all four
  task ids; the `tasks.md` checkboxes for all four are flipped to
  `[X]` in the same amended commit, per `gate-script` "Stamping a
  reviewed slice".
- T020–T023 are `chore:`/`docs:` work the orchestrator performs
  directly; no subagent dispatch.
- The QuickCheck property in T013 is **mandatory** — supersedes
  plan D6's "optional" stance. Confidence-multiplier framing is
  retained; gating is now explicit.
