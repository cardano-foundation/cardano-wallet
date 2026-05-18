# Implementation Plan: Script-witness parity in `Transaction.Ledger`

**Branch**: `5288-script-witnesses` | **Date**: 2026-05-18 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/008-script-witness-parity/spec.md`

## Summary

Extend the ledger-native transaction builder
(`Cardano.Wallet.Shelley.Transaction.Ledger`) so it accepts every
script-witness shape currently consumed by the legacy
`Cardano.Wallet.Shelley.Transaction.mkUnsignedTx`: native-script
spending witnesses, a staking script for withdrawals/certificates,
mint/burn `ScriptSource` values (local script and reference input),
and an output-attached reference script. The acceptance proof is
byte-equivalence of the resulting ledger transaction body against
the legacy `mkUnsignedTx`-built body on six representative scenarios
plus the pre-existing non-script scenarios from #5287.

Approach: introduce one record (`ScriptWitnesses`) at the builder
boundary, pass it through `buildLedgerTx` / `buildLedgerTxRaw` to
`mkLedgerTx`, set the ledger body lenses
(`referenceInputsTxBodyL`, the first output's reference-script
field, withdrawal/cert script credentials) and the ledger witness
set (`witsTxL . scriptTxWitsL`). One vertical, bisect-safe slice
covers builder API + plumbing + parity tests; the existing
`mkUnsignedTx` is not modified.

## Technical Context

**Language/Version**: Haskell, GHC pinned via `flake.nix`.
**Primary Dependencies**: `cardano-ledger-api`, `cardano-ledger-conway`,
`cardano-balance-tx` (`Write.Tx era`), legacy `cardano-api` (kept
for reference comparison only).
**Storage**: N/A — pure transaction construction.
**Testing**: HSpec via `cardano-wallet-unit:unit`, namespace
`Cardano.Wallet.Shelley.TransactionLedger` (already present).
**Target Platform**: Linux dev, multi-platform CI; the slice is
platform-agnostic.
**Project Type**: Haskell monorepo (single-project layout under
`lib/`).
**Performance Goals**: None — parity, not throughput.
**Constraints**:

- No edits under `lib/integration/**` (parent #5243 policy).
- No changes to `mkUnsignedTransaction`, `mkUnsignedTx`, signing,
  or `cardano-api` build-depends.
- Dijkstra remains `pendingWith`.
- Plutus script witnesses remain explicitly out of scope.

**Scale/Scope**: One source module (`Transaction/Ledger.hs`), at most
one new auxiliary module if `ScriptWitnesses` is factored, and one
test module (`TransactionLedgerSpec`). Estimated diff under ~400 lines
including test scenarios.

## Constitution Check

Gates derived from `.specify/memory/constitution.md`:

| Principle | Status | Notes |
|---|---|---|
| I. Maintenance-First Stability | OK | Parity-only; no on-chain behavior change. Risk surface is the new builder branch, gated by byte-equivalence proof. |
| II. Era-Aware Design | OK | Conway only; Dijkstra branches stay `pendingWith` per #5209. |
| III. Type Safety as Security | OK | `ScriptWitnesses` is a typed record at module scope with explicit field names; no untyped tuples. |
| IV. Formal Specification | OK | `spec.md`, `research.md`, `data-model.md` committed under `specs/008-script-witness-parity/`. |
| V. Reproducible Builds | OK | `gate.sh` invokes everything via `nix develop --quiet -c`. |
| VI. Comprehensive Testing | OK | Unit tests cover all six scenarios; integration tests untouched per parent policy. |
| VII. Code Quality Gates | OK | `gate.sh` runs fourmolu/hlint/build/unit. |

No justified violations.

## Project Structure

### Documentation (this feature)

```text
specs/008-script-witness-parity/
├── plan.md              # This file
├── research.md          # Phase 0 decisions
├── data-model.md        # ScriptWitnesses carrier shape + body/wits map
├── spec.md              # Spec
├── quickstart.md        # Repro / verification recipe
└── checklists/
    └── requirements.md  # Spec-quality checklist
```

### Source code touched (repository)

```text
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/
├── Build.hs             # No change expected (mkLedgerTx is already era-flexible enough)
└── Ledger.hs            # ScriptWitnesses + plumbing + lens writes
lib/unit/test/unit/Cardano/Wallet/Shelley/
└── TransactionLedgerSpec.hs   # New describe block with the six parity scenarios
lib/unit/cardano-wallet-unit.cabal # Touched only if a new test module is split out (unlikely)
```

**Structure Decision**: keep all production changes inside
`Transaction/Ledger.hs`. The new carrier `ScriptWitnesses` lives in
the same module since it is part of the builder API; no separate
module is justified for one record. Tests join the existing
`TransactionLedgerSpec` rather than fan out into new test modules.

## Orchestration / subagent ownership split

This PR is delivered by the resolve-ticket orchestrator (the main
agent) with one implementation subagent run for the single
behavior-changing slice. Roles:

| Asset | Owner |
|---|---|
| `specs/008-script-witness-parity/spec.md`, `plan.md`, `data-model.md`, `research.md`, `tasks.md`, `checklists/` | Orchestrator |
| `gate.sh` | Orchestrator |
| PR metadata (title, body, draft↔ready) | Orchestrator |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` (script-witness extension) | Implementation subagent |
| `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` (RED scenarios + final passing assertions) | Implementation subagent |
| Any small helper imports inside the two files above | Implementation subagent |
| Anything else under `lib/wallet/` or `lib/unit/` | Out of scope; if touched, the subagent must report and the orchestrator decides. |
| Anything under `lib/integration/**` | Forbidden by parent policy. |

## Vertical slices

One bisect-safe implementation slice. The parity proof for any one
scenario only passes once the builder fully supports the matching
script-witness shape, so partial slices would force a failing-gate
intermediate commit. The slice's commit is what the subagent
returns; orchestrator accepts it, stamps `tasks.md`, and pushes.

If the slice's diff grows past what one subagent run can safely
ship (rough threshold: ≥500 changed lines including tests, or
the subagent runs `./gate.sh` more than three times without
convergence), the orchestrator stops, splits the work along a
clean line (most likely "builder API + scaffolding" → "scenario
tests"), and re-dispatches. The split point is named in
`tasks.md` so the second run inherits the contract.

### Slice 1 — `feat(5288): script-witness parity in Transaction.Ledger`

Files in scope:

- `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs`
- `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs`

Behavior change:

1. Add `ScriptWitnesses` and `noScriptWitnesses` to `Ledger.hs`'s
   export list and at module scope.
2. Extend `buildLedgerTx` and `buildLedgerTxRaw` with a
   `ScriptWitnesses` parameter.
3. Inside the builder, before calling `mkLedgerTx`, build:
   - the deduplicated set of reference inputs from
     `swMintingSources` `Right` branches,
   - the first-output reference-script attachment if
     `swReferenceScript` is set,
   - the staking-script-credential override on the
     `mkWithdrawalsLedger` call when `swStakingScript` is `Just`,
   - the set of native scripts to install in `witsTxL .
     scriptTxWitsL` (the `Left` branches of `swMintingSources`,
     `swNativeInputs` values, and `swStakingScript`).
4. Call `mkLedgerTx` with the extended body, then set the witness
   set fields on the returned `Tx era` via the appropriate lenses.
   Alternative: extend `mkLedgerTx` itself to take the scripts;
   chosen during implementation based on which keeps the diff
   minimal.
5. Update the two production call sites in `Ledger.hs`
   (`mkTransaction` and `constructUnsignedTxLedger`) to pass
   `noScriptWitnesses`. Update the three existing test call sites in
   `TransactionLedgerSpec` likewise.

Tests (RED then GREEN, in the same commit):

- Add a new `describe "ledger script-witness parity" $ do …` block
  in `TransactionLedgerSpec` covering the six scenarios from
  `spec.md` US1 ACs 1–6.
- Each scenario is one `it "<name>"` case that:
  1. Constructs the same hand-rolled selection + ctx values for
     both builders.
  2. Calls `mkUnsignedTx` (legacy) and serialises to a ledger body
     via `Read.Tx`.
  3. Calls `buildLedgerTx` (new) and reads its
     `bodyTxL` and `witsTxL` fields.
  4. Asserts byte-equality of the body CBOR and equality of the
     `scriptTxWitsL` map. (Witness-set equality is part of the
     parity contract; see D2 in research.md.)
- Property test: optional, deferred (D6 in research.md). If the
  subagent adds it, it must still pass the gate.

## Proof strategy (RED + GREEN per slice)

For Slice 1:

- **RED**: subagent adds the new `describe` block in
  `TransactionLedgerSpec` with the six scenarios as `it`s and, in
  the same uncommitted working tree, runs
  `nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 --test-options '--match="Cardano.Wallet.Shelley.TransactionLedger script-witness"'`.
  This must fail because the builder does not yet accept
  `ScriptWitnesses`. Subagent captures the failing-test tail in
  `WIP.md`.
- **GREEN**: subagent then adds `ScriptWitnesses` plumbing in
  `Ledger.hs`, updates non-script call sites to pass
  `noScriptWitnesses`, and re-runs the same command. Both the new
  script scenarios and the pre-existing
  `Cardano.Wallet.Shelley.TransactionLedger` tests must be green.
- **GATE**: subagent runs `./gate.sh` and captures its output. The
  orchestrator re-runs `./gate.sh` independently before accepting.

## Live-boundary check

Diagnostic per resolve-ticket: *"What system boundary does this
exercise that the unit suite cannot?"* — Answer: **none**.

The parity comparison runs two pure builders over hand-rolled
selections and compares CBOR bytes; there is no node, database,
API, or other live system in the picture. The unit suite is
conclusive. No live-boundary smoke is required in `gate.sh`, and
no operator follow-up is owed.

If a future child (#5285 / #5289) does need a live-chain proof
that a script-witnessed body submits and validates on a real node,
that smoke belongs to that child, not here.

## Carry-forward invariants from parent #5243

These must be honored by this PR and re-stated in any later child:

- No file under `lib/integration/**` may be modified.
  *Existing integration tests may be invoked unchanged as
  verification signal only.*
- The legacy `mkUnsignedTransaction` / `mkUnsignedTx` path remains
  the authoritative reference until #5285 retires it; do not
  modify either in this PR.
- The `cardano-api` build-dep stays in `cardano-wallet.cabal`;
  it is the final child #5290 that removes it.
- Conway is the supported recent era; Dijkstra is `pendingWith`.

## Risks and mitigations

- **Risk**: the legacy `mkUnsignedTx` route serialises body bytes in
  a particular field order (e.g., reference inputs as a set with
  the cardano-api-imposed ordering) that the ledger builder might
  not produce identically.
  *Mitigation*: the comparison is performed after both bodies
  cross into ledger-typed `TxBody era` (via `Read.Tx` on the
  legacy side). Ledger's `TxBody era` has canonical CBOR encoding
  in Conway (sets are serialised in canonical order, maps in
  key-sorted order), so the comparison is well-defined regardless
  of pre-canonicalisation field order. If a body field surfaces a
  divergence, the test diagnostic will name the lens that
  disagreed, which is enough to pinpoint the body-map row in
  `data-model.md` that needs adjustment.

- **Risk**: the witness-set comparison surfaces a divergence the
  body-byte comparison misses (e.g., if both `mkUnsignedTx` and
  the new builder happen to omit a script).
  *Mitigation*: the parity tests assert both
  `bodyTxL` byte-equality *and* `scriptTxWitsL` equality. The
  data-model lists which scripts must appear; if a scenario fails
  the `scriptTxWitsL` half, it is a real bug surfacing.

- **Risk**: a test scenario inadvertently exercises Plutus
  semantics.
  *Mitigation*: all native scripts in the fixtures are
  `Script KeyHash` literals (timelock-encodable). The builder
  routes via `toLedgerTimelockScript`. There is no path to a
  Plutus script in this PR.

- **Risk**: an existing call site of `buildLedgerTx` /
  `buildLedgerTxRaw` outside the two files listed above is
  missed.
  *Mitigation*: research D3 confirms only two production sites
  (both inside `Ledger.hs`) and three test sites (all in
  `TransactionLedgerSpec`). The subagent brief instructs the
  worker to grep before claiming completeness.

## Complexity Tracking

No constitution violations to justify. The slice's design is the
minimum surface for the parity contract.
