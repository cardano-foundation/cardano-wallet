# Feature Specification: Script-witness parity in `Transaction.Ledger`

**Feature Branch**: `5288-script-witnesses`
**Created**: 2026-05-18
**Status**: Draft
**Input**: GitHub issue cardano-foundation/cardano-wallet#5288, child of #5243.

## Context (non-normative)

This is one slice of the transaction-layer migration off `cardano-api`
(parent epic #5243). #5287 already plumbed mint/burn `MultiAsset` through
`buildLedgerTx` and `buildLedgerTxRaw`. The remaining blocker before
`mkUnsignedTransaction` itself can be moved off
`Cardano.createTransactionBody` (the next child #5285) is script-witness
parity: the legacy `mkUnsignedTx` accepts native-script inputs, a
staking script, mint/burn `ScriptSource` values (local script or
reference input), and an output reference script, and the ledger
builder must accept the same surface and produce the same on-chain body
fields.

This spec covers **only** the parity surface. Migrating
`mkUnsignedTransaction`, deleting certificate helper modules, signing,
Dijkstra-era extensions, and `cardano-api` build-dep removal are all
out of scope (tracked by #5285, #5289, #5290).

## User Scenarios & Testing *(mandatory)*

### User Story 1 — Body-byte parity for representative script scenarios (Priority: P1)

As a wallet maintainer continuing the `cardano-api` removal, I can
build an unsigned ledger transaction body via the
`Cardano.Wallet.Shelley.Transaction.Ledger` builders for every
script-witness scenario currently handled by
`Cardano.Wallet.Shelley.Transaction.mkUnsignedTx`, and the resulting
body matches the legacy `mkUnsignedTx`-built body byte-for-byte for
representative scenarios.

**Why this priority**: This is the only acceptance criterion that
unblocks the rest of the parent epic. Without provable byte parity,
the next child (#5285) cannot route `mkUnsignedTransaction` onto the
ledger builder without risk of changing on-chain transaction shapes.

**Independent Test**: A unit / golden test in
`Cardano.Wallet.Shelley.TransactionLedgerSpec` constructs an unsigned
body via both `mkUnsignedTx` and the extended ledger builder from the
same wallet-level inputs (selection, withdrawal, certs, mint, burn,
native-script inputs, staking script, mint sources, output reference
script) and asserts byte-equivalent ledger bodies in the recent era
(Conway). Each scenario is one independent test case.

**Acceptance Scenarios**:

1. **Given** a `SelectionOf TxOut` whose inputs carry native-script
   spending witnesses via `txNativeScriptInputs`,
   **When** both builders run on the same inputs,
   **Then** the ledger bodies are byte-equal and both bodies' input
   sets match the selection.
2. **Given** a withdrawal plus delegation certs that must be witnessed
   by a staking native script (`stakingScriptM = Just …`),
   **When** both builders run,
   **Then** the ledger bodies are byte-equal, the certificate uses the
   staking-script credential, and the withdrawal references the
   staking-script credential.
3. **Given** a mint+burn `MultiAsset` whose policies are backed by
   *local* native scripts via `mintingSource`,
   **When** both builders run,
   **Then** the ledger bodies are byte-equal and `mintTxBodyL` matches
   the `toLedgerMintValue` already proven by #5287.
4. **Given** a mint+burn whose policies are backed by *reference*
   inputs (`ScriptSource = Right (ReferenceInput …)`),
   **When** both builders run,
   **Then** the ledger bodies are byte-equal and
   `referenceInputsTxBodyL` contains exactly those reference inputs.
5. **Given** an output reference script (`txReferenceScript = Just …`),
   **When** both builders run,
   **Then** the ledger bodies are byte-equal and the first output
   carries the reference-script field set to the timelock encoding of
   the wallet `Script KeyHash`.
6. **Given** a mixed scenario that combines several of the above
   simultaneously (multisig inputs + staking script + local mint +
   reference-input mint + output reference script),
   **When** both builders run,
   **Then** the ledger bodies are byte-equal.

### User Story 2 — Non-script scenarios remain green (Priority: P2)

As the same maintainer, I can confirm that existing non-script
ledger-builder behavior (covered by the current
`TransactionLedgerSpec` suite plus the `ledgerMintPlumbingSpec` from
#5287) continues to pass after the extension.

**Why this priority**: This guards against regressions in already-shipped
work. It is P2 because it is enforced by the existing test suite; the
new scenarios in US1 are the proof artifact this PR adds.

**Independent Test**: Re-running
`cardano-wallet-unit:unit --match="Cardano.Wallet.Shelley.TransactionLedger"`
against the new code on a selection with no scripts, no mint, no
staking script, and no reference script produces a body byte-equal to
the legacy one.

**Acceptance Scenarios**:

1. **Given** a `defaultTransactionCtx`-shaped scenario (no scripts of
   any kind),
   **When** both builders run,
   **Then** the ledger bodies are byte-equal and the
   `ledgerMintPlumbingSpec` invariants from #5287 continue to hold.

### Edge Cases

- A native-script input map (`txNativeScriptInputs`) whose keys do
  not correspond to selection inputs must surface as a deterministic
  builder error (matching the legacy "each input should have script in
  multisig" failure mode), not as a silently malformed body. The
  legacy builder errors via `error` in this case; parity does not
  require preserving the exact `error` call, but the new builder must
  fail in a way the unit test can pin.
- A `mintingSource` reference-input whose `TxIn` is identical for
  multiple policies must produce a single reference input in the body
  (parity with the legacy de-duplication that arises from
  `Map.unionsWith` + body set semantics).
- A staking script combined with a withdrawal that has zero coin
  must still produce a script-witnessed withdrawal in the body
  (matches the legacy `ScriptWitness ScriptWitnessForStakeAddr`
  branch).
- An output reference script combined with an empty outputs list
  produces a body with no outputs and no orphaned reference script;
  this matches the legacy "no outputs" branch.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The `Cardano.Wallet.Shelley.Transaction.Ledger` module
  MUST expose a builder API that accepts every script-witness input
  currently consumed by `mkUnsignedTx`: `txNativeScriptInputs ::
  Map TxIn (Script KeyHash)`, `stakingScriptM :: Maybe (Script
  KeyHash)`, mint/burn `ScriptSource` values (`Map AssetId
  ScriptSource`), and `txReferenceScript :: Maybe (Script KeyHash)`.
- **FR-002**: For each of those script-witness shapes, the ledger
  builder MUST set the corresponding ledger body field
  (`inputsTxBodyL`, `referenceInputsTxBodyL`, `mintTxBodyL`,
  `withdrawalsTxBodyL`, `certsTxBodyL`, `outputsTxBodyL`) so that the
  resulting `Write.Tx era` serialises to bytes equal to those of the
  legacy `mkUnsignedTx`-built body for the same inputs in the recent
  era (Conway).
- **FR-003**: The existing mint/burn parity already proven by #5287
  (`buildLedgerTx` and `buildLedgerTxRaw` carry `toLedgerMintValue`
  unchanged) MUST be preserved across the changes in this PR.
- **FR-004**: Existing non-script unit specs under
  `Cardano.Wallet.Shelley.TransactionLedgerSpec` MUST continue to
  pass without modification of their assertions; new tests cover the
  added script-witness scenarios.
- **FR-005**: Dijkstra-era behaviour MUST remain at parity with the
  current state of the ledger builder — that is, the existing
  `pendingWith "TODO: Dijkstra"` branches stay as `pendingWith`, and
  the new builder MUST NOT introduce additional Dijkstra-only
  obligations. Any Dijkstra extension is tracked by #5209.
- **FR-006**: No source under `lib/integration/**` may be modified.
  Existing integration tests may be invoked unchanged as verification
  signal only.
- **FR-007**: Parity proof MUST consist of unit or golden tests inside
  `lib/unit/` that exercise both `mkUnsignedTx` (legacy) and the new
  ledger builder on the same inputs and assert byte equality of the
  resulting ledger bodies.
- **FR-008**: `mkUnsignedTx` and `mkUnsignedTransaction` themselves
  MUST NOT be modified by this PR. The legacy code path remains the
  reference implementation for parity; #5285 (the next child) is the
  ticket that retires it.

### Key Entities

- **Native-script input map** — a `Map TxIn (Script KeyHash)`
  associating spending inputs with the native script that must witness
  them. Inputs not present in this map are assumed key-witnessed.
- **Staking script** — an optional `Script KeyHash` whose hash is the
  stake credential used by withdrawals and certificates instead of a
  key hash.
- **Mint script source** — a `Map AssetId ScriptSource` where
  `ScriptSource = Either (Script KeyHash) ReferenceInput`; the `Left`
  branch carries the policy script inline, the `Right` branch points
  at a reference input that holds the policy script on-chain.
- **Output reference script** — an optional `Script KeyHash` attached
  to the first wallet-built output, mirroring the legacy
  `toCardanoTxOut shelleyEra refScriptM firstOut` call.
- **Legacy body** — the `Cardano.TxBody` returned by `mkUnsignedTx`,
  converted to ledger bytes for comparison. This is the parity
  reference.
- **Ledger body** — the `TxBody era` field of the `Write.Tx era`
  returned by the extended builder. This is the parity subject.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: For every scenario listed in User Story 1, the unit
  suite asserts byte-equal ledger bodies between the legacy and new
  builders (six scenarios at minimum).
- **SC-002**: The full `cardano-wallet-unit:unit` test pattern
  `Cardano.Wallet.Shelley.TransactionLedger` is green on the PR head.
- **SC-003**: The PR diff contains zero modifications to files under
  `lib/integration/**`.
- **SC-004**: `Cardano.Wallet.Shelley.Transaction.mkUnsignedTransaction`
  and `mkUnsignedTx` are byte-identical to their `origin/master`
  versions; the diff against `origin/master` for those two functions
  is empty.
- **SC-005**: `gate.sh` runs green at HEAD before the PR is marked
  ready for review.

## Assumptions

- The recent era for parity proof is Conway. Dijkstra is `pendingWith`
  for the foreseeable future (per #5209 and the existing pattern in
  `TransactionLedgerSpec`).
- "Body bytes" means the CBOR encoding of the ledger `TxBody era`,
  not the full `Tx` envelope. Witness sets and aux-data attachment
  surface (needed by future signing in #5289) are out of scope here.
- The legacy `mkUnsignedTx` is the authoritative reference for byte
  shape. If it has a latent quirk (e.g. specific ordering inside a set
  or strictness around empty fields), parity means matching that quirk
  in the ledger builder; the quirk is not "fixed" in this PR.
- Plutus script witnesses remain explicitly out of scope. The legacy
  `mkUnsignedTx` source comment states the function "is never used
  with Plutus scripts"; the new builder inherits that boundary.
- The wallet-level `Script KeyHash → Timelock` conversion already
  available via `toLedgerTimelockScript` is the correct source for
  building ledger native-script values; no new script-language
  decisions are required.
- Carrying the native-script set alongside the body (so #5289 can
  attach witnesses) MAY be required and will be designed during
  planning. It is a means to the parity end, not an additional
  feature.
