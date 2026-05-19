# Research: Script-witness parity in `Transaction.Ledger`

**Branch**: `5288-script-witnesses` | **Spec**: [spec.md](./spec.md)

## Scope

This is a Phase-0 research pass that resolves the design decisions
the planning step needs:

1. Where exactly does the legacy `mkUnsignedTx` write each
   script-witness fact onto the on-chain body?
2. What is the ledger-typed equivalent for each fact (which lens or
   ledger field carries it)?
3. Where do native scripts live in a ledger `Tx` such that the future
   signing path (#5289) can still find them?
4. How does the ledger builder return shape need to change to carry
   the witness set, given that the existing
   `buildLedgerTx`/`buildLedgerTxRaw` return only `Write.Tx era`?
5. What is the smallest API surface change at the call sites so the
   slice stays bisect-safe?

## Decision log

### D1 — Body-field mapping table

Decision: implement the body side of parity by setting these exact
ledger lenses inside `mkLedgerTx` (and threading the script-witness
inputs through `buildLedgerTx` / `buildLedgerTxRaw` to reach it).

| Legacy `mkUnsignedTx` write | Ledger body lens | Notes |
|---|---|---|
| `txIns = inputWits` with `ScriptWitnessForSpending` | `inputsTxBodyL` | Inputs are already present; the script itself goes in the witness set, not the body. |
| `txInsReference = TxInsReference (toNodeTxIn …)` from `mintingSource` `ReferenceInput` branch | `referenceInputsTxBodyL` | Deduplicate `TxIn`s. Multiple policies may share a reference input. |
| `txOuts` with `refScriptM` on the first output | `outputsTxBodyL` (first element's reference-script field) | Output-level reference script. The legacy code attaches it only to the first output; we mirror that. |
| `txWithdrawals` with `ScriptWitnessForStakeAddr` | `withdrawalsTxBodyL` | Withdrawal credential becomes a script hash via the existing `Cred.ScriptHashObj` path. |
| `txCertificates` with `StakeCredentialByScript` | `certsTxBodyL` | `certificateFromDelegationActionLedger` *already* accepts `Either XPub (Script KeyHash)` (see `Ledger.hs:660-695`). Today `mkTransaction` always passes `Left xpub`; we extend the call sites to pass `Right script` when `stakingScriptM` is set. |
| `txMintValue = TxMintValue … MultiAsset` | `mintTxBodyL` | Already plumbed by #5287 via `toLedgerMintValue`; we preserve it. |
| `txAuxScripts = TxAuxScriptsNone` | n/a | Legacy never sets aux scripts in `mkUnsignedTx`; ledger builder need not either. |

Rationale: each row is a direct one-to-one rewrite of a line in
`Cardano.Wallet.Shelley.Transaction.mkUnsignedTx`
(`lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs:805-983`).
Byte equality follows when every row matches.

Alternatives considered:

- *Build a typed `TxBodyContent` mirror and convert.* Adds an
  intermediate type with no value; the parity proof is body-bytes,
  not API shape.
- *Defer reference-input merging to ledger builder caller.* Rejected:
  parity requires deterministic body shape regardless of caller order;
  merging in the builder is the only stable place.

### D2 — Witness-set field

Decision: the extended builder sets
`witsTxL . scriptTxWitsL` on the returned `Write.Tx era` with every
locally-supplied native script that the legacy `TxMintValue` shape can
carry: native input scripts, the optional staking script, and one
local mint script per token policy id after the legacy merge.
Reference-input-only branches do not contribute to `scriptTxWitsL`
(the script bytes are on-chain in the referenced UTxO). The output
reference script does not go through `scriptTxWitsL` either — it
lives in the output structure.

Rationale: the existing wallet code reads scripts back via
`witsTxL . scriptTxWitsL` (see
`lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/WitnessCount.hs:133-173`).
Putting them there at unsigned-build time keeps the signing path
(today: `signTransaction` in `Cardano.Wallet.Shelley.Transaction`,
future: #5289) able to find them without further plumbing.

The legacy `mkUnsignedTx` route uses `Cardano.createTransactionBody`,
which internally extracts the `BuildTxWith` script-witness scripts
and packages them into the `ShelleyTxBody`'s embedded scripts list.
When the wallet converts the legacy body to a ledger `Tx` (via
`Read.Tx`) for byte comparison, those embedded scripts end up at
`scriptTxWitsL`. Parity is therefore equivalent on this field.

Alternatives considered:

- *Return `(Tx era, Set (Script KeyHash))` and have the caller
  install scripts at signing time.* Rejected: changes the return
  shape gratuitously and pushes a contract onto every future caller.
  Setting `scriptTxWitsL` at build time matches what the legacy code
  effectively does.

### D3 — Builder API extension shape

Decision: introduce a small record at module scope:

```haskell
data ScriptWitnesses = ScriptWitnesses
    { swNativeInputs   :: Map TxIn (Script KeyHash)
    , swStakingScript  :: Maybe (Script KeyHash)
    , swMintingSources :: Map AssetId ScriptSource
    , swReferenceScript :: Maybe (Script KeyHash)
    }

noScriptWitnesses :: ScriptWitnesses
```

Both `buildLedgerTx` and `buildLedgerTxRaw` grow one additional
argument of type `ScriptWitnesses`. Existing call sites in `mkTransaction`
and `constructUnsignedTxLedger` pass `noScriptWitnesses`; existing
tests in `TransactionLedgerSpec` pass `noScriptWitnesses`. New
parity tests pass populated values.

Rationale:

- The single-record shape avoids extending two function signatures
  with four scalar arguments and threading them through the test
  call sites.
- `noScriptWitnesses` keeps the slice bisect-safe: every existing
  call site has a one-token edit, and the witness-set/body fields
  default to "no scripts" in that branch, which is exactly the
  previous behavior.
- `Map TxIn (Script KeyHash)` for inputs and `Map AssetId
  ScriptSource` for mint sources match the field names already used
  by `TransactionCtx`; no new wallet-level vocabulary is introduced.

Alternatives considered:

- *Four positional arguments.* Rejected — signature noise and a
  larger diff on test call sites for no benefit.
- *A separate `buildLedgerTxWithScripts` function.* Rejected — two
  builders with overlapping bodies invite divergence; the parity
  contract has to hold in both, and the simpler choice is one
  builder with a defaultable record.

### D4 — Slice shape

Decision: one bisect-safe slice. The slice extends the builder API,
plumbs `ScriptWitnesses` through to the body/witness-set lenses,
adds the six parity tests, and leaves the rest of the module
untouched. Estimated diff size is small enough for a single reviewer
pass — most of the line count is the new test scenarios, not the
builder change.

Rationale:

- Partial builder support (e.g., "native inputs only this slice,
  staking script next slice") would force an intermediate commit
  where the parity proof fails on the not-yet-covered scenarios.
  That violates the bisect-safety invariant — the gate would not
  pass at that intermediate commit unless we also gate the parity
  test on a feature flag, which is overhead with no payoff.
- The witness-set plumbing for one branch is the same code path as
  for the others; doing them together is the minimum coherent unit.

Alternatives considered:

- *Two slices: builder API plumbing first, parity tests second.*
  Rejected — TDD/DDD requires RED before GREEN in the same
  reviewable slice; splitting them across two commits hides the
  failing-state evidence in the merge log.

### D5 — Live-boundary diagnostic answer

Decision: no live-boundary smoke is needed for this PR; `gate.sh`'s
unit pattern is conclusive.

Rationale: the parity proof compares the *CBOR bytes* of two
pure functions (`mkUnsignedTx` and `mkLedgerTx`) over
hand-constructed selections. There is no external system boundary
(no node, no database, no API) that the proof bypasses. The
diagnostic question "what system boundary does this exercise that
the unit suite cannot?" has an empty answer.

If the parent's #5285 ever needs a live-chain proof (e.g., to
confirm an on-chain submission of a script-witnessed transaction
built via the ledger path), that smoke belongs to #5285, not here.

### D6 — Property-test coverage vs. enumerated scenarios

Decision (revised at tasks-phase review): ship **both** the six
enumerated scenarios from `spec.md` AND one QuickCheck property
that combines random selections, withdrawals, certs, mint maps,
and reference scripts within documented generator bounds. Both
are acceptance-gating; the property is named
`prop_buildLedgerTx_matches_mkUnsignedTx_on_script_witnesses`
(see [tasks.md](./tasks.md) T013 and
[briefs/T010-T013.md](./briefs/T010-T013.md) for the generator
bounds).

Rationale: enumerated cases are tractable in code review (every
reader can see exactly what is being compared) and pin specific
acceptance scenarios from `spec.md`. The property mechanically
covers combinations the enumerated cases cannot reach (e.g. an
input both natively script-witnessed AND referenced by an
output-attached reference script). At the parity-with-existing-code
boundary, divergences are typically structural rather than
statistical, so `withMaxSuccess 100` is sufficient; the property
does not need to be a soak test to add value.

Alternatives considered:

- *Property test only.* Rejected — review unfriendly; harder to
  diagnose a failure scenario-by-scenario; loses the line-by-line
  audit trail to `spec.md` US1 ACs.
- *Enumerated only (the previous decision in this PR).* Rejected
  at tasks-phase review: the generator boundary catches
  combinations the six enumerated cases cannot, and parity
  proofs benefit from "explicit wins" over implicit coverage
  trust.
