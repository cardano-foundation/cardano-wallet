# Plan: Align protocol-parameter size fields with ledger types

One bisect-safe PAIR slice. The final history contains one behavior commit;
the commit owner may use multiple local RED/GREEN proof rounds before the
ticket owner accepts and the commit owner squashes them.

## Architecture and boundaries

- Align `TokenBundleMaxSize` with the ledger maximum-value-size domain by
  giving it a `Word32` payload. Preserve `TxSize`'s existing `Natural` payload
  for additive transaction-size arithmetic.
- Align primitive `minimumCollateralPercentage` to `Word16`;
  `maximumCollateralInputCount` is already correctly typed.
- Transfer the three fields directly in the recent-era ledger-to-wallet
  protocol-parameter conversions.
- Keep any necessary widening localized to downstream consumers such as API
  projection or transaction-size arithmetic.
- Preserve the observable `ApiNetworkParameters` JSON contract and its
  existing golden file.
- Do not touch SQLite schema or migrations: the current schema does not store
  any of the aligned fields.

## Behavioral invariants

1. Ledger-to-wallet conversion preserves each aligned field exactly across
   every supported recent era that exposes it.
2. The maximum value size has the ledger's full `Word32` domain in the wallet;
   collateral percentage and maximum collateral input count have the ledger's
   `Word16` domain.
3. Downstream widening cannot truncate, wrap, or change transaction-size
   comparisons.
4. API encoding remains the same numeric JSON contract, including the checked
   golden artifact.
5. Persistence compatibility is preserved by leaving the schema and migration
   surface untouched.
6. The targeted obsolete ledger-field conversions are absent, and the proof
   is reached by the ticket gate rather than existing only as unexecuted test
   code.

## Slice A — align the primitive domain and preserve boundaries

The commit owner owns proof files and chooses the invariant/test order. Its
coder owns production changes within the maximum fence identified in the
PAIR brief. The ticket owner will accept only an alternating RED/GREEN chain
whose coverage ledger accounts for all invariants above.

Expected production surface:

- primitive protocol-parameter and maximum-value-size types;
- primitive ledger conversions and Byron default construction;
- downstream API projection and transaction constraints where widening is
  genuinely required;
- generators or non-proof support code that must follow the aligned types.

Expected proof surface:

- primitive conversion/type tests selected by the commit owner;
- existing API network-parameter golden execution;
- the frozen compile-time type contract, zero-conversion search, no-schema
  diff, relevant package builds/tests, format check, and HLint.

## Verification

The frozen gate will run, from their CI working directories:

- the ticket-owned compile-time type contract;
- focused commit-owner proofs with `--fail-on=empty`;
- the primitive test suite;
- the API types/golden test selection;
- relevant Nix builds;
- format checking and HLint;
- the issue's targeted zero-match search;
- immutable comparisons proving the API golden and SQLite schema/migration
  files are unchanged from the slice base.
