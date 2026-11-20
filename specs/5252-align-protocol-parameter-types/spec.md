# Spec: Align wallet protocol-parameter size fields with ledger types

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5252

## User story

As a wallet maintainer, I want the wallet's in-memory protocol-parameter
types to agree with cardano-ledger 1.19 so that ledger values cross into the
wallet without obsolete integral-conversion shims, while existing API JSON
and wallet databases remain compatible.

## Current state

Ledger 1.19 exposes the relevant fields as:

- maximum value size: `Word32`;
- collateral percentage: `Word16`;
- maximum collateral input count: `Word16`.

The wallet already represents the maximum collateral input count as
`Word16`, but represents the collateral percentage as `Natural` and wraps
the maximum value size in `TokenBundleMaxSize` over `TxSize`, whose payload
is `Natural`. Recent-era ledger conversions therefore contain integral
conversion shims for all three ledger fields.

The SQLite `ProtocolParameters` table does not persist any of these three
fields. Its stored columns are fee policy, transaction maximum size,
decentralization level, desired pool count, minimum UTxO value, hard-fork
epoch, and key deposit. This ticket therefore does not require a database
migration or a pre-change database fixture; adding either would change a
schema boundary that the requested type alignment does not affect.

The network-parameters API currently emits the three values as JSON numbers.
Its observable JSON representation is a compatibility boundary even where
the Haskell API-facing type remains wider than the aligned primitive type.

## Functional requirements

- FR1: The primitive wallet representation of maximum value size has a
  `Word32` payload, matching the ledger field.
- FR2: `ProtocolParameters.minimumCollateralPercentage` is `Word16`, matching
  the ledger field.
- FR3: `ProtocolParameters.maximumCollateralInputCount` remains `Word16` and
  recent-era ledger conversions transfer all three aligned values without
  obsolete conversion shims at the ledger-to-wallet boundary.
- FR4: The issue's targeted search for `fromIntegral` applied to
  `ppMaxValSizeL`, `ppCollateralPercentageL`, or `ppMaxCollateralInputsL`
  returns no matches under `lib/`.
- FR5: Existing network-parameters API JSON keys, number encoding, and golden
  values remain unchanged.
- FR6: Transaction-size arithmetic remains safe and retains its existing
  semantics. Any necessary widening from the aligned `Word32` maximum into a
  consumer's `Natural`-based size arithmetic is explicit at that downstream
  boundary rather than hidden in the ledger-to-wallet conversion.
- FR7: SQLite schema and migration code remain unchanged because the aligned
  fields are not persisted.

## Observable success criteria

- A compile-time contract accepts the maximum-value-size payload as `Word32`
  and the primitive collateral percentage as `Word16`.
- Recent-era protocol-parameter conversion preserves representative and
  boundary values for maximum value size, collateral percentage, and maximum
  collateral input count without truncation or reinterpretation.
- The targeted `fromIntegral` search returns zero matches.
- The existing `ApiNetworkParameters` JSON golden test passes with the
  golden file unchanged.
- Primitive and wallet unit tests, builds, format checking, and HLint pass.
- No SQLite schema or migration file changes are present in the slice.

## Rejection behavior

Reject an implementation that narrows by truncation, changes public JSON,
changes SQLite schema without newly discovered persistence evidence, weakens
the existing `TxSize` arithmetic domain, or merely hides a ledger-boundary
conversion while leaving the wallet field types unaligned.
