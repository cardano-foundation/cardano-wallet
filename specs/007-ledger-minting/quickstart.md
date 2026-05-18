# Quickstart: Reviewing or Resuming `007-ledger-minting`

## What you're looking at

A small, structural plumbing change in the wallet's ledger-based transaction body builder. The change unblocks the migration of `mkUnsignedTransaction` off `cardano-api` (Story 1 of issue [#5285](https://github.com/cardano-foundation/cardano-wallet/issues/5285)) by removing two `mempty -- TODO: minting support` placeholders from `Cardano.Wallet.Shelley.Transaction.Ledger`. Script-witness support is **not** in this slice.

Read these in order if you are reviewing or resuming:

1. `spec.md` — what and why, in spec-kit framing.
2. `research.md` — Phase 0 findings: exact call sites, line numbers, signatures, the `TokenMap` ↔ `MultiAsset` asymmetry.
3. `data-model.md` — the new translation function, its inputs, and the mint-burn netting rule.
4. `contracts/ledger-mint-translation.md` — invariants as property statements.
5. `plan.md` — the design decisions distilled (D1–D6); read this last to confirm the chosen approach.

## Files the implementation will touch

| File | Change |
|---|---|
| `lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Convert.hs` | Add `toLedgerMintValue :: TokenMap -> TokenMap -> MultiAsset`. |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` | `buildLedgerTx` and `buildLedgerTxRaw` take an explicit `MultiAsset` parameter. The two `mempty -- TODO` sites at lines 433 and 496 vanish. `mkTransaction` and `constructUnsignedTxLedger` translate and pass through. |
| `lib/wallet/src/Cardano/Wallet.hs` | One call site (around line 2746) updated to pass `(TokenMap, TokenMap)` mint/burn through to `constructUnsignedTxLedger`. |
| `lib/primitive/test/.../Ledger/ConvertSpec.hs` (or equivalent) | Properties P1–P8 from the contract. |
| `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` | Properties B1–B3 from the contract. |

No `cabal.project` changes; no new dependencies; no new packages.

## How to build and test the slice

From the worktree root (`/code/cardano-wallet-5243`), inside `nix develop` (or with direnv loaded):

```bash
# Build only the affected libraries.
cabal build cardano-wallet:lib:wallet primitive:lib:primitive -O0 -v0

# Run the convert and builder spec modules.
cabal test cardano-wallet-unit:unit primitive:test:unit-tests -O0 -v0 \
  --test-show-details=streaming
```

For the fast inner loop, narrow further with hspec's `--match`:

```bash
cabal run cardano-wallet-unit:unit -- --match "TransactionLedger/mint"
cabal run primitive:test:unit-tests -- --match "Convert/toLedgerMintValue"
```

Format check before commit:

```bash
just check-fmt
```

## Expected commit shape

Three vertical, bisect-safe commits, in this order:

1. **`feat(primitive): toLedgerMintValue (TokenMap, TokenMap) → MultiAsset`** — adds the conversion **and** its property tests in the same commit (RED + GREEN folded, per the PR skill's rule).
2. **`feat(wallet): plumb mint through buildLedgerTx / buildLedgerTxRaw`** — adds the explicit `MultiAsset` parameter to both builders, removes both `mempty -- TODO` sites, wires `mkTransaction` and `constructUnsignedTxLedger`, and adds the builder-side property tests B1–B3. Same commit holds both the test and the implementation, again to keep the slice bisect-safe.
3. **`feat(wallet): pass mint/burn from Cardano.Wallet to constructUnsignedTxLedger`** — updates the single call site at `Cardano.Wallet.hs:~2746`.

If a fourth commit appears for "documentation" or "review fixes", reroute the change into the commit that introduced the issue (per the PR skill).

## How to verify minting is plumbed (smoke check)

After the slice lands, the following must hold:

```bash
# No remaining placeholders.
rg 'mempty -- TODO: minting support' lib/wallet/src
# (expect: no matches)

# The new converter exists.
rg '^toLedgerMintValue' lib/primitive
# (expect: a definition in Convert.hs)

# Both builders mention the new parameter.
rg 'buildLedgerTx(Raw)?\b' lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs
# (expect: a `MultiAsset` somewhere in their argument lists)
```

The migration of `mkUnsignedTransaction` itself is a separate feature; it is **not** delivered by this slice and **not** verified by these checks.

## Open questions deferred to next phase

- The exact preferred spec module under `lib/primitive/test/` for the converter properties — to be confirmed by the first task ("locate spec module for `Convert.hs`"). No conversion implementation lives there yet, but the surrounding pattern in that package should drive the choice.
- Whether `Arbitrary TokenMap` is already exported from the test-only support library or needs a brief re-export — confirm on the first run of `cabal test`.
