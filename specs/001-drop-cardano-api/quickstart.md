# Quickstart: Remove cardano-api Dependency

## Prerequisites

- Nix with flakes enabled
- Access to `cardano-foundation/cardano-balance-transaction` (public repo)
- Local clone of `cardano-wallet` on branch `001-drop-cardano-api`

## Development Environment

```bash
cd /path/to/cardano-wallet
nix develop
```

## Build & Test

```bash
# Build all packages
just build

# Run unit tests
just test

# Run integration tests (requires local cluster)
just integration

# Check for cardano-api in build plan
cabal build all --dry-run 2>&1 | grep cardano-api

# Verify no Cardano.Api imports remain
grep -r "import.*Cardano\.Api" lib/ --include="*.hs" -l
```

## Key Files to Understand

| File | Role |
|------|------|
| `cabal.project` (line 141) | `cardano-balance-transaction` pin |
| `lib/primitive/lib/.../SealedTx.hs` | Core transaction type (rewrite target) |
| `lib/primitive/lib/.../Tx/Tx.hs` | TxMetadata re-export (replace with owned type) |
| `lib/primitive/lib/.../NetworkId.hs` | NetworkId with cardano-api bridge |
| `lib/cardano-wallet-read/.../Eras.hs` | Era GADT system (keep, remove bridge) |
| `lib/wallet/src/.../Transaction.hs` | Transaction construction (heaviest cardano-api usage) |
| `lib/api/src/.../SchemaMetadata.hs` | REST API metadata serialization |
| `lib/cardano-api-extra/` | Test generators (delete at end) |

## Phase Verification

After each phase, verify:
1. `just ci` passes (lint + build + test)
2. No new `Cardano.Api` imports introduced
3. Integration tests pass on local cluster
