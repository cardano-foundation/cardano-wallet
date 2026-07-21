# Implementation Plan: Root Key Protection Upgrade to V2

**Branch**: `003-root-key-v2-upgrade` | **Date**: 2026-05-06 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/003-root-key-v2-upgrade/spec.md`

## Summary

Upgrade the wallet root-key storage format from the legacy V1 scheme (PBKDF2-HMAC-SHA512
or Scrypt) to a modern V2 authenticated-encryption envelope, with opportunistic silent
migration on each successful wallet unlock.  Expose the active scheme as a stable
machine-readable field (`encryption_method`) on the `GET /v2/wallets/{id}` passphrase
info object so that frontends can detect wallets still using V1 and prompt users
accordingly.

The cryptographic primitive library (`cardano-crypto-wallet-v2`) is already written and
pinned in `cabal.project`.  The wallet business logic, persistence layer, API types, and
OpenAPI contract all need coordinated changes.

## Technical Context

**Language/Version**: Haskell (GHC 9.6), `NoImplicitPrelude`, `-Wall`/`-Werror` in CI
**Primary Dependencies**:
- `cardano-crypto-wallet-v2` — V2 envelope (already pinned via `source-repository-package`)
- `cardano-crypto` — existing `XPrv` / `xprv` / `unXPrv`
- `persistent` + `persistent-sqlite` — SQLite key/hash column storage
- `servant` — REST API routing and type-level contracts
- `aeson` — JSON serialisation of `ApiWalletPassphraseInfo`

**Storage**: SQLite; root key stored in `private_key` table as two `TEXT` columns (`key`,
`hash`).  V1 and V2 are distinguished by the byte-length of the first colon-delimited
segment of the `key` column (exactly 256 hex chars = V1 XPrv; longer = V2 CBOR
envelope).  Passphrase scheme tracked separately in wallet metadata (`passphrase_scheme`
column).  **No schema migration required.**

**Testing**: `hspec` + `QuickCheck` (unit); local cardano-node cluster (integration);
preprod (E2E).

**Target Platform**: Linux x86_64 / aarch64 (musl static), Windows (cross-compiled),
macOS x86_64 + arm64.

**Project Type**: Library + REST web-service (Servant).

**Performance Goals**: V1→V2 migration adds < 50 ms to a single wallet-unlock operation
(dominated by Argon2id KDF, acceptable for a one-time per-wallet cost).

**Constraints**: 70-character Fourmolu line limit; leading commas; 4-space indent; HLint
must pass; `-Werror` in release; no breaking changes to the public API contract.

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-checked after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Maintenance-First Stability | ✓ PASS | Opportunistic migration; no forced re-keying; fully backward-compatible DB; old clients can still read V1 rows |
| II. Era-Aware Design | ✓ PASS | `KeyFlavorS` GADT dispatches over Byron / Icarus / Shelley / Shared uniformly; address-derivation payload preserved for Byron |
| III. Type Safety as Security | ✓ PASS | `HashedCredentials` sum type makes V1/V2 statically distinct; `PassphraseScheme` in metadata prevents silent downgrade |
| IV. Formal Specification | ⚠ GAP | `specifications/api/swagger.yaml` `walletPassphraseInfo` object is missing the `encryption_method` property — **must be added before merge** |
| V. Reproducible Builds | ✓ PASS | `cardano-crypto-wallet-v2` pinned via `source-repository-package` SHA256 in `cabal.project` |
| VI. Comprehensive Testing | ⚠ GAP | Store-law unit tests exist; missing: (a) roundtrip test for V1/V2 serialisation in `PersistPrivateKey`, (b) unit test for `withRootKey` migration path, (c) unit test for `migrateV1toV2` atomicity, (d) `ApiWalletPassphraseInfo` golden JSON test for each scheme |
| VII. Code Quality Gates | ✓ PASS | Compiles with `-Wall`; Fourmolu formatting applied; HLint passing |

**Gate result**: Two constitution gaps must be closed before merge:
1. OpenAPI spec updated with `encryption_method`
2. Missing unit tests written

## Project Structure

### Documentation (this feature)

```text
specs/003-root-key-v2-upgrade/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output
│   └── wallet-passphrase-info.yaml
└── checklists/
    └── requirements.md
```

### Source Code (repository root)

```text
cabal.project                                       # source-repository-package pin (done)

lib/secrets/src/Cardano/Wallet/Primitive/
└── Passphrase/Types.hs                             # PassphraseScheme + FromText (done)

lib/wallet/src/Cardano/Wallet/
├── Primitive/Types/Credentials.hs                  # HashedCredentials sum type (done)
├── Address/Keys/PersistPrivateKey.hs               # V1/V2 serialisation (done)
└── Wallet.hs                                       # withRootKey + migrateV1toV2 (done)

lib/api/src/Cardano/Wallet/Api/
├── Types.hs                                        # ApiWalletPassphraseInfo + JSON (done)
└── Http/Shelley/Server.hs                          # toApiPassphraseInfo helper (done)

specifications/api/swagger.yaml                     # walletPassphraseInfo schema (MISSING)

lib/unit/test/unit/Cardano/Wallet/
├── Api/TypesSpec.hs                                # Arbitrary instance (done)
├── DB/Store/PrivateKey/StoreSpec.hs                # store-law tests (done, needs V2 coverage)
└── Address/Keys/PersistPrivateKeySpec.hs           # V1/V2 roundtrip tests (MISSING)
```

## Complexity Tracking

No constitution violations requiring justification. Both gaps (OpenAPI spec, test
coverage) are straightforward additions, not architectural complexity.

---

## Phase 0: Research

> All unknowns resolved. No external research agents needed — implementation is complete
> and the codebase is the source of truth. See `research.md` for findings.

---

## Phase 1: Design & Contracts

> See `data-model.md` for entity definitions and `contracts/wallet-passphrase-info.yaml`
> for the API contract change.
