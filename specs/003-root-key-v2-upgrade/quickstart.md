# Quickstart: Root Key Protection Upgrade to V2

**Branch**: `003-root-key-v2-upgrade` | **Date**: 2026-05-06

This document describes the remaining work needed to bring the implementation to
constitution compliance.  The core feature (migration logic, API types, persistence)
is already implemented in commit `7aa70dffe6`.

---

## What is already done

- `PassphraseScheme` extended with `EncryptWithArgon2idV2`; `FromText`/`ToText` instances
- `HashedCredentials` sum type (`V1` / `V2`) in `Credentials.hs`
- `PersistPrivateKey`: V1/V2 serialise/deserialise via length-based distinguisher
- `withRootKey`: opportunistic V1→V2 migration on successful unlock
- `migrateV1toV2`: atomic write of new key + updated metadata scheme
- `ApiWalletPassphraseInfo` extended with `encryptionMethod :: ApiT PassphraseScheme`
- `ToJSON`/`FromJSON` for `ApiT PassphraseScheme`
- `toApiPassphraseInfo` helper wiring all three wallet-type construction sites
- `Arbitrary ApiWalletPassphraseInfo` updated in `TypesSpec`
- Cross-version rejection tests in `PersistPrivateKeySpec.hs` (Gap 2a — complete)
- T006 golden JSON tests for all three `PassphraseScheme` values in `TypesSpec.hs` (Gap 2c — complete)
- T003 V2 wrong-passphrase test in `WalletSpec.hs` (partial Gap 2b coverage)

---

## Remaining work (constitution gaps)

### Gap 0 — Update `cabal.project` pin for `cardano-crypto-wallet` 0.2

When `cardano-crypto-wallet` 0.2 is published to CHaP, update the
`source-repository-package` stanza (lines marked `-- BEGIN cardano-crypto-wallet` /
`-- END cardano-crypto-wallet`) to point at the new release tag and refresh the `sha256`
hash.  The wallet code already uses the 0.2 API names (`ExtKeyMaterial`,
`withDeterministicRandomnessForTesting`, etc.).

### Gap 1 — OpenAPI spec (Principle IV: Formal Specification)

**File**: `specifications/api/swagger.yaml`

Add `encryption_method` to the `walletPassphraseInfo` object.  See the proposed schema
in `contracts/wallet-passphrase-info.yaml`.

Concretely, change lines ~1213–1219 from:

```yaml
x-walletPassphraseInfo :: &walletPassphraseInfo
  description: Information about the wallet's passphrase
  type: object
  required:
    - last_updated_at
  properties:
    last_updated_at: *date
```

to:

```yaml
x-walletPassphraseInfo :: &walletPassphraseInfo
  description: Information about the wallet's passphrase
  type: object
  required:
    - last_updated_at
  properties:
    last_updated_at: *date
    encryption_method:
      description: |
        The scheme used to protect the wallet's root key in storage.
        Absent on wallets that have no stored passphrase (e.g. watch-only wallets).
      type: string
      enum:
        - scrypt
        - pbkdf2-hmac-sha512
        - argon2id-v2
      example: argon2id-v2
```

---

### Gap 2 — Unit tests (Principle VI: Comprehensive Testing)

#### 2a. `PersistPrivateKey` roundtrip — **COMPLETE**

`lib/unit/test/unit/Cardano/Wallet/Address/Keys/PersistPrivateKeySpec.hs` covers:

- V1 Shelley key: `serializeXPrv` → `unsafeDeserializeXPrv` roundtrip
- V1 Byron key (with payload passphrase): roundtrip
- V2 Shelley key: roundtrip
- V2 Byron key (with payload passphrase): roundtrip
- Cross-version rejection: V2 row does not deserialise as V1 and vice-versa

Use `withFastKdfForTesting` (and optionally `withDeterministicRandomnessForTesting` for
golden bytes) to keep V2 roundtrip tests fast.

#### 2b. `withRootKey` migration path (add to existing wallet unit tests)

In `lib/unit/test/unit/Cardano/Wallet/WalletSpec.hs` (or equivalent), test:

- Given a V1 wallet, when `withRootKey` is called with the correct passphrase, then
  the stored credentials are upgraded to V2 and scheme metadata is updated
- Given a V1 wallet, when `withRootKey` is called with a wrong passphrase, then
  credentials remain V1 and the error is propagated
- Given a V2 wallet, `withRootKey` does not re-encrypt (idempotent)

#### 2c. `ApiWalletPassphraseInfo` JSON golden tests — **COMPLETE**

Golden JSON assertions for all three `PassphraseScheme` values are in
`lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs` alongside the existing
`jsonTest`.

---

## Deployment considerations

> [!WARNING]
> **Migration is irreversible.** The first time a wallet is unlocked after this
> version is deployed, its root key is silently re-encrypted under Argon2id and
> the database row is updated in-place. There is no opt-out and no undo.
>
> **Downgrading the binary after migration will lock wallets.**  An older
> `cardano-wallet` binary that reads a `"V2:"` key column will crash the wallet
> worker process for that wallet; the wallet will appear inaccessible until the
> binary is upgraded again.  Funds are not lost — the key material is intact — but
> they cannot be spent until the binary is restored.
>
> **Recommended operator checklist before deploying:**
> 1. Back up all wallet databases (SQLite files in the wallet state directory).
> 2. Confirm you can re-deploy this binary version quickly if rollback is needed.
> 3. After deploying, do not downgrade to any binary that predates this release.

## How to verify locally

```sh
# Build and test the wallet library
nix develop
cabal build lib:cardano-wallet lib:cardano-wallet-api
cabal test lib:unit --test-options '--match "/Api.Types/ApiWalletPassphraseInfo"'

# Validate the swagger spec (once updated)
cabal run validate-swagger -- specifications/api/swagger.yaml
```

---

## Notes for reviewers

- The `encryption_method` field is **not** in the `required` list of
  `walletPassphraseInfo`; existing clients that omit it will parse the response
  correctly (forward-compatibility).
- `migrateV1toV2` currently fails silently; a low-priority follow-up is to add a
  tracer event so operators can observe migration progress in logs.
- Any test that creates a V2 `EncryptedKey` must wrap the creation in
  `withFastKdfForTesting`; the production Argon2id cost (128 MiB / t=3 / p=4) makes
  naive unit tests prohibitively slow.
- Use `withDeterministicRandomnessForTesting` in addition when the exact ciphertext
  bytes must be stable (e.g., golden binary tests).  Both helpers are bracket-scoped
  and can be nested.
