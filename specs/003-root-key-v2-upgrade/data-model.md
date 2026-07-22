# Data Model: Root Key Protection Upgrade to V2

**Branch**: `003-root-key-v2-upgrade` | **Date**: 2026-05-06

---

## Entities

### HashedCredentials

The in-memory and on-disk representation of a stored wallet root key.  A sum type with
two constructors.

| Constructor | Fields | When used |
|-------------|--------|-----------|
| `HashedCredentialsV1` | `k 'RootK XPrv` (scheme-encrypted key), `PassphraseHash` (PBKDF2/Scrypt hash) | Legacy wallets not yet migrated |
| `HashedCredentialsV2` | `EncryptedKey` (AEAD envelope), `Maybe (Passphrase "addr-derivation-payload")` | All new wallets; migrated wallets |

**Invariants**:
- A V1 key's `PassphraseHash` is produced by the scheme recorded in wallet metadata
  (`passphraseScheme`).
- A V2 key's `EncryptedKey` is self-authenticating; the `PassphraseHash` column in the
  DB is left empty.
- The Byron payload passphrase is `Nothing` for all non-Byron key flavors.

**State transition**:
```
HashedCredentialsV1 ──(successful unlock)──► HashedCredentialsV2
```
The transition is one-way and written atomically alongside the metadata update.

> [!WARNING]
> **Irreversible.** Once a wallet row is written as V2, any binary that does not
> understand the `"V2:"` prefix will either crash or permanently lock that wallet.
> Back up the database before deploying this version if rollback may be required.

---

### WalletPassphraseInfo

Wallet metadata sub-record.  Extended with a new field.

| Field | Type | Notes |
|-------|------|-------|
| `lastUpdatedAt` | `UTCTime` | Timestamp of last passphrase change |
| `passphraseScheme` | `PassphraseScheme` | **Extended**: now also `EncryptWithArgon2idV2` |

---

### PassphraseScheme

Enumeration of supported key-protection schemes.

| Value | DB text | API JSON | Notes |
|-------|---------|----------|-------|
| `EncryptWithScrypt` | `"scrypt"` | `"scrypt"` | Legacy; imported Byron wallets only |
| `EncryptWithPBKDF2` | `"pbkdf2-hmac-sha512"` | `"pbkdf2-hmac-sha512"` | All wallets created before this feature |
| `EncryptWithArgon2idV2` | `"argon2id-v2"` | `"argon2id-v2"` | New default; all migrated wallets |

---

### ApiWalletPassphraseInfo (API layer)

JSON representation of `WalletPassphraseInfo` returned by `GET /v2/wallets/{id}`.

| Field | JSON key | Type | Notes |
|-------|----------|------|-------|
| `lastUpdatedAt` | `last_updated_at` | ISO-8601 datetime | Unchanged |
| `encryptionMethod` | `encryption_method` | string enum | **New field** |

The `encryption_method` field is optional in the response schema to preserve
backward-compatibility: clients that do not understand it can ignore it, and wallets
without a passphrase omit the entire `passphrase` object as before.

---

## DB Column Encoding

```
private_key table
┌──────────────────────────────────────────────────────────────────┬──────────────────┐
│ key (TEXT)                                                       │ hash (TEXT)      │
├──────────────────────────────────────────────────────────────────┼──────────────────┤
│ V1 non-Byron: hex(unXPrv k)              [exactly 256 hex chars] │ hex(hpwd)        │
│ V1 Byron:     hex(unXPrv k):hex(pp)      [first seg = 256 chars] │ hex(hpwd)        │
│ V2 non-Byron: "V2:" hex(unEncryptedKey ek)                       │ (empty)          │
│ V2 Byron:     "V2:" hex(unEncryptedKey ek) ":" hex(pp)           │ (empty)          │
└──────────────────────────────────────────────────────────────────┴──────────────────┘

Distinguisher: presence of the literal prefix `"V2:"` in the `key` column.
  key starts with "V2:"  →  V2
  otherwise              →  V1

Within V1, Byron keys are distinguished from non-Byron keys by the presence of a
single `":"` separator after the first 256-character hex segment.

Within V2, Byron keys are distinguished from non-Byron keys by the presence of an
additional `":"` separator after the `EncryptedKey` hex.

Note: the length of the first `":"` -delimited segment is also a valid V1/V2
distinguisher (== 256 → V1; 2 chars "V2" → V2), but the explicit prefix is
canonical and is what the implementation checks first.
```

### Legacy "old V2" format (migration compatibility only)

During the transition period when V2 was first introduced, some wallets were written
with an additional PBKDF2-XPrv segment that has since been removed for security.
The deserialization layer accepts but silently discards this segment:

```
Old V2 non-Byron: "V2:" hex(unXPrv k) [256 chars] ":" hex(unEncryptedKey ek)
Old V2 Byron:     "V2:" hex(unXPrv k) [256 chars] ":" hex(unEncryptedKey ek) ":" hex(pp)
```

The stale XPrv bytes are discarded on read; the key is re-serialized in the new
format on the next write (e.g., migration or passphrase change). New software never
writes this format.

---

## Validation Rules

- `encryption_method` in API requests is not accepted (read-only; derived from stored
  scheme, not user-supplied).
- `PassphraseScheme` `FromText` must reject any string not in the defined set.
- V2 migration MUST NOT occur if passphrase verification fails (enforced by the control
  flow: migration is only called after `Right _` from the verify step).

---

## V2 Envelope Binary Format (`EncryptedKey` internals)

`unEncryptedKey` (the bytes stored after the `"V2:"` DB prefix) is a CBOR-encoded
5-element list:

```
[Salt, Nonce, AAD, EncSecretKey, Tag]
```

| Field | CBOR type | Size | Notes |
|-------|-----------|------|-------|
| `Salt` | bytes | 32 | Argon2id salt; random per encryption |
| `Nonce` | bytes | 24 | XChaCha20-Poly1305 nonce; random per encryption |
| `AAD` | bytes | variable | Nested CBOR (see below); bound as AEAD additional data |
| `EncSecretKey` | bytes | 64 | XChaCha20-Poly1305 ciphertext of the 64-byte extended secret key |
| `Tag` | bytes | 16 | XChaCha20-Poly1305 authentication tag |

The `AAD` is itself a CBOR-encoded 8-element list that **permanently records the
envelope metadata**:

```
[version, kdfId, [memoryKiB, timeCost, parallelism, outputLen], cipherId, payloadKind, payloadLen, pubKey, chainCode]
```

| AAD field | Value | Notes |
|-----------|-------|-------|
| `version` | `2` | Envelope version |
| `kdfId` | `1` (= Argon2id) | |
| `memoryKiB` | `131072` | 128 MiB |
| `timeCost` | `3` | |
| `parallelism` | `4` | |
| `outputLen` | `32` | Wrapping key size in bytes |
| `cipherId` | `1` (= XChaCha20-Poly1305) | |
| `payloadKind` | `1` | Indicates secret-key payload |
| `payloadLen` | `64` | |
| `pubKey` | bytes | 32-byte Ed25519 public key |
| `chainCode` | bytes | 32-byte BIP-32 chain code |

**Implications**:

- The public key and chain code are **authenticated but not encrypted**: they can be
  read from the raw bytes without the passphrase.  `encryptedPublic` and
  `encryptedChainCode` exploit this.

- The Argon2id production parameters (`128 MiB / t=3 / p=4`) are **fixed in the
  AAD** and validated on every decode.  An envelope produced with different parameters
  (e.g., the fast-test parameters) cannot be decoded by production code, and production
  envelopes cannot be "upgraded" to higher parameters without full re-encryption.

- `withDecryptedExtKeyMaterial` **only operates on `EnvelopeV2` keys**.  When called
  on a `LegacyV1` `EncryptedKey` (128-byte raw XPrv bytes), it returns
  `Left XPrvDecodeError` immediately.  V1 keys are decrypted via the separate
  `cardano-crypto` XPrv pathway; this function is V2-only.

### Library-level format discriminator

The library distinguishes V1 from V2 by **byte-length** of the raw `EncryptedKey`
bytes (separate from the DB column `"V2:"` prefix, which is stripped before the bytes
reach the library):

```haskell
data XPrvFormat = LegacyV1 | EnvelopeV2

encryptedKeyFormat :: EncryptedKey -> XPrvFormat
-- LegacyV1 iff byte-length == 128  (64 secret + 32 pub + 32 cc, unencrypted)
-- EnvelopeV2 otherwise             (CBOR envelope, always longer than 128 bytes)
```

---

## Library types and error handling (`cardano-crypto-wallet` 0.2)

### `XPrvError`

All fallible library operations return `Either XPrvError a`.  Relevant variants:

| Variant | When raised |
|---------|-------------|
| `XPrvDecodeError` | CBOR decode failure; also returned by `withDecryptedExtKeyMaterial` for `LegacyV1` keys |
| `XPrvUnsupportedVersion` | AAD `version ≠ 2` |
| `XPrvUnsupportedKdf` | AAD `kdfId ≠ 1` |
| `XPrvUnsupportedCipher` | AAD `cipherId ≠ 1` |
| `XPrvInvalidKdfParams` | AAD KDF params do not match the fixed production values |
| `XPrvAuthenticationFailed` | XChaCha20-Poly1305 tag check failed (wrong passphrase or tampered bytes) |
| `XPrvPublicKeyMismatch` | Decrypted secret key does not match the authenticated public key in AAD |
| `XPrvHardenedDerivationUnsupported` | `encryptedDerivePublic` called with a hardened index (≥ 0x80000000) |
| `XPrvInternalError` | C-level operation failed unexpectedly |

`XPrvAuthenticationFailed` is the canonical wrong-passphrase signal.
`XPrvPublicKeyMismatch` indicates a corrupt or tampered envelope.

### Test helpers

Two bracket-scoped overrides are available for tests:

```haskell
-- Reduce Argon2id cost (4 MiB / t=1 / p=1) for fast test execution.
-- Envelopes created inside this bracket are NOT decodable outside it.
withFastKdfForTesting :: IO a -> IO a

-- Replace system RNG with a deterministic counter so test output is reproducible.
withDeterministicRandomnessForTesting :: IO a -> IO a
```

These two helpers are independent and can be nested.  `withFastKdfForTesting` is
required for any test that creates a V2 key and then immediately decrypts it in the
same test run (otherwise the 128 MiB Argon2id cost makes the test suite impractically
slow).  `withDeterministicRandomnessForTesting` is useful for golden tests where the
exact ciphertext bytes must be stable across runs.
