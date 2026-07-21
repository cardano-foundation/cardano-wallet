# Research: Root Key Protection Upgrade to V2

**Branch**: `003-root-key-v2-upgrade` | **Date**: 2026-05-06

All decisions were resolved by reading the existing implementation and the project
constitution.  No external research was required.

---

## Decision 1: Migration trigger point

**Decision**: Migrate V1 → V2 opportunistically inside `withRootKey`, immediately after
a successful passphrase verification, before invoking the caller's action callback.

**Rationale**: `withRootKey` is the single choke-point through which every passphrase-
authenticated operation flows (signing, address derivation, passphrase change).
Hooking migration here guarantees the upgrade happens on the first real use without
requiring callers to be aware of it.

**Alternatives considered**:
- Explicit migration endpoint (`POST /v2/wallets/{id}/migrate-key`): rejected — requires
  user action, defeats the zero-friction goal.
- Background migration task at wallet open: rejected — introduces concurrency complexity
  and the wallet may never be opened by the owner on that node.

---

## Decision 2: Atomicity of the migration write

**Decision**: The migration writes both the new key record and the updated
`passphrase_scheme` metadata in a single `atomically` block via the existing
`DBLayer` STM interface.

**Rationale**: SQLite transactions guarantee that either both writes land or neither
does.  The `atomically` wrapper in `DBLayer` composes the two `stm` actions into one
transaction.  A crash mid-migration therefore leaves the DB in a consistent V1 state.

**Alternatives considered**:
- Two separate transactions: rejected — window between writes allows inconsistent state
  (key=V2, scheme=V1 or vice-versa).

---

## Decision 3: Silent failure on migration error

**Decision**: `migrateV1toV2` returns `IO ()` and swallows errors from the V2 encryption
call.  The key stays V1 and migration is retried on the next unlock.

**Rationale**: The primary operation (signing, etc.) must not be blocked by a migration
failure.  V2 encryption errors are highly unlikely once the library is correct (they
indicate a malformed key, which would have already failed V1 verification).

**Gap identified**: There is currently no log/trace event on migration failure or
success.  The constitution's maintenance-first principle suggests this should be
observable.  A `tracer` call should be added (low-priority, does not block merge).

---

## Decision 4: V1/V2 serialisation distinguisher

**Decision**: V1 vs V2 is identified by the byte-length of the first colon-delimited
segment of the `key` DB column: exactly 256 hex chars (128 bytes) = V1 XPrv; anything
longer = V2 CBOR envelope.

**Rationale**: Reuses the existing two-column schema (`key`, `hash`) without any schema
migration.  The V2 CBOR envelope is self-describing and always longer than 256 hex chars.

**Alternatives considered**:
- New DB column `key_version`: rejected — requires a schema migration on existing
  production databases, violating maintenance-first stability.
- Sentinel prefix byte: rejected — more fragile than length-based detection and requires
  the CBOR decoder to skip it.

---

## Decision 5: In-memory representation after V2 decrypt

**Decision**: After decrypting a V2 key, `withRootKey` reconstructs a standard `k 'RootK
XPrv` with a **plaintext scalar** and passes it to the action callback with
`EncryptWithArgon2idV2` as the scheme.  The action then calls
`preparePassphrase EncryptWithArgon2idV2 _  = Passphrase mempty`, which causes the C
signing layer to run in no-op (memcpy) mode.

**Rationale**: Existing callers of `withRootKey` already use `preparePassphrase` with the
scheme argument to prepare the passphrase before passing it to the C layer.  Returning
`EncryptWithArgon2idV2` from the callback is sufficient to make all callers correct
without any changes to downstream code.

---

## Decision 6: API field name and values

**Decision**: Field name `encryption_method`; values `"scrypt"`, `"pbkdf2-hmac-sha512"`,
`"argon2id-v2"`.  These match the `ToText`/`FromText` instances on `PassphraseScheme`
and are serialised via the existing `ApiT` + `toTextApiT`/`fromTextApiT` pattern.

**Rationale**: Consistent with the existing `ToText PassphraseScheme` implementation
already used for DB storage.  The `ApiT` wrapper provides automatic JSON
round-trip via `ToText`/`FromText` without bespoke encoding.

---

## Open items (non-blocking)

| Item | Priority | Owner |
|------|----------|-------|
| Add tracer events to `migrateV1toV2` (success + failure) | Low | wallet team |
| Integration test: V1 wallet → sign tx → verify `encryption_method` = v2 | Medium | wallet team |
| E2E test on preprod with imported Daedalus scrypt wallet | Low | QA team |
