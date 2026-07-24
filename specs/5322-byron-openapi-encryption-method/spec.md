# Spec — #5322: OpenAPI overclaims argon2id-v2 on Byron/Icarus passphrase responses

## P1 user story

As an API consumer, I read `passphrase.encryption_method` on wallet
responses (including Byron/Icarus) and only see scheme values that
wallet type can actually produce at runtime.

## Background

#5294 added the response field `passphrase.encryption_method`
(`scrypt` | `pbkdf2-hmac-sha512` | `argon2id-v2`) on the shared OpenAPI
anchor `walletPassphraseInfo` (`specifications/api/swagger.yaml:1213-1229`),
reused verbatim by three response schemas:

- `ApiWallet` (Shelley, line 3155)
- `ApiByronWallet` (line 3175)
- `ApiActiveSharedWallet` (line 3233)

Runtime is flavor-split (confirmed by reading
`Cardano.Wallet.attachPrivateKeyFromPwd` and `Cardano.Wallet.withRootKey`,
`lib/wallet/src/Cardano/Wallet.hs:4265-4453`):

- **Shelley / Shared** (non-Byron, non-Icarus): new wallets and V1→V2
  migration use `EncryptWithArgon2idV2` — all three enum values are
  legitimately reachable.
- **Byron random / Icarus**: `attachPrivateKeyFromPwd` always takes the
  `legacyV1` branch (`EncryptWithPBKDF2`, or `EncryptWithScrypt` via
  `attachPrivateKeyFromPwdHashByron` for legacy-imported keys);
  `withRootKey`'s migration branch is a literal no-op for
  `ByronKeyS`/`IcarusKeyS` ("V2 signing is not yet implemented for
  these legacy wallet types"). Byron/Icarus responses can therefore
  only ever report `scrypt` or `pbkdf2-hmac-sha512` — `argon2id-v2` is
  structurally unreachable, but the OpenAPI contract currently
  documents it as valid.

The response-mapping function `toApiPassphraseInfo`
(`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:1168-1170`) is a
pure passthrough of whatever scheme is in stored metadata — it does not
discriminate by flavor and needs no change; the fix is confined to (a)
the OpenAPI contract, (b) the test-generation code that must agree with
the narrowed contract, and (c) new coverage proving the runtime
guarantee, per the issue's own Non-goals (full V2 signing for
Byron/Icarus is explicitly out of scope unless product says otherwise).

## User stories

1. As an API consumer, the Byron wallet response schema in
   `specifications/api/swagger.yaml` only lists `scrypt` and
   `pbkdf2-hmac-sha512` as possible `encryption_method` values — not
   `argon2id-v2`.
2. As a developer, I have unit-level proof that Byron/Icarus wallet
   creation and unlock never persists `EncryptWithArgon2idV2`, and that
   Shelley wallet creation does (existing coverage:
   `Cardano.WalletSpec.walletRootKeyV2Idempotent`).
3. As an API consumer, an incomplete shared wallet (no root key yet)
   response omits the `passphrase` field entirely (already true
   structurally in `ApiIncompleteSharedWallet`; needs regression
   coverage).
4. As a release engineer, `scripts/release/openapi-diff.sh` diffs
   against the last **stable** release tag, never a `nightly`
   prerelease, so "No changes" is a trustworthy signal.

## Functional requirements

- FR1: `specifications/api/swagger.yaml` gives `ApiByronWallet.passphrase`
  a narrower `encryption_method` enum (`scrypt`, `pbkdf2-hmac-sha512`)
  via a new anchor, without touching the existing `walletPassphraseInfo`
  anchor used by `ApiWallet` / `ApiActiveSharedWallet` (both can
  legitimately report `argon2id-v2`).
- FR2: The `Arbitrary ApiByronWallet` instance used by the
  schema-conformance property test
  (`validateEveryToJSONWithPatternChecker (Proxy @(Api T0))` in
  `TypesSpec.hs`) never generates `argon2id-v2` for the `passphrase`
  field, so generated samples stay conformant with FR1's narrowed
  schema.
- FR3: New unit coverage proves Byron/Icarus `attachPrivateKeyFromPwd`
  and `withRootKey` never yield `EncryptWithArgon2idV2` /
  `HashedCredentialsV2`.
- FR4: A regression test proves an incomplete shared wallet's JSON
  response has no `passphrase` key.
- FR5: `scripts/release/openapi-diff.sh` filters the GitHub Releases
  API response to non-prerelease entries before picking the baseline
  tag.

## Non-goals (carried from the issue)

- Implementing full Argon2id V2 signing for Byron/Icarus.
- Changing request APIs to accept a client-chosen encryption scheme.
- Any change to `attachPrivateKeyFromPwd`, `withRootKey`, or
  `toApiPassphraseInfo` production logic — all three already produce
  the right runtime values; only docs/tests change.

## Success criteria

- [ ] `ApiByronWallet.passphrase.encryption_method` in swagger.yaml
      cannot express `argon2id-v2`.
- [ ] `validateEveryToJSONWithPatternChecker` and
      `validateEveryToJSON`-covered suites stay green with the
      narrowed schema (no probabilistic failure).
- [ ] A unit test fails if Byron/Icarus `attachPrivateKeyFromPwd` or
      `withRootKey` ever persisted `EncryptWithArgon2idV2`.
- [ ] A unit test fails if an incomplete shared wallet's JSON response
      ever gains a `passphrase` key.
- [ ] `scripts/release/openapi-diff.sh` picks a non-prerelease tag even
      when a `nightly` release is more recently published.
- [ ] `./gate.sh` green at HEAD.
