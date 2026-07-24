# Plan — #5322

## Tech stack

Haskell (cardano-wallet monorepo), nix + `just`/`cabal`, hspec +
QuickCheck (`genericArbitrary`, `Test.Aeson.GenericSpecs` golden specs),
YAML (OpenAPI 2.0-style spec with custom `x-` anchors), bash (release
scripts).

## Slice breakdown

Four independent, bisect-safe slices. No ordering dependency between
them except 1 and 3 both touch `TypesSpec.hs` (different describe
blocks; sequencing avoids merge noise, not a hard conflict).

### Slice 1 — Narrow the Byron OpenAPI passphrase schema + keep the schema-conformance property test green

**Files:**
- `specifications/api/swagger.yaml`
- `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs`
- `lib/unit/test/data/Cardano/Wallet/Api/ApiByronWallet.json`

**Change:**
1. Add a new anchor next to `walletPassphraseInfo`
   (`specifications/api/swagger.yaml:1213-1229`), e.g.:
   ```yaml
   x-walletPassphraseInfoLegacy :: &walletPassphraseInfoLegacy
     description: Information about the wallet's passphrase
     type: object
     required:
       - last_updated_at
     properties:
       last_updated_at: *date
       encryption_method:
         description: |
           The scheme used to protect the wallet's root key in storage.
           Absent on wallets that have no stored passphrase.
         type: string
         enum:
           - scrypt
           - pbkdf2-hmac-sha512
         example: pbkdf2-hmac-sha512
   ```
   Point `ApiByronWallet.passphrase` (currently line 3175,
   `passphrase: *walletPassphraseInfo`) at
   `*walletPassphraseInfoLegacy` instead. Leave `ApiWallet` (line 3155)
   and `ApiActiveSharedWallet` (line 3233) on the original
   `walletPassphraseInfo` anchor unchanged — both legitimately reach
   `argon2id-v2`.
2. `TypesSpec.hs` currently has (~line 1606):
   ```haskell
   instance Arbitrary ApiByronWallet where
       arbitrary = genericArbitrary
       shrink = genericShrink
   ```
   `genericArbitrary` recurses into the shared, flavor-agnostic
   `Arbitrary ApiWalletPassphraseInfo` (~line 1918:
   `ApiWalletPassphraseInfo <$> genUniformTime <*> arbitrary`), which
   can produce any of the 3 `PassphraseScheme` constructors including
   `EncryptWithArgon2idV2`. After step 1, `validateEveryToJSONWithPatternChecker
   (Proxy @(Api T0))` (~line 956) will eventually generate an
   `ApiByronWallet` sample whose `passphrase.encryption_method` is
   `argon2id-v2` and fail schema validation against the now-narrower
   spec — **this is the RED**, triggered purely by step 1, no new test
   needed. Fix it by replacing the generic instance with one that
   builds `passphrase` from a Byron-scoped generator restricted to
   `EncryptWithScrypt` / `EncryptWithPBKDF2` (e.g. reuse
   `genericArbitrary` for every field except override `passphrase` with
   `traverse (\_ -> ApiWalletPassphraseInfo <$> genUniformTime <*>
   (ApiT <$> elements [EncryptWithScrypt, EncryptWithPBKDF2])) =<<
   arbitrary` or equivalent — driver's call on the cleanest
   construction).
3. `ApiByronWallet.json` golden fixture has a literal sample with
   `"encryption_method": "argon2id-v2"` (under a `"discovery":
   "sequential"` — i.e. an Icarus-style sample). This is now a
   factually wrong example for a schema that says Byron can't produce
   it. Update that literal value to `"pbkdf2-hmac-sha512"` (or
   `"scrypt"`) to keep the fixture honest. This is not required to make
   any test pass (golden-spec decoding is unconstrained by the OpenAPI
   enum) — do it anyway for documentation consistency, and note in
   `WIP.md` that it's a non-test-driven consistency fix.

**TDD shape:** RED is the existing
`validateEveryToJSONWithPatternChecker (Proxy @(Api T0))` spec starting
to fail once step 1 lands alone (may take several QuickCheck runs to
surface the counterexample — record the failing sample in `WIP.md`).
GREEN is step 2 restoring conformance. Commit steps 1–3 together as one
slice once GREEN (do not commit the RED state).

**Gate:** `nix develop --quiet -c cabal build cardano-wallet-api-http-test`
(or the repo's actual unit-test target name — check `justfile`/CI for
the exact invocation) focused on `TypesSpec`, e.g.
`nix develop --quiet -c just unit "TypesSpec"` if such a pattern-filter
exists, otherwise the full unit suite.

### Slice 2 — Unit proof: Byron/Icarus never persist Argon2id V2

**Files:** `lib/unit/test/unit/Cardano/WalletSpec.hs` only.

**Context:** The existing `"V1 → V2 root key migration"` describe block
(~line 503) already proves the *Shelley* side of both acceptance
criteria via `dummyStateF :: WalletFlavorS DummyState` (`DummyState =
TestState ... ShelleyKey ...`, ~line 1932) and `setupFixture`/
`WalletLayerFixture`:
- `walletRootKeyV2Idempotent` (~line 790) already proves
  `attachPrivateKeyFromPwd` on Shelley persists `HashedCredentialsV2`
  (i.e. `EncryptWithArgon2idV2`) — **no change needed for the Shelley
  side of AC "Shelley create reports argon2id-v2".**
- Nothing in this file exercises the Byron/Icarus flavor.

**Change:** Add a Byron-flavored fixture and two new tests, mirroring
the existing Shelley pattern:
- Use the **production** `ByronWallet :: WalletFlavorS (RndState n)`
  constructor (`lib/wallet/src/Cardano/Wallet/Flavor.hs:90`) — do not
  invent a new dummy type. `RndState` already has a production
  `PersistAddressBook` instance
  (`lib/wallet/src/Cardano/Wallet/DB/Store/Checkpoints/Store.hs:807`).
  `mkRndState` (used in `Cardano.Wallet.Address.Discovery.RandomSpec`,
  ~line 312: `mkRndState @'Mainnet rootXPrv 0`) builds an initial
  `RndState`.
- Check `setupFixture`'s type signature in this file — it should
  already be polymorphic over the state type (it's called with
  `dummyStateF :: WalletFlavorS DummyState` today); confirm it type-checks
  against `ByronWallet :: WalletFlavorS (RndState 'Mainnet)` too. If
  `setupFixture` turns out to be hard-coded to `DummyState` in a way
  that resists reuse, that is a legitimate blocker — write a `Q-NNN`
  file rather than forcing a bigger refactor.
- New test A: mirrors `walletRootKeyV2Idempotent`'s shape but for the
  Byron fixture — after `W.attachPrivateKeyFromPwd wl (byronKey, pwd)`,
  assert the stored credentials are `HashedCredentialsV1` (never `V2`),
  and (if scheme is readable at this layer) that the scheme is
  `EncryptWithPBKDF2`, never `EncryptWithArgon2idV2`.
- New test B: mirrors `walletRootKeyV1toV2Migration`'s shape but for
  the Byron fixture — after `withRootKey` unlock, assert the stored
  credentials are **still** `HashedCredentialsV1` (no migration
  happened), unlike the Shelley test which asserts migration *did*
  happen.
- Sanity check (not a permanent code change): before finalizing,
  temporarily comment out the `ByronKeyS -> pure ()` no-op branch in
  `withRootKey` (`lib/wallet/src/Cardano/Wallet.hs:~4444`) and confirm
  test B fails — proves the test actually exercises the intended
  no-migration path rather than passing vacuously. Revert the comment
  before committing.

**TDD shape:** This locks in already-correct production behavior (per
investigation, `withRootKey`'s Byron/Icarus branch is already a
no-op and `attachPrivateKeyFromPwd`'s Byron/Icarus branch is already
`legacyV1`). A documented RED-skip exception applies: "production
behavior already correct; new tests characterize and regression-lock
it; the sanity check above (temporarily breaking the no-op) stands in
for RED." Log this explicitly in `WIP.md`.

**Gate:** `nix develop --quiet -c just unit "WalletSpec"` (or full unit
suite if no pattern filter exists).

### Slice 3 — Regression test: incomplete shared wallet omits `passphrase`

**Files:** `lib/unit/test/unit/Cardano/Wallet/Api/TypesSpec.hs` (or a
new small unit test file if that's cleaner) — driver's call on exact
location, staying inside the existing JSON-golden/schema test area.

**Change:** `ApiIncompleteSharedWallet`
(`specifications/api/swagger.yaml:3242-3269`) has no `passphrase`
property at all today — the runtime type should mirror that
structurally (check the Haskell `ApiIncompleteSharedWallet` /
equivalent record in `lib/api/src/Cardano/Wallet/Api/Types.hs` has no
`passphrase` field, or that `ApiSharedWallet`'s `Left` branch — see
`ApiSharedWallet`'s `Arbitrary` at `TypesSpec.hs:1465-1470`,
`oneof [Right <$> arbitrary, Left <$> arbitrary]` — never serializes a
`passphrase` key). Add a small unit test: encode an arbitrary/fixture
`ApiIncompleteSharedWallet` (or the `Left` branch of `ApiSharedWallet`)
value to JSON and assert the resulting `Object` has no `"passphrase"`
key (e.g. `KeyMap.member "passphrase" obj \`shouldBe\` False`).

**TDD shape:** Should already pass (the schema already lacks the
field) — this is new regression coverage, not a bug fix. If it
surprisingly fails, that is a real bug to report, not paper over.

**Gate:** same unit-test target as Slice 1.

### Slice 4 — Fix `openapi-diff.sh` release-tag baseline selection

**Files:** `scripts/release/openapi-diff.sh` only.

**Change:** Line ~16 currently:
```bash
last_release_tag=$(curl -s https://api.github.com/repos/cardano-foundation/cardano-wallet/releases | jq -r '.[0].tag_name')
```
takes `.[0]` unconditionally — a `nightly` prerelease republished on
its own schedule routinely has a more recent `published_at` than the
last stable tag, silently flipping the diff baseline. Fix:
```bash
last_release_tag=$(curl -s https://api.github.com/repos/cardano-foundation/cardano-wallet/releases | jq -r '[.[] | select(.prerelease == false)][0].tag_name')
```

**TDD shape:** No test harness for this shell script (legitimate
RED-skip per resolve-ticket's auto-decided exceptions). Proof: run the
`curl | jq` pipeline before/after and show the selected tag stays the
stable one even with a more-recent `nightly` present; paste both
outputs into `WIP.md`.

**Gate:** manual smoke as described above; `shellcheck
scripts/release/openapi-diff.sh` if shellcheck is already used
elsewhere in this repo's CI (check `.github/workflows/`), otherwise
skip.
