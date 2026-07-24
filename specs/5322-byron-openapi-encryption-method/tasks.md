# Tasks — #5322

## Slice 1 — Narrow the Byron OpenAPI passphrase schema

- [X] T5322-S1a Add `walletPassphraseInfoLegacy` anchor to
      `specifications/api/swagger.yaml` (enum: `scrypt`,
      `pbkdf2-hmac-sha512`); point `ApiByronWallet.passphrase` at it;
      leave `ApiWallet` / `ApiActiveSharedWallet` on the original
      `walletPassphraseInfo` anchor.
- [X] T5322-S1b Confirm RED: `validateEveryToJSONWithPatternChecker
      (Proxy @(Api T0))` fails after T5322-S1a alone (record the
      counterexample in `WIP.md`).
- [X] T5322-S1c Replace the generic `Arbitrary ApiByronWallet` instance
      in `TypesSpec.hs` with one whose `passphrase` field only
      generates `EncryptWithScrypt` / `EncryptWithPBKDF2`.
- [X] T5322-S1d Confirm GREEN: focused unit run passes.
- [X] T5322-S1e Fix the stale `"argon2id-v2"` example in
      `lib/unit/test/data/Cardano/Wallet/Api/ApiByronWallet.json` to a
      legitimate Byron value.
- [X] T5322-S1f Commit (one commit, Tasks: T5322-S1a, T5322-S1b,
      T5322-S1c, T5322-S1d, T5322-S1e).

## Slice 2 — Unit proof: Byron/Icarus never persist Argon2id V2

- [X] T5322-S2a Add a Byron-flavored `WalletLayerFixture` in
      `Cardano.WalletSpec.hs` using the production `ByronWallet ::
      WalletFlavorS (RndState n)` + `mkRndState`.
- [X] T5322-S2b New test: `attachPrivateKeyFromPwd` on the Byron
      fixture persists `HashedCredentialsV1` / `EncryptWithPBKDF2`,
      never `HashedCredentialsV2` / `EncryptWithArgon2idV2`.
- [X] T5322-S2c New test: `withRootKey` unlock on the Byron fixture
      leaves credentials at V1 (no migration).
- [X] T5322-S2d Sanity check (not committed): temporarily break the
      `ByronKeyS -> pure ()` no-op in `withRootKey` and confirm
      T5322-S2c fails; revert.
- [X] T5322-S2e Commit (one commit, Tasks: T5322-S2a, T5322-S2b,
      T5322-S2c; log the RED-skip rationale + T5322-S2d's result in
      `WIP.md`).

## Slice 3 — Regression test: incomplete shared wallet omits `passphrase`

- [X] T5322-S3a New unit test: encoding an `ApiIncompleteSharedWallet`
      (or `ApiSharedWallet`'s `Left` branch) to JSON never produces a
      `"passphrase"` key.
- [X] T5322-S3b Commit (one commit, Tasks: T5322-S3a).

## Slice 4 — Fix `openapi-diff.sh` release-tag baseline selection

- [ ] T5322-S4a Change the `jq` filter in
      `scripts/release/openapi-diff.sh` to
      `select(.prerelease == false)` before picking `.tag_name`.
- [ ] T5322-S4b Manual smoke: paste before/after `curl | jq` output in
      `WIP.md` showing a stable tag is chosen even with a more-recent
      `nightly` present.
- [ ] T5322-S4c Commit (one commit, Tasks: T5322-S4a, T5322-S4b).

## Finalization

- [ ] Audit PR body matches delivered behavior.
- [ ] Drop `gate.sh` (`chore: drop gate.sh (ready for review)`).
- [ ] `gh pr ready`.
