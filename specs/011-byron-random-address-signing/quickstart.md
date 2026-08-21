# Quickstart: Byron random address key resolution

All commands assume the repository root and the Nix dev shell:

```sh
nix develop --quiet
```

## Baseline

```sh
git status --short --branch
just check-fmt
nix develop --quiet -c cabal build cardano-wallet-address-derivation-discovery -O0 -v0
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Wallet.Address.Discovery.Random"'
```

Expected: green. This is the suite that must stay green for FR-004 / SC-002.

## Step 1 — Resolve the `golden03` question first

`RandomSpec.hs:237` asserts a key at a soft path for an address this codebase did not generate
(research.md §"Open item"). Establish where that address's key actually is before writing anything
else, because the answer decides whether an existing golden is evidence for the fix or a casualty of
it.

Do this as a spec case, not a REPL session. The answer changes an existing golden's expectation, so
the evidence has to be something a reviewer can re-run — and in one of the two outcomes the probe
becomes the regression test for FR-002 over a real pre-2018 address, which is stronger evidence than
any fixture this feature can construct.

Add to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs` a pair of assertions that
rebuild `golden03`'s address from the wallet's own key material — the recorded path in the
derivation-path attribute either way, only the key's path differing:

```haskell
golden03Provenance :: Spec
golden03Provenance = describe "golden03 provenance" $ do
    it "is reproduced by the key at its recorded path"
        $ hex (addressAt (Index 14, Index 42)) `shouldBe` hex golden03Address
    it "is reproduced by the key at the hardened path"
        $ hex (addressAt (Index (14 + h), Index (42 + h)))
            `shouldBe` hex golden03Address
  where
    h = 0x80000000
```

Exactly one must pass. Run:

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="golden03 provenance"'
```

Keep the passing assertion, renamed to state the finding, and delete the other. Record the outcome
in research.md §"Open item", then either leave the golden alone or update its `accIndex`/`addrIndex`
to the hardened values as part of the implementation commit.

If neither passes, stop and report: the address carries something neither candidate class explains,
and the candidate set in data-model.md needs revisiting before any code is written.

## Step 2 — RED

Add to `lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs`:

1. An affected-address fixture: `CBOR.encodeAddress` over the public key at the **hardened** path,
   with `CBOR.encodeDerivationPathAttr` recording the **soft** path. Assert `isOwned` returns the key
   at the hardened path.
2. A property over arbitrary mnemonics and indexes: whatever `isOwned` returns must reproduce the
   address.
3. An unownable-address fixture: attributes encrypted under the wallet's `hdPassphrase`, root built
   from an unrelated public key. Assert `isOurs` is `Just` and `isOwned` is `Nothing`.

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Wallet.Address.Discovery.Random"'
```

Capture the failures. (1) and (3) fail on the returned key; (2) fails for the affected fixture.

## Step 3 — GREEN

1. `lib/address-derivation-discovery/lib/Cardano/Byron/Codec/Cbor.hs`: add the attribute decoder and
   the reconstruction function of contracts/key-resolution.md. If the decoder is exported, replace
   the duplicate at `Cardano/Wallet/Address/Encoding.hs:301` with it.
2. `lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs`: add
   `candidatePaths` (exported for tests) and rewrite `isOwned` to derive-verify-or-continue over it.
   Haddock the caller obligation about `derivationPath` on `isOwned`.
3. Extend `lib/unit/test/unit/Cardano/Byron/Codec/CborSpec.hs` with the reconstruction round-trip,
   the wrong-key case, and the non-address case.

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Wallet.Address.Discovery.Random"'
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Byron.Codec.Cbor"'
```

## Step 4 — Integration

Add an affected-address constructor to
`lib/integration/framework/Test/Integration/Framework/DSL.hs` next to `randomAddresses`
(`DSL.hs:3062`), then:

- `Test/Integration/Scenario/API/Byron/Transactions.hs`: empty random wallet from a fresh mnemonic →
  fund one affected and one unaffected address with `moveByronCoins` (`DSL.hs:2811`) → spend an
  amount that forces both inputs → expect `202` and the transaction reaching `in_ledger`. Covers
  SC-001 and SC-003.
- `Test/Integration/Scenario/API/Byron/Migrations.hs`: same fixture → `POST
  /v2/byron-wallets/{id}/migrations` → expect the plan to include the affected UTxO and the wallet
  balance to reach zero. Covers SC-004.

```sh
just integration-tests-cabal-match "BYRON_TRANS"
just integration-tests-cabal-match "BYRON_MIGRATE"
```

The `moveByronCoins` step waits on the destination balance, which is also the check that discovery is
unchanged (FR-005). If funding succeeds and spending fails, the fixture is right and the fix is
wrong; if funding never lands, the fixture is wrong.

## Step 5 — Gate

```sh
just check-fmt
just hlint
just unit-tests-cabal-match "Cardano.Wallet.Address.Discovery.Random"
just unit-tests-cabal-match "Cardano.Byron.Codec"
git diff --name-only master...HEAD
```

The diff must contain only the write set in plan.md. In particular
`lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs`, `lib/wallet/src/Cardano/Wallet.hs` and
`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs` must not appear — their absence is the
evidence for FR-006.

## Commit

```text
fix(byron): verify derived keys reproduce the address before signing
```

Conventional Commits, single concern, `fix:` so release-please cuts a patch.
