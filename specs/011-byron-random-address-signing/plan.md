# Implementation Plan: Byron random wallets cannot sign for pre-2018 addresses

- **Branch**: `011-byron-random-address-signing` | **Date**: 2026-08-11 | **Spec**: [spec.md](./spec.md)
- **Input**: Feature specification from `/specs/011-byron-random-address-signing/spec.md`
- **Issue**: [#5368](https://github.com/cardano-foundation/cardano-wallet/issues/5368)

## Summary

`Rnd.isOwned` derives a signing key from the derivation path recorded inside a Byron random address
and returns it without checking that the key reproduces the address
(`lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs:254`). For
addresses created before the `cardano-sl` hardening fix, the recorded index is not the index the key
was derived at, so the witness does not match the input and the node rejects the transaction.

The change is confined to key resolution. `isOwned` gains a verification step — derive, rebuild the
address from the resulting public key using the attributes carried by the target address, compare —
and, when the recorded path fails, retries at the hardened forms of the address index, the account
index, and both. The first candidate that reproduces the address wins; if none does, `isOwned`
returns `Nothing` rather than a key that will fail at submission.

Address reconstruction is added to `Cardano.Byron.Codec.Cbor`, which already owns the Byron address
binary format and its `encodeAddress` root computation. No signature at any call site changes:
`Cardano.Wallet.Address.States.IsOwned` dispatches to `Rnd.isOwned` unchanged, and payments
(`lib/wallet/src/Cardano/Wallet.hs:2977`, `lib/wallet/src/Cardano/Wallet.hs:3383`), witness
construction (`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:2529`) and migration
(`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:4774`, via
`W.buildAndSignTransaction` at `lib/wallet/src/Cardano/Wallet.hs:3212`) all inherit the fix.

## Technical Context

- **Language/Version**: Haskell, GHC pinned by the repository flake
- **Primary Dependencies**: `cardano-crypto` (`deriveXPrv`, `toXPub`), `cborg`, in-repo
`cardano-wallet-address-derivation-discovery`
- **Storage**: none — nothing persisted changes; the key is recomputed on demand (spec Assumptions)
- **Testing**: HSpec / QuickCheck through `cardano-wallet-unit:unit`; API integration scenarios through
`cardano-wallet-integration` against a local cluster
- **Target Platform**: multi-platform library code, verified locally on Linux
- **Project Type**: Haskell monorepo under `lib/`
- **Performance Goals**: exactly one key derivation and one address reconstruction per input when the
recorded path identifies the key (SC-006, asserted structurally rather than measured); one extra key
derivation per further candidate, only once an earlier candidate has failed
- **Constraints**: no change to address discovery (FR-005); no change to `isOwned`'s type or to any call
site (FR-006); no new error surface for an unresolvable address (FR-009); no database migration; no
new constraint threaded into the wallet-flavour dispatch
- **Scale/Scope**: two library modules and their specs, plus two Byron API integration scenarios

## Constitution Check

| Principle | Status | Notes |
|---|---|---|
| Maintenance-first stability | OK | Bug fix on a currently-unspendable path. Behaviour for correctly-recorded addresses is preserved by FR-004 and asserted by existing `RandomSpec` properties. |
| Era-aware design | OK | Byron address format only. Witness construction is untouched; `mkByronWitness` already builds attributes from the address, not from the key. |
| Type safety as security | OK | Verification narrows the result: no key is returned unless it provably reproduces the address. Explicit export lists and Haddock on every new export. |
| Formal specification | OK | No API surface change, so `specifications/api/swagger.yaml` is untouched. No Lean invariant covers Byron key resolution. |
| Reproducible builds | OK | All commands run under `nix develop --quiet`; no dependency change. |
| Comprehensive testing | OK | Unit coverage for each candidate class and for the no-match case; integration coverage for spending (SC-001, SC-003) and migration (SC-004), which are the only place those criteria can be observed. |
| Code quality gates | OK | Fourmolu (70 columns, leading commas), HLint, `-Wall`, unit and integration suites. |

No violations, so **Complexity Tracking** is empty.

Re-checked after Phase 1: the design adds two functions to one package and changes no type, no
persisted shape and no API contract, so every row above stands. One item moved from unknown to
tracked — `RandomSpec`'s `golden03` asserts a key at a soft path for an address this codebase did not
generate, and may itself encode the defect. It is resolved by the probe in quickstart.md step 1
before any code is written, and either outcome is compatible with the design (research.md §"Open
item").

## Project Structure

### Documentation (this feature)

```text
specs/011-byron-random-address-signing/
├── spec.md
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
│   └── key-resolution.md
└── tasks.md            # /speckit.tasks output — not created here
```

### Source Code (repository root)

Expected write set:

```text
lib/address-derivation-discovery/lib/Cardano/Byron/Codec/Cbor.hs
lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs
lib/unit/test/unit/Cardano/Byron/Codec/CborSpec.hs
lib/unit/test/unit/Cardano/Wallet/Address/Discovery/RandomSpec.hs
lib/integration/scenarios/Test/Integration/Scenario/API/Byron/Transactions.hs
lib/integration/scenarios/Test/Integration/Scenario/API/Byron/Migrations.hs
lib/integration/framework/Test/Integration/Framework/DSL.hs
```

`DSL.hs` gains one address constructor for the affected shape, alongside the existing
`randomAddresses` (`lib/integration/framework/Test/Integration/Framework/DSL.hs:3062`), so both
integration scenarios build their fixtures the same way.

Must not change:

```text
lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs
lib/wallet/src/Cardano/Wallet.hs
lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs
lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs
specifications/api/swagger.yaml
```

An edit to any of these means the fix has leaked out of key resolution and FR-006 is no longer
satisfied as written.

**Structure Decision**: the change stays inside `lib/address-derivation-discovery`, which owns both
the Byron address codec and the random-derivation discovery state. Tests for that package live in
`lib/unit`; API-level behaviour lives in `lib/integration`.

## Vertical Slice Contract

One behaviour-changing commit:

`fix(byron): verify derived keys reproduce the address before signing`

It must contain:

1. RED proof in `RandomSpec` that an address recording index *i* whose key is at *i* hardened is not
   resolvable today, and that a decrypting-but-underivable address yields a key today.
2. Address reconstruction in `Cardano.Byron.Codec.Cbor`, built from the attributes of the target
   address.
3. Candidate enumeration and verification in `Rnd.isOwned`, first match wins, no match yields
   `Nothing`.
4. Unit coverage for all four candidate classes, the no-match case, and the unchanged foreign-wallet
   case.
5. Integration coverage for spending and migrating an affected address.
6. Local gate green with no diff outside the write set above.

Integration scenarios may land as a second commit if the local cluster run is the only thing
separating them; the library change must never land without its unit proof.

## Proof Strategy

Focused development loop:

```sh
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Random Address Discovery"'
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0 \
  --test-options '--match="Cardano.Byron.Codec.Cbor"'
```

Acceptance:

```sh
just check-fmt && just hlint
just unit-tests-cabal-match "Random Address"
just integration-tests-cabal-match "BYRON_MIGRATE"
just integration-tests-cabal-match "BYRON_TRANS"
```

Post-implementation checks:

```sh
git diff --name-only master...HEAD
rg -n "isOwned" lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs
```

The first must list only the expected write set. The second must be unchanged from `master`.

## Risks And Mitigations

- **Verification rejects an address the node would have accepted.** Mitigated by reconstructing from
  the address's own attributes, which is the same input the ledger check uses — see research.md
  §"Faithfulness to the ledger check".
- **Cost on the happy path.** Verification adds one `toXPub` and one hash-and-compare per input, on
  every path including the unaffected one. Bounded and argued in research.md §"Cost". SC-006 now
  states a structural bound — one derivation, one reconstruction — which a unit test asserts; it was
  never a zero-cost claim and is no longer a latency claim.
- **The residual class is created by this change, not exposed by it.** Today `isOurs` and `isOwned`
  test the same condition for Byron random wallets, so every UTxO the wallet holds yields a key.
  Verification makes `isOwned` strictly stronger, so an address that decrypts but matches no
  candidate now resolves to `Nothing`. FR-009 fixes the consequence: no witness is produced for that
  input — `signTransaction` assembles witnesses with `mapMaybe` (`Shelley/Transaction.hs:365`) — and
  the node rejects the transaction as it does today. No new error path, and nothing to add at the
  three call sites.
- **The returned key's `derivationPath` field no longer matches the address's recorded path** for
  affected addresses. Nothing reads it today (research.md §"Consumers of the returned key"), but
  anything that later rebuilds an address from the returned key via `paymentAddress` would be wrong.
  A Haddock note on `isOwned` records this.
- **Integration fixture cannot produce an affected address.** Mitigated by constructing the address
  directly from `CBOR.encodeAddress` and `CBOR.encodeDerivationPathAttr` and funding it with
  `moveByronCoins`, which already sends to caller-supplied addresses.
- **An existing golden goes red for the right reason.** `RandomSpec`'s `golden03` records a soft path
  and asserts the key at it. If that address's key is really at the hardened form, the golden was
  asserting a key the chain would reject and its expectation must be updated. Resolve first
  (quickstart.md step 1); never update a golden to match new behaviour without confirming which key
  actually reproduces the address.
- **Scope creep into discovery.** FR-005 is explicit; `isOurs`, `addressToPath` and
  `discoveredAddresses` keying stay as they are, including the pre-existing path-collision behaviour
  noted in research.md.
