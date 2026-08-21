# Research: Byron random address key resolution

**Issue**: [#5368](https://github.com/cardano-foundation/cardano-wallet/issues/5368)
**Prior art**: [#1041](https://github.com/cardano-foundation/cardano-wallet/issues/1041) (indexes
outside the hardened domain exist), [#1042](https://github.com/cardano-foundation/cardano-wallet/issues/1042)
(`Index 'WholeDomain` so they are discovered)
**Measured at**: `d3d170d02`, 2026-08-11

## Current-state inventory

Key resolution for Byron random wallets, in full:

```haskell
-- lib/address-derivation-discovery/lib/Cardano/Wallet/Address/Discovery/Random.hs:248
isOwned st (key, pwd) addr =
    (,pwd) . deriveCredFromKeyKeyFromPath key pwd
        <$> addressToPath addr (hdPassphrase st)
```

`addressToPath` (`Random.hs:275`) decrypts the derivation-path attribute out of the address;
`deriveCredFromKeyKeyFromPath` (`Random.hs:352`) derives account then address key at that path.
Nothing compares the resulting key to the address. Every ingredient for the comparison is already in
the package.

| Piece | Location | Note |
|---|---|---|
| Address root computation | `Cardano/Byron/Codec/Cbor.hs:349` `encodeAddress` | `blake2b224 . sha3256` over `[addrType, spendingData, attributes]`; attributes supplied by the caller |
| Payload extraction | `Cbor.hs:180` `decodeAddressPayload` | Strips the CRC wrapper, canonical decode |
| Attribute extraction | `Cbor.hs:217` `decodeAllAttributes` | Returns `[(Word8, ByteString)]`, canonical map decode |
| Same pattern, already used | `Cardano/Wallet/Address/Encoding.hs:290` `toHDPayloadAddress` | Local decoder: `decodeListLenCanonicalOf 3`, skip root, `decodeAllAttributes` |
| Index domain lifting | `Cardano/Wallet/Address/Derivation.hs:486` `liftIndex` | `firstHardened = 0x80000000` at `Derivation.hs:397` |

`isOwned` is dispatched by flavour at
`lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs:68`, which already carries a
`HasSNetworkId (NetworkOf s)` constraint, and `NetworkOf (RndState n) = n`
(`Cardano/Wallet/Address/States/Families.hs:47`).

### Consumers of the returned key

| Call site | Use |
|---|---|
| `lib/wallet/src/Cardano/Wallet.hs:2977` (`buildAndSignTransactionPure`) | passed to `signTransaction` as `keyFrom` |
| `lib/wallet/src/Cardano/Wallet.hs:3383` (`buildAndSignTransaction`) | passed to `mkTransaction` as `keyFrom` |
| `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs:2529` | passed to witness construction as `keyLookup` |

All three reach `mkTxInWitness` (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs:445`), which
takes `getRawKey keyF k` and nothing else. The `derivationPath` and `payloadPassphrase` fields of the
returned `ByronKey` are discarded. Migration reaches the same place through
`W.buildAndSignTransaction` (`Server.hs:4774`), which is why FR-006 needs no call-site change.

## Faithfulness to the ledger check

What the node validates is not "the key is at the recorded path" but "the address root recomputes
from the witness's public key and the witness's attributes". `mkByronWitness`
(`Shelley/Transaction.hs:834`) builds those attributes from the address:

```haskell
addrAttr =
    Byron.mkAttributes
        $ Byron.AddrAttributes
            (toHDPayloadAddress addr)
            (Byron.toByronNetworkMagic nw)
```

`mkByronWitnessLedger` (`Shelley/Transaction/Ledger.hs:404`) does the same. So the witness carries the
address's own HD payload verbatim, and only the key material varies. Verifying locally by rebuilding
the address from a candidate public key **under the target address's own attributes** reproduces the
node's acceptance criterion exactly. Two consequences:

- A candidate our check accepts is a candidate the node accepts, up to the network magic discussed
  below.
- An address our check rejects for every candidate is an address for which this wallet could not
  produce a valid witness at all. FR-003's `Nothing` is therefore the correct answer, not a
  conservative one.

The one divergence: our reconstruction takes the network-magic attribute from the address, while the
witness takes it from the wallet's configured network. These differ only for an address belonging to
a different network, which cannot appear in this wallet's UTxO set.

## Decisions

| Decision | Rationale | Alternative rejected |
|---|---|---|
| Rebuild the address from the target's own attributes, via a new function in `Cardano.Byron.Codec.Cbor`. | That module already owns `encodeAddress` and the payload decoders; the reconstruction is pure binary-format work. Needs no network identifier, so `isOwned` keeps its type and the flavour dispatch is untouched (spec Assumptions). | `paymentAddressS @n` on the candidate key. It builds the derivation-path attribute from the key's own `derivationPath` field (`Address/Derivation/Byron.hs:160`), which for a hardened candidate encrypts the *hardened* path — producing an address that cannot match a target recording the soft path. Making it work requires splicing the recorded path into the candidate key's record, which fabricates a `ByronKey` whose `derivationPath` contradicts its `getKey`, and additionally binds verification to the wallet's configured network rather than the address's attributes. |
| Compare the full reconstructed address bytes, not just the 28-byte root. | Matches Acceptance Scenario 1.2 as written ("reproduces the address exactly") and self-checks the attribute re-encoding at the same time. Cost difference is one CRC32 over ~40 bytes. | Root-only comparison. Same crypto cost, weaker statement, and no cheaper: the root is a hash over the re-encoded attributes either way. |
| Candidate order: recorded, address index hardened, account index hardened, both. Stop at the first match. | The order the spec fixes in Key Entities, most likely first. FR-008 makes the common case exactly one derivation and one comparison. | Deriving all four and picking the match: four times the cost for wallets that are not affected at all. |
| De-duplicate candidates before evaluating. | An already-hardened path collapses all four candidates to one, which is the overwhelmingly common case (spec Edge Cases). Order-preserving dedup on a four-element list. | Deriving duplicates and relying on the first-match short-circuit: correct but does redundant work whenever exactly one level is soft. |
| Keep index hardening local to `Random.hs`. | Only candidate generation needs it; keeping it out of `Cardano.Wallet.Address.Derivation` keeps the diff inside one package and avoids adding a general-purpose index operation nobody else calls. | Exporting `hardenIndex` next to `liftIndex` in `Derivation.hs`. Conceptually the right home; revisit if a second caller appears. |
| No change to discovery. | FR-005. `isOurs` and `addressToPath` already find these addresses correctly — that is what #1042 delivered. | Re-keying `discoveredAddresses` by address instead of path would fix the collision noted below but changes the state's shape and its persisted representation. |

## Cost

Per input resolved, on top of what happens today:

| Path | Extra work |
|---|---|
| Recorded path correct (all wallets today) | one `toXPub`, one SHA3-256 + Blake2b-224 over ~90 bytes, one CRC32, one `ByteString` comparison |
| Address index hardened | the above ×2, plus one extra `deriveXPrv` pair |
| Account index hardened | ×3, plus two extra `deriveXPrv` pairs |
| Both, or no match | ×4, plus three extra `deriveXPrv` pairs |

Verification is unconditional — that is the point of FR-001 — so even unaffected wallets pay one
public-key computation and one hash per input. A `toXPub` is one Ed25519 scalar multiplication, the
same order as the `deriveXPrv` calls already performed and well under the Ed25519 signature that
follows.

SC-006 was originally a latency claim. Nothing in the repository measures this path — `latency-bench`
drives Shelley wallets only, and `restore-bench` exercises `RndState` through `isOurs`, not `isOwned`
— so it has been restated as a structural bound: one derivation and one reconstruction per input when
the recorded path is correct. That is assertable in a unit test via `candidatePaths`, and it is the
property the latency wording was reaching for. No benchmark is added.

An optional refinement, not required: candidates sharing an account index can share the account key,
reducing the worst case from eight `deriveXPrv` calls to six. Only reachable on the fallback path.

## Resolved: `golden03` does not encode the defect

**Measured 2026-08-13**, by rebuilding the address from the test's own mnemonic at both candidate
paths and comparing bytes (`RandomSpec.hs`, `golden03 provenance`):

| Key derived at | Reconstructed root | Verdict |
|---|---|---|
| recorded path (14, 42) | `f26d102b29332fd6c244a9915b6cad7890f5b54ac34dcd62975b525a` | matches the golden |
| hardened (14+2³¹, 42+2³¹) | `9a29bd1f8515c49428c5b2a10049768fcfd70c4e2f757cd5b065ac74` | does not match |

So `golden03`'s key genuinely is at the soft path it records. Its `accIndex`/`addrIndex` expectation
stands unchanged, and the address is now a worked example of the spec's own caveat that a soft index
does not by itself imply the defect. It is direct evidence for FR-004: an address of this shape must
keep resolving on the first candidate, with no fallback derivation.

The passing assertion is kept in `RandomSpec.hs` as `golden03Provenance`, so the claim above is
re-runnable rather than a transcript.

The original reasoning that made this worth checking is preserved below, since it explains why the
address looked suspicious.

## Why `golden03` was suspect

`RandomSpec.hs:237` carries a golden whose recorded path is soft — account 14, address 42 — and
asserts that `isOwned` returns the key at exactly that path (`checkIsOwned`, `RandomSpec.hs:295`).
Today that assertion cannot fail for the wrong reason, because nothing verifies the key. After the
fix it can: if this address's root is not the key at (14, 42), `isOwned` will return the key at the
hardened form instead, and the golden will go red.

Two features of the address make this worth checking rather than assuming:

- Its attributes carry protocol magic `0x2d964a09` = 764824073, the **mainnet** magic, while sitting
  in the testnet golden group; the other testnet goldens carry `0x4170cb17` = 1097911063. This
  codebase's `paymentAddress` emits the magic attribute only for testnet
  (`Address/Derivation/Byron.hs:160`), so this address was not produced by either branch of it.
- It is the only golden in the file with a soft recorded path. The mainnet goldens and testnet
  `golden01`/`golden02` all record indexes ≥ 2³¹.

The mainnet-magic oddity turned out to be a red herring for provenance: whatever produced the
address, it derived the key at the path it recorded. The measurement above settles it.

## Known limitation, out of scope

`discoveredAddresses` is keyed by `DerivationPath` (`Random.hs:170`). An affected and an unaffected
address can record the same path (spec Edge Cases), in which case the map retains only one of them
and the address list under-reports. This predates the change and is unaffected by it: signing takes
the `Address` from the UTxO and never consults that map, so both addresses remain spendable. FR-005
keeps discovery untouched, so this stays as it is.

## Test strategy

RED, before any implementation:

1. `RandomSpec`: for an address built as `encodeAddress (xpub at hardened path)
   [derivationPathAttr recorded-soft-path]`, `isOwned` returns a key today — the wrong one. Assert
   that the returned key reproduces the address, and that it equals the key at the hardened path.
   Both fail today.
2. `RandomSpec`: an address whose attributes decrypt under the wallet's `hdPassphrase` but whose root
   comes from an unrelated public key yields a key today. Assert `Nothing`, which fails today.

GREEN adds verification and the candidate ladder. Coverage per requirement:

| Requirement | Test |
|---|---|
| FR-001, SC-005 | property: for every `isOwned` result, the returned key reproduces the address |
| FR-002, SC-001 | golden + property: address index hardened resolves |
| FR-003, SC-005 | decrypts-but-underivable address yields `Nothing`; foreign-wallet address still yields `Nothing` |
| FR-004, SC-002 | existing `prop_derivedKeysAreOwned` (`RandomSpec.hs:354`) and the mainnet/testnet goldens, unchanged and still passing, including `golden03` (`RandomSpec.hs:237`) whose recorded indexes are 14 and 42 — soft, and correct |
| FR-007, SC-007 | account index hardened, and both hardened, resolve |
| FR-008, SC-006 | candidate list for an already-hardened path is a singleton, so the recorded-path case performs one derivation and one reconstruction |
| FR-009 | no new error constructor, no swagger error entry, no change to any signing signature — checked by the diff, not by a test |
| FR-006, SC-003, SC-004 | integration: spend from an affected address, spend affected and unaffected in one transaction, migrate a wallet holding one |

`CborSpec` covers the reconstruction function directly: for any public key and attribute set,
reconstructing from the address that key produced returns that address; reconstructing from a
different key does not; a non-address byte string returns `Nothing`.

Integration fixtures build the affected address in the test — `CBOR.encodeAddress` over the public key
at the hardened path with `CBOR.encodeDerivationPathAttr` recording the soft path, plus
`encodeProtocolMagicAttr` on testnet — and fund it with `moveByronCoins`
(`Test/Integration/Framework/DSL.hs:2811`), which already sends to caller-supplied addresses and waits
for the destination balance. That wait doubles as confirmation that discovery is unaffected.

## Risks

- **Non-canonical attribute encodings.** Re-encoding decoded attribute pairs must be byte-identical to
  the original. The decoders enforce canonical forms (`decodeListLenCanonicalOf`,
  `decodeMapLenCanonical`), and any address that failed this would already be unspendable by this
  wallet, since the ledger path re-encodes attributes too. Failure mode is a visible `Nothing`, never
  a wrong key.
- **A fifth variant exists.** If some affected address matches none of the four candidates, FR-003
  makes it fail locally and visibly instead of at submission, and the candidate list is one function
  to extend.
- **Integration flakiness.** The scenarios depend on a local cluster and faucet timing, like every
  other Byron migration scenario. Reuse the existing `eventually` helpers rather than new waits.
