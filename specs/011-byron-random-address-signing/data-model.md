# Data Model: Byron random address key resolution

No persisted type changes. Nothing in `RndState` is added, removed or re-keyed, so no database
migration is required (spec Assumptions). The entities below are values computed during signing.

## Entities

### Recorded derivation path

Already exists as `DerivationPath` (`Random.hs:202`):

```haskell
type DerivationPath =
    (Index 'WholeDomain 'AccountK, Index 'WholeDomain 'CredFromKeyK)
```

Recovered from the address by `addressToPath` (`Random.hs:275`), which decrypts attribute tag 1 under
the wallet's `hdPassphrase`. `'WholeDomain` because #1042 established that these indexes are not
necessarily hardened. Unchanged by this feature.

### Candidate derivation

A `DerivationPath` at which the signing key may lie. Derived from the recorded path by hardening
neither, one, or both levels. Hardening is `ix < 2³¹ ? ix + 2³¹ : ix`, where `2³¹` is
`getIndex (minBound @(Index 'Hardened _))` (`Derivation.hs:397`).

| # | Account index | Address index | Covers |
|---|---|---|---|
| 1 | recorded | recorded | every wallet in normal operation (FR-004) |
| 2 | recorded | hardened | the observed defect (FR-002) |
| 3 | hardened | recorded | account-level variant (FR-007) |
| 4 | hardened | hardened | both levels affected (FR-007) |

Ordered most likely first, de-duplicated with order preserved. A path already hardened at both levels
produces exactly one candidate, which is what makes FR-008's "one derivation, one comparison" the
common case. A path hardened at one level produces two.

### Address reconstruction

The test of a candidate. Given the candidate's public key and the **target address's own
attributes**:

```text
root       = blake2b224 (sha3_256 (cbor [0, [0, xpub], attributes]))
payload    = cbor [root, attributes, 0]
address    = cbor [tag 24, payload, crc32 payload]
```

which is `encodeAddress` (`Cbor.hs:349`) applied to the attribute list decoded out of the target. A
candidate matches when the reconstructed bytes equal the target address bytes.

Attributes are carried, never recomputed: tag 1 is the encrypted derivation path exactly as the
address records it, and tag 2 the protocol magic if present. This is what lets the check run without a
network identifier, and what makes it agree with the ledger — see research.md §"Faithfulness to the
ledger check".

## Resolution flow

```text
isOwned st (rootKey, pwd) addr
  |
  |-- addressToPath addr (hdPassphrase st)      -- Nothing => not ours, as today
  |
  |-- candidatePaths recordedPath               -- 1..4 paths, deduplicated, ordered
  |
  `-- first candidate p such that
        let k = deriveCredFromKeyKeyFromPath rootKey pwd p
        in  reconstruct (toXPub (getKey k)) addr == Just addr
      => Just (k, pwd)
      no such candidate => Nothing
```

## Invariants

- `isOwned` returns `Just (k, _)` only if `k`'s public key reproduces `addr` byte for byte (FR-001,
  FR-003, SC-005).
- If the recorded path reproduces the address, the returned key is the key at the recorded path and
  exactly one derivation and one reconstruction were performed (FR-004, FR-008).
- `isOwned`'s type is unchanged, and so is every call site (FR-006).
- `Nothing` stays `Nothing`: no error is raised for an unresolvable address, and the caller's
  behaviour — omit the witness, submit, let the node reject — is untouched (FR-009).
- `isOurs`, `addressToPath`, `importAddress`, `genChange` and the keying of `discoveredAddresses` are
  unchanged (FR-005).
- Reconstruction consumes the target address's attributes verbatim; it never re-encrypts a derivation
  path and never consults the wallet's network.
- The `derivationPath` field of a returned `ByronKey` is the *candidate* path, which for an affected
  address differs from the path the address records. Only `getKey` may be relied upon downstream.
