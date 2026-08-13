# Contract: Byron random key resolution

The wallet's external contract — the REST API in `specifications/api/swagger.yaml` — does not change.
No endpoint, request body, response body or error code is added, removed or altered. What changes is
which transactions the node accepts, on endpoints that already exist:

| Endpoint | Before | After |
|---|---|---|
| `POST /v2/byron-wallets/{id}/transactions` | Rejected at submission with `MissingVKeyWitnessesUTXOW` when the selection includes an affected UTxO | Accepted |
| `POST /v2/byron-wallets/{id}/migrations` | Same, whenever the plan includes one | Accepted |
| `GET /v2/byron-wallets/{id}/addresses` | Unchanged | Unchanged |
| `GET /v2/byron-wallets/{id}` (balance) | Unchanged | Unchanged |

The contract that does change is internal, between the address-discovery package and everything that
signs.

## `Cardano.Wallet.Address.Discovery.Random.isOwned`

```haskell
isOwned
    :: forall (network :: NetworkDiscriminant)
     . RndState network
    -> (ByronKey 'RootK XPrv, Passphrase "encryption")
    -> Address
    -> Maybe (ByronKey 'CredFromKeyK XPrv, Passphrase "encryption")
```

Signature unchanged, including the absence of any `HasSNetworkId` constraint.

**Preconditions.** The passphrase is the wallet's; a wrong one silently yields wrong keys, as it does
for every other derivation in this module (`Address/Derivation/Byron.hs:314`). With verification in
place a wrong passphrase resolves to `Nothing` instead of to a wrong key, since no candidate
reproduces the address. Both outcomes fail; the wallet validates the passphrase before signing, so
neither is reachable through the API, and callers may not treat this as a passphrase check.

**Postconditions.**

1. `Just (k, pwd)` implies `reconstruct (toXPub (getKey k)) addr == Just addr`. No unverified key is
   ever returned. (FR-001, FR-003)
2. If the address's recorded path reproduces the address, `k` is the key at that path — the result is
   identical to today's, and identical to `deriveCredFromKeyKeyFromPath` at that path. (FR-004)
3. `Nothing` for an address that does not decrypt under the wallet's `hdPassphrase`. Unchanged.
4. `Nothing` for an address that decrypts but that no candidate reproduces. (FR-003) This class does
   not exist before the change — `isOurs` and `isOwned` test the same condition today — so it is
   created here, deliberately. Its consequence is fixed by FR-009: `signTransaction` assembles
   witnesses with `mapMaybe` (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs:365`), so the
   input simply carries no witness and the node rejects the transaction, exactly as it does today. No
   error is raised locally and no API error is added.
5. Candidates are tried in the order recorded → address hardened → account hardened → both, and
   evaluation stops at the first match. (FR-007, FR-008)
6. The result's `pwd` component is the passphrase passed in. Unchanged.

**Caller obligation.** Use `getKey` only. The returned key's `derivationPath` field is the candidate
path and, for an affected address, is not the path the address records; rebuilding an address from the
returned key via `paymentAddress` would produce a different address.

## `Cardano.Byron.Codec.Cbor` — reconstruction

One new exported function, name at the implementer's discretion, with this contract:

```haskell
-- | Rebuild the Byron address that the given public key produces under the
-- attributes carried by the given address.
reconstructAddress :: XPub -> Address -> Maybe Address
```

1. `Nothing` when the argument is not a well-formed Byron address payload of a public-key address.
2. `reconstructAddress xpub (paymentAddress s k) == Just (paymentAddress s k)` when `xpub` is the
   public key of `k`, for either network discrimination.
3. `reconstructAddress xpub' addr /= Just addr` for any `xpub'` other than the one `addr` was built
   from.
4. Pure, total on its declared domain, and independent of the wallet's configured network: the
   protocol-magic attribute is copied from the argument, never synthesised.
5. Attributes are preserved verbatim, in order, including any tag this codebase does not interpret.

Any supporting decoder added alongside it (an attribute list extracted from a payload) is an
implementation detail of this contract, but if exported it should replace the duplicate local decoder
at `Cardano/Wallet/Address/Encoding.hs:301` rather than sit next to it.

## Dispatch

`Cardano.Wallet.Address.States.IsOwned.isOwned` (`lib/wallet/src/Cardano/Wallet/Address/States/IsOwned.hs:68`)
must not change. It is the proof that FR-006 holds: payments, witness construction and migration all
route through this one dispatch, and none of them names a derivation path.
