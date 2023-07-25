# Master Key Generation

_Master key generation_ is the process by which the wallet turns a
[recovery-phrases](recovery-phrases.md#seeds--entropy)
(_entropy_) into
a secure cryptographic key. Child keys can be derived from a master key to produce
a derivation tree structure as outlined in
[hierarchical-deterministic-wallets](hierarchical-deterministic-wallets.md).

In Cardano, the master key generation algorithm is different depending on which style of wallet
one is considering. In each case however, the generation is a function from an initial
seed to an extended private key (_XPrv_) composed of:

- 64 bytes: an extended Ed25519 secret key composed of:
    - 32 bytes: Ed25519 curve scalar from which few bits have been tweaked (see below)
    - 32 bytes: Ed25519 binary blob used as IV for signing
- 32 bytes: chain code for allowing secure child key derivation

### Additional resources

- [SLIP 0010](https://github.com/satoshilabs/slips/blob/master/slip-0010.md)
- [BIP 0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
- [BIP 0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
- [RFC 8032](https://tools.ietf.org/html/rfc8032#section-5.1.5)
- [CIP 3 — "Wallet key generation"](https://cips.cardano.org/cips/cip3/)
- [CIP 1852 — "HD (Hierarchy for Deterministic) Wallets for Cardano"](https://cips.cardano.org/cips/cip1852/)

## History

Throughout the years, Cardano has been using different styles of HD wallets.
We categorize these wallets in the following terms:

Wallet Style | Compatible Products
---          | ---
Byron        | Daedalus, Yoroi
Icarus       | Yoroi, Trezor
Ledger       | Ledger

Each wallet is based on Ed25519 elliptic curves though differs in subtle ways
highlighted in the next sections.

## Pseudo-code

### Byron

```js
function generateMasterKey(seed) {
    return hashRepeatedly(seed, 1);
}

function hashRepeatedly(key, i) {
    (iL, iR) = HMAC
        ( hash=SHA512
        , key=key
        , message="Root Seed Chain " + UTF8NFKD(i)
        );

    let prv = tweakBits(SHA512(iL));

    if (prv[31] & 0b0010_0000) {
        return hashRepeatedly(key, i+1);
    }

    return (prv + iR);
}

function tweakBits(data) {
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0111_1111;
    data[31] |= 0b0100_0000;

    return data;
}
```

### Icarus

_Icarus_ master key generation style supports setting an extra password as an arbitrary
byte array of any size. This password acts as a second factor applied to cryptographic key
retrieval. When the seed comes from an encoded recovery phrase, the password can therefore
be used to add extra protection in case where the recovery phrase were to be exposed.

```js
function generateMasterKey(seed, password) {
    let data = PBKDF2
        ( kdf=HMAC-SHA512
        , iter=4096
        , salt=seed
        , password=password
        , outputLen=96
        );

    return tweakBits(data);
}

function tweakBits(data) {
    // on the ed25519 scalar leftmost 32 bytes:
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * clear the 3rd highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0001_1111;
    data[31] |= 0b0100_0000;

    return data;
}
```

#### More info

For a detailed analysis of the cryptographic choices and the above requirements,
have a look at: [Wallet Cryptography and Encoding](https://github.com/input-output-hk/chain-wallet-libs/blob/master/doc/CRYPTO.md#master-key-generation-to-cryptographic-key)

```js
function generateMasterKey(seed, password) {
    let data = PBKDF2
        ( kdf=HMAC-SHA512
        , iter=2048
        , salt="mnemonic" + UTF8NFKD(password)
        , password=UTF8NFKD(spaceSeparated(toMnemonic(seed)))
        , outputLen=64
        );

    let cc = HMAC
        ( hash=SHA256
        , key="ed25519 seed"
        , message=UTF8NFKD(1) + seed
        );

    let (iL, iR) = hashRepeatedly(data);

    return (tweakBits(iL) + iR + cc);
}

function hashRepeatedly(message) {
    let (iL, iR) = HMAC
        ( hash=SHA512
        , key="ed25519 seed"
        , message=message
        );

    if (iL[31] & 0b0010_0000) {
        return hashRepeatedly(iL + iR);
    }

    return (iL, iR);
}

function tweakBits(data) {
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0111_1111;
    data[31] |= 0b0100_0000;

    return data;
}
```
