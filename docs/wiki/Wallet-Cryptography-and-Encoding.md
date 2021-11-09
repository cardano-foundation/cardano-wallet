1. [Recovery Phrases](#recovery-phrases)
    1. [Motivation](#motivation)
    1. [Encoding](#encoding)
    1. [Dictionaries](#dictionaries)
1. [Hierarchical Deterministic Wallets](#hierarchical-deterministic-wallets)
    1. [Motivation](#motivation)
    1. [Notation](#notation)
    1. [Path Levels](#path-levels)
    1. [Account Discovery](#account-discovery)
    1. [Address Gap Limit](#address-gap-limit)
1. [Master Key Generation](#master-key-generation)
    1. [History](#history)
    1. [Overview](#overview)
    1. [Pseudo-code](#pseudo-code)
        1. [Byron](#byron)
        1. [Icarus](#icarus)
        1. [Ledger](#ledger)

## Recovery Phrases

### Motivation

We define a way for easily entering and writing down arbitrary binary seeds using
a simple dictionary of known words (available in many different languages).

The motivation here is to have sentence of words easy to read and write for humans,
which map uniquely back and forth to a sized binary data (harder to remember).

### Encoding

The process describing how to encode recovery phrases is described in [BIP-0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
section "Generating the mnemonic". Below is a reformulation of this specification.

We call _Entropy_ an arbitrary sequence of bytes that has been generated through high
quality randomness methods. The allowed size of _Entropy_ is 96-256 bits and is 
necessarily a multiple of 32 bits (4 bytes). 

A checksum is appended to the initial entropy by taking the first `ENT / 32` bits 
of the SHA256 hash of it, where `ENT` designates the _Entropy_ size in bits. 

Then, the concatenated result is split into groups of 11 bits, each encoding a number
from 0 to 2047 serving as an index into a known dictionary (see below).

| Sentence Length | Entropy Size        | Checksum Size |
| ------------    | ------------------- | ------        |
| 9 words         | 96  bits (12 bytes) | 3 bits        |
| 12 words        | 128 bits (16 bytes) | 4 bits        |
| 15 words        | 160 bits (20 bytes) | 5 bits        |
| 18 words        | 192 bits (24 bytes) | 6 bits        |
| 21 words        | 224 bits (28 bytes) | 7 bits        |
| 24 words        | 256 bits (32 bytes) | 8 bits        |

### Dictionaries

Cardano uses the same dictionaries as defined in [BIP-0039](https://github.com/bitcoin/bips/blob/master/bip-0039/bip-0039-wordlists.md).

---

## Hierarchical Deterministic Wallets 

### Motivation 

In Cardano, hierarchical deterministic (abbrev. HD) wallets are similar to those described in [BIP-0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#motivation).

Deterministic wallets and elliptic curve mathematics permit schemes where one can
calculate a wallet public keys without revealing its private keys. This permits for
example a webshop business to let its webserver generate fresh addresses
(public key hashes) for each order or for each customer, without giving the
webserver access to the corresponding private keys (which are required for spending the received funds).

However, deterministic wallets typically consist of a single "chain" of
keypairs. The fact that there is only one chain means that sharing a wallet
happens on an all-or-nothing basis. However, in some cases one only wants some
(public) keys to be shared and recoverable. In the example of a webshop, the
webserver does not need access to all public keys of the merchant's wallet;
only to those addresses which are used to receive customer's payments, and not
for example the change addresses that are generated when the merchant spends
money. Hierarchical deterministic wallets allow such selective sharing by
supporting multiple keypair chains, derived from a single root. 

### Notation

Conceptually, HD derivation can be seen as a tree with many branches, where keys
live at each node and leaf such that an entire sub-tree can be recovered from 
only a parent key (and seemingly, the whole tree can be recovered from the root 
master key). 

For deriving new keys from parent keys, we use the same approach as defined in
[BIP32-Ed25519: Hierarchical Deterministic Keys over a Non-linear
Keyspace](https://cardanolaunch.com/assets/Ed25519_BIP.pdf).

We note `CKDpriv` the derivation of a private child key from a parent private key such that:

<pre>
CKDprv((k<sup>P</sup>, c<sup>P</sup>), i) → (k<sub>i</sub>, c<sub>i</sub>)
</pre>

We note `CKDpub` the derivation of a public child key from a parent public key such that:

<pre>
i <  2<sup>31</sup>: CKDpub((A<sup>P</sup>, c<sup>P</sup>), i) → (A<sub>i</sub>, c<sub>i</sub>)
</pre>

> NOTE: This is only possible for so-called "soft" derivation indexes, smaller than 2<sup>31</sup>.

We note `N` the public key corresponding to a private key such that:

<pre>
N(k, c) → (A, c) 
</pre>

To shorten notation, we will borrow the same notation as described in BIP-0032
and write CKDpriv(CKDpriv(CKDpriv(m,3H),2),5) as m/3H/2/5. Equivalently for
public keys, we write CKDpub(CKDpub(CKDpub(M,3),2),5) as M/3/2/5. 

### Path Levels

Cardano wallet defines the following path levels:

<pre>
m / purpose<sub>H</sub> / coin_type<sub>H</sub> / account<sub>H</sub> / account_type / address_index
</pre>

- <code>purpose<sub>H</sub></code> is set to <code>1852<sub>H</sub></code>
- <code>coin_type<sub>H</sub></code> is set to <code>1815<sub>H</sub></code>
- <code>account<sub>H</sub></code> is set for now to <code>0<sub>H</sub></code>
- <code>account_type</code> is either:
  - `0` to indicate an address on the external chain, that is, an address 
    that is meant to be public and communicated to other users. 
  - `1` to indicate an address on the internal chain, that is, an address
    that is meant for change, generated by a wallet software.
  - `2` to indicate a reward account address, used for delegation.
- <code>address_index</code> is either:
  - `0` if the `account_type` is `2`
  - Anything between 0 and 2<sup>31 otherwise

### Account Discovery

> What follows is taken from the "Account Discovery" section from [BIP-0044](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#account-discovery)

When the master seed is imported from an external source the software should start to discover the accounts in the following manner:

-    derive the first account's node (index = 0)
-    derive the external chain node of this account
-    scan addresses of the external chain; respect the gap limit described below
-    if no transactions are found on the external chain, stop discovery
-    if there are some transactions, increase the account index and go to step 1

For the algorithm to be successful, software should disallow creation of new accounts if previous one has no transaction history.

Please note that the algorithm works with the transaction history, not account balances, so you can have an account with 0 total coins and the algorithm will still continue with discovery.

### Address gap limit

Address gap limit is currently set to 20. If the software hits 20 unused addresses in a row, it expects there are no used addresses beyond this point and stops searching the address chain. We scan just the external chains, because internal chains receive only coins that come from the associated external chains.

Wallet software should warn when the user is trying to exceed the gap limit on an external chain by generating a new address. 


## Master Key Generation

### History

Throughout the years, Cardano has been using different styles of HD wallets. 
We categorize these wallets in the following terms:

Wallet Style | Compatible Products
---          | ---
Byron        | Daedalus, Yoroi
Icarus       | Yoroi, Trezor
Ledger       | Ledger

Each wallet is based on Ed25519 elliptic curves though differs in subtle ways 
highlighted in the next sections.

### Overview

The master key generation is the mean by which on turns an initial entropy into 
a secure cryptographic key. Child keys can be derived from a master key to produce
an HD structure as outlined above. Child key derivation is explored in next sections.

In Cardano, the master key generation is different depending on which style of wallet
one is considering. In each case however, the generation is a function from an initial
seed to an extended private key (abbrev. XPrv) composed of:

- 64 bytes: an extended Ed25519 secret key composed of:
    - 32 bytes: Ed25519 curve scalar from which few bits have been tweaked (see below)
    - 32 bytes: Ed25519 binary blob used as IV for signing
- 32 bytes: chain code for allowing secure child key derivation

> Additional resources:
> 
> - [SLIP 0010](https://github.com/satoshilabs/slips/blob/master/slip-0010.md)
> - [BIP 0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki)
> - [BIP 0039](https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki)
> - [RFC 8032](https://tools.ietf.org/html/rfc8032#section-5.1.5)

### Pseudo-code

#### Byron

```
generateMasterKey(seed) {
    return hashRepeatedly(seed, 1);
}

hashRepeatedly(key, i) {
    (iL, iR) := HMAC
        ( hash=SHA512
        , key=key
        , message="Root Seed Chain " ++ UTF8NFKD(i)
        );
    
    prv := tweakBits(SHA512(iL));

    if (prv[31] & 0b0010_0000) { 
        return hashRepeatedly(key, i+1);
    }

    return (prv ++ iR);
}

tweakBits(data) {
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0111_1111;
    data[31] |= 0b0100_0000;
}
```

#### Icarus

_Icarus_ master key generation style supports setting an extra password as an arbitrary 
byte array of any size. This password acts as a second factor applied to cryptographic key 
retrieval. When the seed comes from an encoded recovery phrase, the password can therefore
be used to add extra protection in case where the recovery phrase were to be exposed.

```
generateMasterKey(seed, password) {
    data := PBKDF2
        ( kdf=HMAC-SHA512
        , iter=4096
        , salt=seed
        , password=password
        , outputLen=96
        );

    return tweakBits(data);
}

tweakBits(data) {
    // on the ed25519 scalar leftmost 32 bytes:
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * clear the 3rd highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0001_1111;
    data[31] |= 0b0100_0000;
}
```

> For a detailed analysis of the cryptographic choices and the above requirements, 
> have a look at: [Wallet Cryptography and Encoding](https://github.com/input-output-hk/chain-wallet-libs/blob/master/doc/CRYPTO.md#master-key-generation-to-cryptographic-key)

#### Ledger

```
generateMasterKey(seed, password) {
    data := PBKDF2
        ( kdf=HMAC-SHA512
        , iter=2048
        , salt="mnemonic" ++ UTF8NFKD(password)
        , password=UTF8NFKD(spaceSeparated(toMnemonic(seed)))
        , outputLen=64
        );

    cc := HMAC
        ( hash=SHA256
        , key="ed25519 seed"
        , message=UTF8NFKD(1) ++ seed
        );

    (iL, iR) := hashRepeatedly(data);

    return (tweakBits(iL) ++ iR ++ cc);
}

hashRepeatedly(message) {
    (iL, iR) := HMAC
        ( hash=SHA512
        , key="ed25519 seed"
        , message=message
        );
    
    if (iL[31] & 0b0010_0000) { 
        return hashRepeatedly(iL ++ iR);
    }

    return (iL, iR);
}

tweakBits(data) {
    // * clear the lowest 3 bits
    // * clear the highest bit
    // * set the highest 2nd bit
    data[0]  &= 0b1111_1000;
    data[31] &= 0b0111_1111;
    data[31] |= 0b0100_0000;
}
```