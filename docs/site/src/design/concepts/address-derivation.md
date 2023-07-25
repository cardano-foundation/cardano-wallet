# Address Derivation

## *HD Random* wallets (Byron / Legacy)

#### Note

This scheme is an underspecified / ad-hoc scheme designed in the early eras of
Cardano. It is intrinsically entangled to the address format and relies on
the ability to embed extra pieces of information into addresses themselves in
order to perform key derivation.

An initial key is created using a Ed25519 cryptographic elliptic curve from a
seed (encoded in the form of mnemonic words). From this wallet Key, other keys
can be derived. We therefore define a hierarchy of depth 2, where a single root
key and derivation indexes defines a **derivation path**.

We typically represent that derivation path as follows:

```
m/account_ix'/address_ix'
```

where

- `m` refers to the root master key
- `/` symbolizes a new derivation step using a particular derivation index.
- `'` indicates that keys at this step are considered hardened keys
  (private key is required to derive new keys). Indexes of hardened keys
  follows a particular convention and belongs to the interval `[2³¹, 2³²-1]`.

```
+--------------------------------------------------------------------------------+
|         BIP-39 Encoded 128 bits Seed with CRC a.k.a 12 Mnemonic Words          |
|                                                                                |
|    squirrel material silly twice direct ... razor become junk kingdom flee     |
|                                                                                |
+--------------------------------------------------------------------------------+
              |
              |
              v
+--------------------------+    +-----------------------+
|    Wallet Private Key    |--->|   Wallet Public Key   |    m
+--------------------------+    +-----------------------+
              |
              | account ix
              v
+--------------------------+    +-----------------------+
|   Account Private Key    |--->|   Account Public Key  |    m/account_ix'
+--------------------------+    +-----------------------+
              |
              | address ix
              v
+--------------------------+    +-----------------------+
|   Address Private Key    |--->|   Address Public Key  |    m/account_ix'/address_ix'
+--------------------------+    +-----------------------+
```
<p align="center"><small>Fig 1. Byron HD Wallets Key Hierarchy</small></p>

Note that wallets typically store keys in memory in an encrypted form, using an
encryption passphrase. That passphrase prevents "free" key manipulation.

## *HD Sequential* wallets (à la BIP-44)

BIP-0044 Multi-Account Hierarchy for Deterministic Wallets is a Bitcoin
standard defining a structure and algorithm to build a hierarchy tree of keys
from a single root private key. Note that this is the derivation scheme
used by Icarus / Yoroi.

It is built upon [BIP-0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) and is a direct application of
[BIP-0043](https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki).
It defines a common representation of addresses as a multi-level tree of derivations:

```
m / purpose' / coin_type' / account_ix' / change_chain / address_ix
```

Cardano uses an extension / variation of BIP-44 described in [CIP-1852](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1852/CIP-1852.md).

```
+--------------------------------------------------------------------------------+
|                BIP-39 Encoded Seed with CRC a.k.a Mnemonic Words               |
|                                                                                |
|    squirrel material silly twice direct ... razor become junk kingdom flee     |
|                                                                                |
+--------------------------------------------------------------------------------+
       |
       |
       v
+--------------------------+    +-----------------------+
|    Wallet Private Key    |--->|   Wallet Public Key   |
+--------------------------+    +-----------------------+
       |
       | purpose (e.g. 1852')
       |
       v
+--------------------------+
|   Purpose Private Key    |
+--------------------------+
       |
       | coin type (e.g. 1815' for ADA)
       v
+--------------------------+
|  Coin Type Private Key   |
+--------------------------+
       |
       | account ix (e.g. 0')
       v
+--------------------------+    +-----------------------+
|   Account Private Key    |--->|   Account Public Key  |
+--------------------------+    +-----------------------+
       |                                          |
       | chain  (e.g. 1 for change)               |
       v                                          v
+--------------------------+    +-----------------------+
|   Change Private Key     |--->|   Change Public Key   |
+--------------------------+    +-----------------------+
       |                                          |
       | address ix (e.g. 0)                      |
       v                                          v
+--------------------------+    +-----------------------+
|   Address Private Key    |--->|   Address Public Key  |
+--------------------------+    +-----------------------+
```
<p align="center"><small>Fig 2. BIP-44 Wallets Key Hierarchy</small></p>



Paths with a tilde `'` refer to BIP-0032 hardened derivation path (meaning that
the private key is required to derive children). This leads to a couple of
interesting properties:

1. New addresses (change or not) can be generated from an account's public key alone.
1. The derivation of addresses can be done sequentially / deterministically.
1. If an account private key is compromised, it doesn't compromise other accounts.

This allows for external key-stores and off-loading of key derivation to some
external source such that a wallet could be tracking a set of accounts without
the need for knowing private keys. This approach is discussed more in details
below.

> NOTE:
> One other important aspect is more of a security-concern. The introduction of
> such new address scheme makes it possible to change the underlying derivation
> function to a new better one with stronger cryptographic properties. This is
> rather ad-hoc to the structural considerations above, but is still a major
> motivation.

### Differences between *Cardano HD Sequential* and *BIP-44*

In BIP-44, new derivation paths are obtained by computing points on an elliptic
curve where curve parameters are defined by secp256k1. Cardano's implementation
relies on ed25519 for it provides better properties in terms of security and
performances.

Also, we use `purpose = 1852'` to clearly distinguish these formats from the original BIP-44 specification. Note however that Yoroi/Icarus in the Byron era are using `purpose = 44'`.

## Differences between *HD Random* and *HD Sequential* Wallets

Because BIP-44 public keys (HD Sequential) are generated in sequence, there's
no need to maintain a derivation path in the address attributes (incidentally,
this also makes addresses more private). Instead, we can generate pools of
addresses up to a certain limit (called address gap) for known accounts and
look for those addresses during block application.

We end up with two kind of Cardano addresses:


| Address V1 (Hd Random)                                   | Address V2 (Icarus, HD Sequential)                             |
| ---                                                      | ---                                                            |
| Uses ed25519@V1 (buggy) curve impl for derivation        | Uses ed25519@V2 curve impl for derivation                      |
| Has a derivation path attribute                          | Has no derivation path attribute                               |
| New address indexes are random                           | New address indexes are sequential                             |
| Need root private key and passphrase to create addresses | Need only parent account public key to create addresses        |
| Root keys are obtained from 12-word mnemonic phrases     | Root keys are obtained from mnemonic phrases of various length |

Although the idea behind the BIP-44 protocol is to be able to have the wallet
working in a mode where the wallet doesn't know about the private keys, we still
do want to preserve compatibility with the existing address scheme (which can't
work without knowing private keys).

This leaves us with three operational modes for a wallet:

- Compatibility Mode with private key: In this mode, addresses are derived
  using the wallet root private key and the classic derivation scheme. New address
  indexes are generated randomly.

- Deterministic Mode without private key: Here, we review the definition of a
  wallet down to a list of account public keys with no relationship whatsoever
  from the wallet's point of view. New addresses can be derived for each account
  at will and discovered using the address pool discovery algorithm described in
  BIP-44. Public keys are managed and provided from an external sources.

- Deterministic Mode with private key: This is a special case of the above. In
  this mode, the wallet maintain the key hierarchy itself, and leverage the
  previous mode for block application and restoration.

Those operational modes are detailed in more depth here below. Note that we'll
call _external wallets_ wallets who don't own their private key.
