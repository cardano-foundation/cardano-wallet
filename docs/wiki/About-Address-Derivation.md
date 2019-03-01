# Hd Random and Sequential Key Derivation Schemes

## Cardano *Hd Random* Addresses
                                                                                                                                                                                                 
```
+--------------------------------------------------------------------------------+
|              Base-58 Encoded CBOR-Serialized Object with CRC*                  |
|                                                                                |
|      DdzFFzCqrhstiVdBdYEAmpLPtSWxFYy...rYcBLq29xJD4xZw16REKyhJC9PFGgPSbX       |
|                                                                                |
+--------------------------------------------------------------------------------+
                                    |
                                    |
                                    v
+--------------------------------------------------------------------------------+
|     Address Root    |     Address Attributes    |           AddrType           |
|                     |                           |                              |
|   Hash (224 bits)   |  Der. Path* + Stake + NM  |  PubKey | (Script) | Redeem  | 
|                     |    (open for extension)   |     (open for extension)     |
+--------------------------------------------------------------------------------+
             |                 |                                           
             |                 |     +----------------------------------+   
             v                 |     |        Derivation Path           |   
+---------------------------+  |---->|                                  |   
|  SHA3-256 >> Blake2b_224  |  |     | ChaChaPoly* AccountIx/AddressIx  |   
|                           |  |     +----------------------------------+   
|  -AddrType                |  |                                          
|  -ASD* (~AddrType+PubKey) |  |     +----------------------------------+   
|  -Address Attributes      |  |     |       Stake Distribution         |   
+---------------------------+  |     |                                  |   
                               |---->|  BootstrapEra | (Single | Multi) |   
                               |     +----------------------------------+   
                               |                                          
                               |     +----------------------------------+   
                               |     |          Network Magic           |   
                               |---->|                                  |   
                                     | Addr Discr: MainNet vs TestNet   |   
                                     +----------------------------------+   

```
<p align="center"><small>Fig 1.</small></p>


- (\*) CRC: [Cyclic Redundancy Check](https://computer.howstuffworks.com/encryption7.htm);
  sort of checksum, a bit (pun intended) more reliable.

- (\*) ASD: Address Spending Data; Some data that are bound to an address. It's
  an extensible object with payload which identifies one of the three elements:  
    - A Public Key (Payload is thereby a PublicKey)  
    - A Script (Payload is thereby a script and its version)  
    - A Redeem Key (Payload is thereby a RedeemPublicKey)  

- (\*) Derivation Path: Note that there's no derivation path for Redeem nor
  Scripts addresses!

- (\*) ChaChaPoly: Authenticated Encryption with Associated Data; See [RFC
  7539](https://datatracker.ietf.org/doc/rfc7539) We use it as a way to cipher
  the derivation path using a passphrase (the root public key).


### *Hd Random* Key Derivation

An initial key is created by some combination of HMAC and Ed25519 cryptographic
function from a seed (encoded in the form of mnemonic words) and a passphrase.
From this wallet Key, other keys can be derived provided the passphrase is
known. 


```
+--------------------------------------------------------------------------------+
|         BIP-39 Encoded 128 bits Seed with CRC a.k.a 12 Mnemonic Words          |
|                                                                                |
|    squirrel material silly twice direct ... razor become junk kingdom flee     |
|                                       +                                        |
|                            Base58-Encoded Passphrase                           |
|                                                                                |
+--------------------------------------------------------------------------------+
              |                                                     
              |                                                    
              v                                                     
+--------------------------+    +-----------------------+
|    Wallet Private Key    |--->|   Wallet Public Key   |
+--------------------------+    +-----------------------+
              |                                            
              | (+ passphrase)                                          
              v                                            
+--------------------------+    +-----------------------+
|   Account Private Key    |--->|   Account Public Key  |
+--------------------------+    +-----------------------+
              |                                            
              | (+ passphrase)                                            
              v                                            
+--------------------------+    +-----------------------+
|   Address Private Key    |--->|   Address Public Key  |
+--------------------------+    +-----------------------+
```
<p align="center"><small>Fig 2.</small></p>

## *Hd Sequential* wallets (Ed25519/BIP-44)

### Motivation 

BIP-0044 Multi-Account Hierarchy for Deterministic Wallets is a Bitcoin
standard defining a structure and algorithm to build a hierarchy tree of keys
from a single root private key.

It is built upon
[BIP-0032](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) and
is a direct application of
[BIP-0043](https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki).  It
defines a common representation of addresses as a multi-level tree of
derivations:

```
m / purpose' / coin_type' / account' / change / address_index
```

Paths with a tilde `'` refer to BIP-0032 hardened derivation path (meaning that
the private key and passphrase are required to derive children). This leads to
a couple of interesting properties:

1. New addresses (change or not) can be generated from an account's public key
   alone.
1. The derivation of addresses is done sequentially / deterministically. 
1. If an account private key is compromised, it doesn't compromise other
   accounts. 

This allows for external keystores and off-loading of key derivation to some
external source such that a wallet could be tracking a set of accounts without
the need for knowing private keys. This approach is discussed more in details
below. 

> NOTE:  
> One other important aspect is more of a security-concern. The introduction of
> such new address scheme makes it possible to change the underlying derivation
> function to a new better one with stronger cryptographic properties. This is
> rather ad-hoc to the structural considerations above, but is still a major
> motivation.

### Differences between *Hd Random *and *Sequential* Addresses

Because BIP-44 public keys (Hd Sequential) are generated in sequence, there's no need 
to maintain a derivation path in the address attributes (incidentally, this
also makes addresses more private). Instead, we can generate pools of
addresses up to a certain limit (called address gap) for known accounts and
look for those addresses during block application. 

We end up with two kind of Cardano addresses:


| Address V1 (Hd Random)                                   | Address V2 (Icarus, Hd Sequential)                                     |
| ---                                                      | ---                                                     |
| Uses ed25519@V1 (buggy) curve impl for derivation        | Uses ed25519@V2 curve impl for derivation               |
| Has a derivation path attribute                          | Has no derivation path attribute                        |
| New address indexes are random                           | New address indexes are sequential                      |
| Need root private key and passphrase to create addresses | Need only parent account public key to create addresses |
| Root keys are obtained from 12-word mnemonic phrases     | Root keys are obtained from 15-word mnemonic phrases    |

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

### Differences between Cardano *Hd Sequential* and BIP-44

In BIP-44, new derivation paths are obtained by computing points on an elliptic
curve where curve parameters are defined by secp256k1. Cardano's implementation
relies on ed25519 for it provides better properties in terms of security and 
performances. 