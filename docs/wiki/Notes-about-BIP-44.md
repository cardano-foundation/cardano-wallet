## Abstract

This document gives a semi-technical overview of multi-account hierarchy for deterministic wallets, their trade-offs and limitations. 

## Overview 

[BIP-44](https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki) is a standard to defines a logical hierarchy for deterministic wallets. It is constructed on top of another standard called [BIP-32](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki) which describes how to create hierarchical deterministic (abbrev. HD) wallets.
In a nutshell, a HD wallet is a set of public/private key pairs that all descend from a common root key pair. The process of generating a new key pair from a parent key pair is known as _key derivation_. BIP-32 offers an approach for defining such structure by showing how to derive child keys from a parent key, an index and an elliptic curve. Cardano differs 
from BIP-32 in its choice of elliptic curve (namely Curve25519) but the base principle remains the same. 

On top of this derivation mechanism, BIP-44 defines a set of _5 standard levels_ (i.e. subsequent derivation indexes) with a precise semantic. In particular:

1. The first derivation index is called the _purpose_ and is always set to `44'` to indicate that the derivation indexes must be interpreted according to BIP-44.

2. The second derivation index is called the _coin\_type_ and is set depending on the underlying coin. There's a [public list of registered coin types](https://github.com/satoshilabs/slips/blob/master/slip-0044.md). Ada is registered as `1815'`. The idea of having this second level 
  is to allow a wallet to use a single root key to manage assets of different kinds. 

3. The third derivation index is called the _account_ and is used to separate the space in multiple user entities for enhanced organization. 

4. The fourth derivation index is called the _change_ which is meant for distinguish addresses that need to be treated as change from those treated as deposit addresses.

5. The fifth and last index is called the _address_, which is meant to increase sequentially to generate new addresses. 

Each derivation level can contain up to `2^31 (2 147 483 648)` different values, which in total makes for _a lot_ of possible combinations. In practice, the first two levels are always set to the same constants and the third and fourth index has a very limited range. 

## Address & Account discovery

Because it is not possible for a software to know after the facts what indexes were used to obtain a particular key pair, one needs a strategy for discovering addresses and knowing whether a given address belongs to a wallet. 
A naive approach would be to generate upfront all possible addresses for a given sub-path and to perform lookups in this table. Not only would this be extremely inefficient (it would take ~4 months to generate the entire space for a _single_ sub-path on a decent hardware), it would also be very much ineffective (for each address on a blockchain, one would have to lookup in an index containing more than two billion entries).  

Instead, BIP-44 specifies a few key points that each software implementing it should follow. In particular, BIP-44 introduces the concept of a _gap limit_, which is the maximum number of **consecutive** unused addresses the software must keep track of at any time.


### Example

Let see how it works with an example in which we consider only a single derivation level (the last one) and for which the gap limit is set to `3`. 

1. A new empty wallet would be allowed to generate up to 3 addresses with index `0`, `1` and `2`

    ```
    ┌───┬───┬───┐
    │ 0 │ 1 │ 2 │
    └───┴───┴───┘
    ```


2. The wallet scans the blockchain looking for addresses which would have been generated from these indexes. An addresses is found using the index `i=2`. 

    ```
              ↓
    ┌───┬───┬───┐       ┌───┬───┬───┬───┬───┬───┐
    │ 0 │ 1 │ 2 │   ⇒   │ 0 │ 1 │ ✓ │ 3 │ 4 │ 5 │
    └───┴───┴───┘       └───┴───┴───┴───┴───┴───┘
    ```
  
   By discovering an address using `i=2`, the wallet needs to generate 3 new addresses at indexes `i=3`, `i=4` and `i=5` so that it is always keeping track of 3 consecutive unused addresses. 


3. The wallet continues scanning the blockchain and finds an address at index `i=0`. 
  
    ```
      ↓
    ┌───┬───┬───┬───┬───┬───┐      ┌───┬───┬───┬───┬───┬───┐
    │ 0 │ 1 │ ✓ │ 3 │ 4 │ 5 │  ⇒   │ ✓ │ 1 │ ✓ │ 3 │ 4 │ 5 │
    └───┴───┴───┴───┴───┴───┘      └───┴───┴───┴───┴───┴───┘
    ```

    Because discovering the address at index `i=0` does not reduce the number of consecutive unused addresses, there's no need to generate new addresses, there are still 3 consecutive unused addresses available.


4. The wallet continues scanning the blockchain and finds an address at index `i=3`

    ```
                  ↓
    ┌───┬───┬───┬───┬───┬───┐      ┌───┬───┬───┬───┬───┬───┬───┐
    │ ✓ │ 1 │ ✓ │ 3 │ 4 │ 5 │  ⇒   │ ✓ │ 1 │ ✓ │ ✓ │ 4 │ 5 │ 6 │
    └───┴───┴───┴───┴───┴───┘      └───┴───┴───┴───┴───┴───┴───┘
    ```

    Here, we need to generate only one new address in order to maintain a number of 3 consecutive unused addresses. 


5. This goes on and on until there are no more addresses discovered on-chain with indexes we know of. 


**What if there's an address with index `i=7` ?**

## Limitations of BIP-44

The discovery algorithm above works only because wallet software enforces and satisfies two invariants which are stated in BIP-44 as such:


1. Software should prevent a creation of an account if a previous account does not have a transaction history (meaning none of its addresses have been used before). 

2. Address gap limit is currently set to 20. If the software hits 20 unused addresses in a row, it expects there are no used addresses beyond this point and stops searching the address chain. 

For sequential discovery to work, one needs to fix some upper boundaries. Without such boundaries, it'd be impossible for a wallet software to know _when_ to stop generating addresses. Because each software that follows BIP-44 also abides by these two rules, it generally works fine. Yet, there are some annoying consequences stemming from it:


### Limit #1 - Unsuitable for exchanges

Users like exchanges do typically need to generate _a lot_ of unused addresses upfront that their users can use for deposit. BIP-44 is a standard that is well tailored for personal wallets, where the address space grows with the user needs. It is however a quite poor choice for exchanges who typically use a single set of root credentials for managing assets of thousands of users. 

Possible mitigations: 

1. Exchanges could make use of a larger gap limit, violating the classic BIP-44 default but, still following the same principle otherwise. Despite being inefficient, it could work for exchanges with a limited number of users. 
2. Another wallet scheme that would be better suited for exchanges should probably be considered.  


### Limit #2 - Error prone with staging networks

A subtle issue with BIP-44 wallets would be to use a software to generate addresses on a given network, and use such generated addresses on a different albeit compatible network. This has been the case between the Cardano Mainnet and Staging networks recently. Let's run through the following example:

1. A user creates a brand new wallet on the staging network. Following the default recommended gap limit, 20 unused addresses are generated for this wallet.

2. The user also creates a wallet on the main network, using the same recovery phrase. Because both networks share the same address discimination policy, there's no difference between addresses of both networks. Addresses from the staging network are valid addresses on the main network.

3. This user gives her 10th address to someone in order to receive some funds on the **staging network**. Eventually, she receives funds on her addresses which causes the **staging** wallet to generate 10 new addresses (as seen before).

4. Then, she also needs to receive funds on her wallet from the **main network** but mistakenly takes an address from the **staging network**. Above all, she happens to take the 24th addresses of her staging wallet. 

5. Because addresses are valid on both networks, the transaction is successfully processed but never show up in her main wallet. 


What has happened? The **main** wallet only looks for addresses with indexes between 0 and 19 (incl.) and a transaction is made on an address with index `i=23`; it is therefore outside of what's currently observable by the main wallet. This is because, from the main wallet's perspective, one of the two BIP-44 invariants has been violated. It shouldn't have been possible to generate an address beyond the 20th index on the main network because no previous address had been used on this network. 

Possible mitigations:

1. Having a different discrimination between staging and main networks would prevent addresses from one to be mistakenly used on another. In practice, we typically want such two networks to be as close as possible. Thus enforcing this discrimination simply in _client softwares_ could already be a sufficient for most users. More advanced users who would be crafting addresses themselves would still be able to by-pass this kind of safety net, but it comes with the "advanced" powers.

2. Wallet could allow arbitrarily longer gaps to allow transactions to be discovered even if made outside of the default gap limit. In most cases, this would be sufficient with a slightly larger gap (e.g. 40 or 50) because typical users don't use large indexes anyway. We've used this technique with gap as high as 10.000 (with a following shrinking) during the ITN on Cardano to cope with the lack of blockchain history caused by the ITN snapshot.