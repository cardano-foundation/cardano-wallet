## Motivation

The current cardano-wallet has accumulated a lot of technical debts and
complexity coming from various factors:

- Rapid teams hand-over before the end of the development
- An already bloated legacy upon which we tried to scaffold a new cleaner model
- Unnecessary layers of indirections without clear boundaries
- Some premature optimizations and unclear / undocumented design decisions
- Intricate glue code coming from the lack of / inefficient communication between teams
- Complex implementation decisions, especially on testing
- Feature creeping

As a result, implementing fundamentally new features has become extremely hard
and almost impossible in some cases without breaking backward compatibility.
With the upcoming implementation need for both decoupling (process-wise and
library-wise) and delegation, doubled with the struggle the team has been
experiencing with implementing BIP-44 (despite this being a rather _simple_
feature addition), we are considering starting over again using a more agile
development process from the start making testing and QA a first-class citizen
of that process.


## Plan

The team effort for the next 6 weeks will be put on implementing a first
minimal viable wallet working on a testnet, aiming for a smooth transition
to mainnet shortly after.

Doing so, the team will refine its development process as specified in the
[Cardano Wallet Backend - Software Development Manifesto]. In a glimpse: we
will divide the team in pairs and work in short iteration cycles.

The development of the minimal wallet is formerly aimed at being a "demoable",
working version of a decoupled wallet working with a testnet of Rust or Haskell
Cardano Nodes, focusing primarily on Rust nodes, which implement a subset of
the delegation features. This means that we can prioritize features and user
stories in a way that only what is really needed gets developed and tested. A
few examples:

- Incentive and pool rewards may not be needed for a first iteration
- Ranking and various pool metrics can be reduced down to:
    - The controlled stake
    - The number of slots correctly processed from a previous epoch
- Various actors (Node, Wallet Backend, Wallet Frontend) needn't to consider
  transition phases from Byron to this particular testnet. All nodes can be assumed
  to be already running this on this testnet.
- Some necessary features for an actual production system may be postponed to
  later like managing rollbacks or persisting the data on disk.
- A few basic primitives already available in Rust may just be used through FFI
  from Haskell code instead of being re-implemented in Haskell.

The end goal is really to be efficient in implementing something that can be
used as a foundation, aiming for a mainnet shortly after. By keeping the
implementation simple, unbloated, decoupled and working through well-defined
interfaces, we allow the wallet backend to later work with the complete Shelley
Haskell nodes, or more featureful Rust nodes. Meanwhile, a first minimal
implementation may already shed lights on some unforeseen issues and design
decisions.


## User Stories

### Forewords

Left out:

1. Updates

Essential, but not strictly required for a working testnet. Yet, it's not practical
to update n softwares for different parties, so we may re-assess this decision in
the next release planning.


2. Old address scheme

We will have to support this probably forever, in some forms. It may not be as
featureful as the new scheme and is out-of-scope for the summit testnet. We
should still consider making the wallet core code fairly agnostic to the
address scheme being used.


3. Externally-owned wallets

Implementation not yet finalized on the legacy code, out-of-scope for the
incoming delegation, can be added later.


4. Accounts manipulation

With BIP-44, accounts are implicitely managed, and we can't just create them
out of the void. Plus, it might be worthwhile considering balances for each
account we know of (linear scanning of the current UTxO), but it's probably
overkill to also do the transaction management at the account-level)


5. Access of a single address

Instead, we'll provide the frontend with a JavaScript library that performs
address validation.


6. Ada Redemption

Move to dedicated certificate redemption standalone software.


### API Design

https://rebilly.github.io/ReDoc/?url=https://gist.githubusercontent.com/KtorZ/3d78edd0b9a333ad589e9967da55f5da/raw/33314a3697e6a168108cced78369402391ff3945/.yaml

### Setup CI

| Priority  | Estimate |
| ---       | ---      |
| Very High | 10       |


- [#6](https://github.com/input-output-hk/cardano-wallet/issues/6) Rename & archive 'cardano-wallet' & create a new one  --> 1
- [#1](https://github.com/input-output-hk/cardano-wallet/issues/1) Review the .travis.yaml file from `cardano-wallet` (+ connect travis to new repository) --> 3
- [#4](https://github.com/input-output-hk/cardano-wallet/issues/4) Setup repository structure (test folders, src folder, stylish-haskell, .gitignore etc..) --> 2
- [#5](https://github.com/input-output-hk/cardano-wallet/issues/5) Add build instructions for building + caching Rust http-bridge to .travis.yml --> 3
- [#2](https://github.com/input-output-hk/cardano-wallet/issues/2) Add branch protections in GitHub & team permissions --> 1


### Receive And Process Blocks

| Priority | Estimate |
| ---      | ---      |
| High     | 13       |

- [#12](https://github.com/input-output-hk/cardano-wallet/issues/12)
  Implement (part-of) http-bridge API (get network tip, get block) --> 5
- [#11](https://github.com/input-output-hk/cardano-wallet/issues/11)
  Implement necessary CBOR decoders (block and block header) --> 3
- [#3](https://github.com/input-output-hk/cardano-wallet/issues/3)
  Ticking function to retrieve current tip and get next block (no handling of rollbacks) --> 5


### Launcher

| Priority | Estimate |
| ---      | ---      |
| High     | 8        |

- [#7](https://github.com/input-output-hk/cardano-wallet/issues/7)
  Define a small launcher CLI with a single 'start' command (--node-port, --wallet-server-port, --network) --> 5
- [#8](https://github.com/input-output-hk/cardano-wallet/issues/8)
  Spawn an Http Bridge and a wallet server from specified options, crash launcher if one of the two services die -> 3


### Create a Wallet

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

NOTE: No persistence layer yet.

- [#16](https://github.com/input-output-hk/cardano-wallet/issues/16)
  Porting and cleaning up Cardano.Mnemonic module (incl. corresponding tests)
- [#14](https://github.com/input-output-hk/cardano-wallet/issues/14)
  Extend the Cardano.Mnemonic to support recovery passphrase
- [#20](https://github.com/input-output-hk/cardano-wallet/issues/20)
  Define type primitives needed to represent a wallet (Tx, UTxO, Address etc..). A wallet has a name, a root private key, a state etc.. (cf the API specifications)
- [#21](https://github.com/input-output-hk/cardano-wallet/issues/21)
  Port and review the Ed25519 module from cardano-wallet to define derivation primitives needed for BIP-44 addressing
- [#22](https://github.com/input-output-hk/cardano-wallet/issues/22)
  Port and review the Address discovery (AddressPool + AddressPoolGap modules) from cardano-wallet
- [#23](https://github.com/input-output-hk/cardano-wallet/issues/23)
  Sketch wallet layer to fetch and create a wallet
- Sketch API Layer and servant server to serve that API, using the wallet layer to implement its handlers
- Sketch first wallet CLI, using the wallet layer to implement the various commands

### Keep Track of available UTxO and Balance

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

- Maintain a wallet's corresponding UTxO (prefiltering + address discovery)
- _Naively_ (no balance caching) compute to the total balance through maintained UTxO


### Submit Transactions

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

- Perform Coin Selection on the available UTxO
- Adjust selection to cover for network fees
- Generate or Use change address(es)
- Sign transactions with corresponding private keys
- No more grouping policy
- Funds are taken within the wallet (regardless of the account structure)
- A transaction may have the following metadata:
    - A unique identifier
    - A depth (a.k.a "number of confirmations")
    - A status: pending vs in-ledger vs invalidated (instead of: "applying", "inNewestBlocks", "persisted", "wontApply", "creating")
    - A total amount
    - A direction (outgoing vs incoming)
    - A timestamp
    - Absolute (from genesis) slot number & block number the transaction was inserted


### Handle Rollbacks

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

- Handle rollbacks at the networking layer / db storage
- Handle rollbacks within the wallet (cf Checkpoints)

### List Available Staking Pools

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

- A pool boils down to a (metadata --> 3-to-4-letter identifier) + public key
- Having more metadata --> TODO ADD LINK TO JSON SCHEMA + Discussion with Vincent
- We may associate the following metrics to a pool:
    - Controlled stake (or % of distribution owned, approximately)
    - number of slots successfully handled in the previous epoch


### Join / Quit Staking Pools

| Priority | Estimate |
| ---      | ---      |
| High     | ?        |

- Certificate keys derived from the wallet's private key (can use a level of
  the BIP-44 derivation --> change level)
- Publish certificate to the blockchain
- Craft transaction using a staking key to register to a pool
- Check Pool Status and sanity check to prevent user from making mistakes
- Make Sure The Staking Status is available in the wallet representation


### Estimate Transactions' Fee

| Priority | Estimate |
| ---      | ---      |
| Medium   | ?        |

- Should be very similar to transaction submission in essence: coin selection,
  adjust to cover for minimal fee, return actual estimated fee.


### Restore Historical Data

| Priority | Estimate |
| ---      | ---      |
| Medium   | ?        |

- Consider making this the "only" behavior of the wallet (instead of distinguishing between creation vs restoration)
- Restore from a list of mnemonic + recovery passphrase
- Focus on linear scanning of the blockchain, leave out optimization related to sequential derivation
- Non-interruptible process
- Aim at less than 5 minutes restoration time against mainnet


### List Available Addresses

| Priority | Estimate |
| ---      | ---      |
| Medium   | ?        |


- In BIP-44, addresses aren't created by clients, but are rather sequentially discovered
- This list can be known at any time
- Addresses may have the following metadata:
    - is used
    - ~~is change address~~ --> not used and incorrect in the context of multi-account. Removed in 1.6 already.
    - ~~ownership~~ --> already ambiguous and controversial in the current implementation. Not used.


### Make Protocol Settings Available

| Priority | Estimate |
| ---      | ---      |
| Low      | ?        |

- Current Slot Id, Slot Duration, Slot Count, security parameter, fee policy, maxTxSize
- Can be hard-coded for now or retrieve from a config file, before implementing Ledger rules
- Can be simply "mocked" in the meantime


### Make Software Information Available

| Priority | Estimate |
| ---      | ---      |
| Low      | ?        |


- Useful in the long-run, pretty useless for the testnet summit release
- Software Information, Git Revision
- Can be simply "mocked" in the meantime


### Make Node / Blockchain Status Available

| Priority | Estimate |
| ---      | ---      |
| Low      | ?        |

- Sync progress, blockchain height, localBlockchainHeight, localTimeInformation, subscriptionStatus
- Can be simply "mocked" in the meantime


### Allow Wallets to be Deleted

| Priority | Estimate |
| ---      | ---      |
| Low      | ?        |

- Remove a wallet, remove its corresponding keys
- Cleanup metadata, addresses and all related DB entries


## Annexes

### Reminder: What is a _Wallet_?

Cardano Wallets are represented by a cryptographic master private key which
allows deterministic and sequential derivation of child keys through
cryptographic computations.

The master private key can be derived from a list of mnemonic words (see
[BIP-0039][BIP-0039]) and a password. New keys can be derived from the master
key forming a hierarchical tree structure of related keys (see
[BIP-0032][BIP-0032]). That tree structure is layered in various _paths_ with
particular purpose (see [BIP-0044][BIP-0044]).

To every key, one can associate a corresponding Cardano address. Consequently,
keys can be used to verify whether an address _belongs_ to the wallet (as in,
comes from a key that can be derived from the master key).


### Available Rust Primitives

We could leverage some of the rust crypto primitives for a bunch of operation,
using Haskell's FFI with C bindings. Here's a list of the available primitives
in Rust:

https://github.com/input-output-hk/rust-cardano/blob/master/cardano-c/cardano.h


### New Addresses In Shelley Era

Rust nodes will already be using a new addresses format which Shelley will also
use.  The specification for this address format is available here:

https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md


### HTTP Bridge

In the early phase, as an alternative to using a trusted Haskell node to
retrieve blocks through the diffusion layer, we could rely on the existing Rust
HTTP-Bridge which provides some useful API endpoints to efficiently retrieve
blocks (and epochs) from a Cardano network or submit transactions to it:

https://github.com/input-output-hk/cardano-http-bridge

For example:

- `GET /:network/block/:blockid`
- `GET /:network/tip`
- `POST: /:network/txs/signed`

Later, when ready, we can switch over to use the Rust node API using a new
block format. But it allows us for an easy testing in the early phase.


### Prototype

https://github.com/KtorZ/wallet-prototype
