### :heavy_check_mark: [Setup New `cardano-wallet` Repository & CI](https://github.com/input-output-hk/cardano-wallet/milestone/1)

### :heavy_check_mark: [Receive And Process Blocks (via `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/2)

### :heavy_check_mark: [Basic Launcher](https://github.com/input-output-hk/cardano-wallet/milestone/3)

### :heavy_check_mark: [Support Wallet Creation](https://github.com/input-output-hk/cardano-wallet/milestone/4)

### :heavy_check_mark: [Wallet Layer Integration (against `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/5)

### :heavy_check_mark: [Restore Historical Data](https://github.com/input-output-hk/cardano-wallet/milestone/7)

### :heavy_check_mark: [Benchmarking & Nightly Builds](https://github.com/input-output-hk/cardano-wallet/milestone/7)

### :heavy_check_mark: [Fee Calculation](https://github.com/input-output-hk/cardano-wallet/milestone/6)

### :heavy_check_mark: [Transaction creation, submission & Coin Selection](https://github.com/input-output-hk/cardano-wallet/milestone/6)

### :heavy_check_mark: [Initial Wallet Backend Server & Corresponding CLI](https://github.com/input-output-hk/cardano-wallet/milestone/7)

### :heavy_check_mark: : [Integrate node.js IPC listener in the launcher](https://github.com/input-output-hk/cardano-wallet/milestone/8)

### :heavy_check_mark: : [SQLite implementation for the database layer](https://github.com/input-output-hk/cardano-wallet/milestone/9)

### :heavy_check_mark: : [List Addresses](https://github.com/input-output-hk/cardano-wallet/milestone/16)

### :hammer: [Jörmungandr High-level integration](https://github.com/input-output-hk/cardano-wallet/milestone/10)

### :hammer: [Jörmungandr Integration Testing](https://github.com/input-output-hk/cardano-wallet/milestone/15)

### :hammer: [Logging](https://github.com/input-output-hk/cardano-wallet/milestone/14)

---

_items below are more-or-less prioritized_ 

---

### Better Coin Selection

The CS currently does this random + improvement procedure for all outputs, one output after the other. Instead, we could perform the random selection only for all outputs, *and then*, try to improve each output one after the other. This way, we can use our available inputs to cover requested outputs. And, once done, with the remaining available inputs, we try to improve each output one after the other. This should be much more resilient to multi-output transaction while still allowing a best-effort for cleaning up the UTxO as we make transactions. 

### Random Derivation Support

- Finalize port of derivation for random addresses
- Finalize port of discovery for random addresses
- Extend API to support random wallet

### Handle Rollbacks

- Handle rollbacks at the networking layer / db storage
- Handle rollbacks within the wallet (cf Checkpoints)

### List Available Staking Pools

- A pool boils down to a (metadata --> 3-to-4-letter identifier) + public key
- Having more metadata --> TODO ADD LINK TO JSON SCHEMA + Discussion with Vincent
- We may associate the following metrics to a pool:
    - Controlled stake (or % of distribution owned, approximately)
    - number of slots successfully handled in the previous epoch


### Join / Quit Staking Pools

- Certificate keys derived from the wallet's private key (can use a level of
  the BIP-44 derivation --> change level)
- Publish certificate to the blockchain
- Craft transaction using a staking key to register to a pool
- Check Pool Status and sanity check to prevent user from making mistakes
- Make Sure The Staking Status is available in the wallet representation

## Improvements to the CLI

A few ideas to make the CLI better. It'd be nice to already get feedback on the existing one and see what people external to the project would have to say. A few ideas already:

 - if the passphrases don't match - prompt user to put them again twice
 - make wallet backend server introduce itself in the HTTP header server (and make CLI to check for that value and put some warning/error if it don't match)
 - `create wallet` wizard to be a whole responsive/prompt CLI
 - Different levels of `--help`, e.g.: `cardano-wallet --help`, `cardano-wallet wallet --help` etc..
 - Improve error msg for `FromText AddressAmount`
 - Chose a better name for CLI option `transaction create`. Currently we use it to do all three steps: coin selection, sign, submit. See discussion https://github.com/input-output-hk/cardano-wallet/pull/225#discussion_r281454697
 - `Qualtity "lovelace" Natural` is used to parse number of lovelace/coins from CLI. This is defined in `FromText (Quantity sym b)` https://github.com/input-output-hk/cardano-wallet/pull/225/files/fff43a4e5a70ed93bf028217ebdc90429252be2d#diff-27d87fed0f151afbb3b4e829fb315ba3R107 . We might want to use more fine grained parser for coins and parse "20lovelace" and "20ada" differently (and do coin conversion autimatically) and default "20" to lovelace
 - Improve password validation in API and CLI which validate for strong passwords (passwords should have enough entropy)

### Make Protocol Settings Available

- Current Slot Id, Slot Duration, Slot Count, security parameter, fee policy, maxTxSize
- Can be hard-coded for now or retrieve from a config file, before implementing Ledger rules
- Can be simply "mocked" in the meantime


### Make Software Information Available

- Useful in the long-run, pretty useless for the testnet summit release
- Software Information, Git Revision
- Can be simply "mocked" in the meantime


### Make Node / Blockchain Status Available

- Sync progress, blockchain height, localBlockchainHeight, localTimeInformation, subscriptionStatus


### Updates

- Support for software update API (proxying to underlying node's API for updates, if any)


### Externally-owned wallets & Fine-grained transaction manipulation

- Manual coin selection
- Off-band signing
- Track of list of account or address public keys
- Manual change address selection

### Ada Redemption External Tools (?)

- Dedicated certificate redemption standalone software 
- Construct a redemption transaction and submit it to the chain
- ADA are tracked in the wallet backend afterward, like any other blocks

### Better Restoration Stress Benchmark

Existing chain data doesn't necessarily include "extreme" cases that might occur in the future

- Make data generators which set up transactions for wallets of various sizes.
- Use the [weigh](https://www.fpcomplete.com/blog/2016/05/weigh-package) package to measure and display the GHC heap usage of test scenarios.
- Figure out a way of generating semi-realistic transactions in blocks
- Use a mock network layer to feed generated blocks to wallet layer
- Set up a test case which checks heap usage after applying a certain number of transactions in a certain number of blocks.
- Also measure how long it took to apply those blocks/transactions.
- Automatically check benchmark results and compare them against some baseline with threshold


### Better supervision of child processes in launcher

Currently child processes (cardano-wallet-server and cardano-http-bridge) are
not terminated when the parent cardano-wallet-launcher is terminated via
SIGKILL signal (kill -9). We need to implement solution that will terminate the
child processes when the parent process is killed via kill -9. The idea is to
use shared socket. More details on [#75](https://github.com/input-output-hk/cardano-wallet/pull/75) 
& [#77](https://github.com/input-output-hk/cardano-wallet/pull/77)

---

## Annexes

<details>
  <summary><strong>Reminder: What is a _Wallet_?</strong></summary>

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
</details>

<details>
  <summary><strong>Available Rust Primitives</strong></summary>

  We could leverage some of the rust crypto primitives for a bunch of operation,
  using Haskell's FFI with C bindings. Here's a list of the available primitives
  in Rust:

  https://github.com/input-output-hk/rust-cardano/blob/master/cardano-c/cardano.h
</details>

<details>
  <summary><strong>New Addresses In Shelley Era</strong></summary>

  Rust nodes will already be using a new addresses format which Shelley will also
  use.  The specification for this address format is available here:

  https://github.com/input-output-hk/implementation-decisions/blob/master/text/0001-address.md
</details>

<details>
  <summary><strong>cardano-http-bridge</strong></summary>

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
</details>

<details>
  <summary><strong>prototype</strong></summary>

  https://github.com/KtorZ/wallet-prototype
</details>
