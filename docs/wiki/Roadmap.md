### :heavy_check_mark: [Setup New `cardano-wallet` Repostiory & CI](https://github.com/input-output-hk/cardano-wallet/milestone/1)

### :heavy_check_mark: [Receive And Process Blocks (via `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/2)

### :heavy_check_mark: [Basic Launcher](https://github.com/input-output-hk/cardano-wallet/milestone/3)

### :heavy_check_mark: [Support Wallet Creation](https://github.com/input-output-hk/cardano-wallet/milestone/4)

### :hammer: [Wallet Layer Integration (against `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/5)

### :hammer: [Transaction creation, submission & Coin Selection](https://github.com/input-output-hk/cardano-wallet/milestone/6)

### :hammer: [Initial Wallet Backend Server & Corresponding CLI](https://github.com/input-output-hk/cardano-wallet/milestone/7)

---

_items below are more-or-less prioritized_ 

---

### Restore Historical Data

- Consider making this the "only" behavior of the wallet (instead of distinguishing between creation vs restoration)
- Restore from a list of mnemonic + recovery passphrase
- Focus on linear scanning of the blockchain, leave out optimization related to sequential derivation
- Non-interruptible process
- Aim at less than 5 minutes restoration time against mainnet


### List Available Addresses

- In BIP-44, addresses aren't created by clients, but are rather sequentially discovered
- This list can be known at any time
- Addresses may have the following metadata:
    - is used
    - ~~is change address~~ --> not used and incorrect in the context of multi-account. Removed in 1.6 already.
    - ~~ownership~~ --> already ambiguous and controversial in the current implementation. Not used.


### Logging and monitoring

- Choose a logging library ([iohk-monitoring-framework](https://input-output-hk.github.io/iohk-monitoring-framework/pres-20181204/html/index.html) seems like the logical choice).
- Set up the logger using a very basic fixed config that writes to
  stdout/stderr (can be improved upon later).
- Adjust program structure so that a logging object can be passed
  throughout the program.
- Add debug, info and error logging at appropriate points in program,
  remove `putStrLn`.
- Add time observations of various important actions. For example: node backend calls, database queries, wallet restore, block application.
- Add space observations of important objects. For example: database size (records), UTxO distribution, number of addresses, number of transactions.


### Benchmarking (space & time)

- Find a way of running a wallet in a unit test with generated data (e.g. transactions).
- Make data generators which set up transactions for wallets of various sizes.
- Use the [weigh package](https://www.fpcomplete.com/blog/2016/05/weigh-package) to measure various test cases.
- Examples of test cases would be: receiving blocks from backend, wallet restoration, listing transactions.
- Make sure benchmarks are run in CI, at least nightly.


### Random Derivation Support

- Finalize port of derivation for random addresses
- Finalize port of discovery for random addresses
- Extend API to support random wallet

### :hammer: [Implement Wallet Backend Server & Corresponding CLI](https://github.com/input-output-hk/cardano-wallet/milestone/7)

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


### Estimate Transactions' Fee

- Should be very similar to transaction submission in essence: coin selection,
  adjust to cover for minimal fee, return actual estimated fee.


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


### Ada Redemption External Tools

- Dedicated certificate redemption standalone software 
- Construct a redemption transaction and submit it to the chain
- ADA are tracked in the wallet backend afterward, like any other blocks


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
