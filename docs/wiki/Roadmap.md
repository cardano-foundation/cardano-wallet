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

<details> 
  <summary>Forewords</summary>

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
</details>

### :heavy_check_mark: [Setup New `cardano-wallet` Repostiory & CI](https://github.com/input-output-hk/cardano-wallet/milestone/1)

### :heavy_check_mark: [Receive And Process Blocks (via `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/2)

### :heavy_check_mark: [Basic Launcher](https://github.com/input-output-hk/cardano-wallet/milestone/3)

### :heavy_check_mark: [Support Wallet Creation](https://github.com/input-output-hk/cardano-wallet/milestone/4)

### :hammer: [Wallet Layer Integration (against `cardano-http-bridge`)](https://github.com/input-output-hk/cardano-wallet/milestone/5)

---

### Debts

We have identified the following debts from the previous iteration, to be tackled in a single ticket estimated for 8 points (to be created --> Matthias)

#### Poor README

Our README is quite poor in information and it would be worth adding some
basics stuff to it. That's a point Johannes already raised on the legacy
repository actually. We could add at least:

- Basic Overview / Goal
- Build & Tests instructions
- A link to the wiki
- Link to the generated Haddock documentation
- Link to the API documentation


#### Areas missing testing:

| Module                                     | Comments                                                                                                                                                |
| ---                                        | ---                                                                                                                                                     |
| `Cardano.DBLayer` & `Cardano.DBLayer.MVar` | - Pretty much all function <br/> -`readCheckpoints` in particular _(tested manually during profiling)_                                                  |
| `Cardano.NetworkLayer`                     | -`listen` _(tested manually during profiling)_                                                                                                          |
| `Cardano.NetworkLayer.HttpBridge`          | - Error cases from `convertError`                                                                                                                       |
| `Cardano.Wallet`                           | - Tracking of pending transactions <br/> - Wallet metadata manipulation <br/> -`currentTip` _(tested manually during profiling)_                        |
| `Cardano.Wallet.AddressDerivation`         | - Overflow on address indexes                                                                                                                           |
| `Cardano.Wallet.AddressDiscovery`          | - Overflow on address indexes during pool extension <br/> - `IsOurs` & `isOurs`                                                                         |
| `Cardano.Wallet.Binary`                    | - Decoding EBB _(discarded by network layer)_ <br/> - Invalid constructors in block representations <br/> - Decoding redeem addresses & witnesses _(?)_ |
| `Cardano.Wallet.Binary.Packfile`           | - Some error paths (`VersionTooOld` & `BlobDecodeError` with unconsumed data)                                                                           |
| `Cardano.Wallet.Mnemonic`                  | - Error cases in `mkMnemonic` <br/> - Error cases in `genEntropy`                                                                                       |
| `Cardano.Wallet.Primitive`                 | - Arithmetic overflow on `SlotId`                                                                                                                       |


#### Unused code 

| Module                            | Comments                                                                     |
| ---                               | ---                                                                          |
| `Cardano.Wallet.AddressDiscovery` | - Manual semigroup instance on `AddressPool`                                 |
| `Cardano.WalletLayer`             | - Debug `printInfo` function in the implementation of `watchWallet`          |
| `Servant.Extra.ContentTypes`      | - `WithHash` & `Hash "Blockheader"` in the `Cardano.NetworkLayer.HttpBridge` |


### Submit Transactions

##### Overview 

- Perform Coin Selection on the available UTxO (with fee adjustment) (cf: [Kernel/CoinSelection](https://github.com/input-output-hk/cardano-wallet-legacy/tree/develop/src/Cardano/Wallet/Kernel/CoinSelection))
- Generate change address
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
- Keep in mind that in a near future, we do want to allow signing transaction
  off-band (by another software), so signing shouldn't be tightly coupled to
  transaction submission.

##### Tasks

| Description                                                          | Estimate | Created By |
| ---                                                                  | ---      | ---        |
| Model API Types for Transactions in Servant                          | 5        | Johannes   |
| Port coin selection and fee calculation from legacy (minus grouping) | 13       | Matthias   |
| Extend network layer to support transaction creation                 | 3        | Piotr      |
| Extend wallet layer to support transaction creation                  | 3        | Matthias   |
| Keep track of known transactions in the wallet primitive logic       | 5        | Pawel      |
| Monitor memory allocation and stress wallet primitives               | 5        | Rodney     |
| Compute metadata for transaction                                     | 5        | Ante       |

### Implement Wallet Backend Server & Corresponding CLI

##### Overview

- CLI to interact with the wallet layer from the terminal. The CLI acts as a proxy to the wallet
  backend server (and therefore, requires the wallet server to be up-and-running) such that
  every endpoint from the API has an equivalent command in the CLI (which delegates the logic 
  to the API via an HTTP call).

- Each command should output a corresponding JSON object (got from the API).

- We have a corresponding web-server that serve the various API endpoints,
  defaulting to an error 501 Not Implemented for endpoints that aren't yet
  implemented.

##### Tasks

In scope: 

- [Wallets List | `GET /wallets`](https://rebilly.github.io/ReDoc/?url=https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/specifications/api/swagger.yaml#operation/listWallets)
- [Wallets Create/Restore | `POST /wallets`](https://rebilly.github.io/ReDoc/?url=https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/specifications/api/swagger.yaml#operation/postWallet)
- [Wallets Get | `GET /wallets/{walletId}`](https://rebilly.github.io/ReDoc/?url=https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/specifications/api/swagger.yaml#operation/getWallet)
- [Transactions Create | `POST /wallets/{walletId}/transactions`](https://rebilly.github.io/ReDoc/?url=https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/specifications/api/swagger.yaml#operation/postTransaction)

| Description                                                           | Estimate | Created By |
| ---                                                                   | ---      | ---        |
| Finalize translation of Swagger API specification into Servant types  | 8        | -          |
| Automatically generate golden tests for ToJSON instances of API types | 3        | Johannes   |
| Implement API handlers for selected endpoints                         | 5        | Jonathan   |
| Extend cardano wallet CLI with selected commands                      | 3        | Matthias   |


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
- Can be simply "mocked" in the meantime


### Allow Wallets to be Deleted

- Remove a wallet, remove its corresponding keys
- Cleanup metadata, addresses and all related DB entries

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

### Benchmarking (space)

- Find a way of running a wallet in a unit test with generated data (e.g. transactions).
- Make data generators which set up transactions for wallets of various sizes.
- Use the [weigh package](https://www.fpcomplete.com/blog/2016/05/weigh-package) to measure various test cases.
- Examples of test cases would be: receiving blocks from backend, wallet restoration, listing transactions.
- Make sure benchmarks are run in CI, at least nightly.


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
