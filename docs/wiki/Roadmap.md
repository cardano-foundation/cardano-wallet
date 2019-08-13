> :information_source: How To Read This Document :information_source:
> 
> Sections below describe user stories in a non-technical way, from a business
> perspective. We aim at capturing user requirements that are non technical but
> reflects on actual capabilities or behaviours that are expected from the
> system. Each section is structured to answer the four questions below:
>
> **When:**
>   - What is the priority. How important is that story compared to another. 
>
> **Who:**   
>   - Who's the target / protagonist of that story. To whom does it benefit.
> 
> **What:**
>   - What is the story about, in terms of user experience and expected behaviour.
> 
> **Why:**
>   - What business value does this have. Why is it something relevant.

--- 

## Overview

#### Daedalus

- [Legacy / Existing Wallet Support](#gift-legacy--existing-wallet-support)
- [Transaction Input Resolution](#question-transaction-input-resolution)
- [Wallet Backend on Windows](#gift-wallet-backend-on-windows)
- [Wallet Backend on OSX](#gift-wallet-backend-on-osx)
- [Pagination Of Transaction History](#hammer-pagination-of-transaction-history)

#### Byron Rewrite

- [Haskell `cardano-node` networking integration](#gift-haskell-cardano-node-networking-integration)
- [Make Protocol Parameters Available to Clients](#gift-make-protocol-parameters-available-to-clients)
- [Support Roll-backs](#gift-support-roll-backs)
- [Automatic Software Update](#question-automatic-software-updates)
- TODO: Keep Track of Protocol Parameters

#### Shelley Phase

- [Join Stake Pools](#gift-join-stake-pools)
- [Quit Stake Pools](#hammer-quit-stake-pools)
- [List Stake Pools](#hammer-listing-available-stake-pools)
- [Fine Grained Transaction Manipulation](#question-fine-grained-transaction-manipulation)

#### Miscellaneous

- [Friendlier Command-Line](#gift-friendlier-command-line)
- [Enforce Stronger Master Passphrases](#gift-enforce-stronger-master-passphrases)
- [Better Restoration Benchmark](#gift-better-restoration-stress-benchmark)
- [Better Supervision Of Child Processes](#gift-better-supervision-of-launchers-child-processes)

## :hammer: Legacy / Existing Wallet Support

**When:** 
  - High Priority
  - Byron rewrite

**Who:** 
  - Daedalus 

**What:** 
  - Be able to keep monitoring their wallet balance and make transactions to
    addresses as they always used to. A wallet software must provide the
    following capabilities:

    - Restore an existing wallet from current mainnet from a 12-word mnemonic sentence
    - Monitor a wallet available and pending balances
    - List historical transactions of that particular wallet
    - Perform transactions to any addresses of the Cardano blockchain
    - Create new deposit addresses 
    - List existing known addresses of one's wallet, and tell whether each address
      is used or not.

**Why:**
  - Some level of compatibility with the current software is necessary to 
    maintain the continuity between the old and new versions. Current users
    except only minimal disruptions of their service as we add new features
    to the software.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

--- 

## :gift: (Haskell) `cardano-node` networking integration

**When:** 
  - High Priority
  - Byron rewrite

**Who:**
  - High-level client applications using `cardano-node`  

**What:** 
  - Connect the wallet backend software to a `cardano-node` via the newly
    designed mini-protocols (in particular, the "chain sync" and "transaction
    submission" protocols). We want to illustrate the good functioning of 
    the networking interface provided by the new Haskell nodes showing that
    blocks can be downloaded to follow a blockchain maintained by a network
    of Haskell `cardano-node`s.

Why:
  - The wallet backend is the interface sitting between many client
    applications and the cardano-node. This avoid a low-level integration for
    client applications that want to deal with a Cardano blockchain but have
    little knowledge of or desire to implement the low-level mini-protocols.
    Therefore, a first step towards a fully integrated wallet backend with the
    ability to retrieve blocks from a network full of `cardano-node`. Later,
    additional functionalities can be added on top of this networking interface
    to provide high-level features to client applications.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

--- 

## :question: Transaction Input Resolution

**When:** 
  - Low Priority ?
  - ?

**Who:** 
  - Daedalus 

**What:**
  - Cardano is a UTxO-based blockchain. Therefore, transactions represents a mapping
    between inputs and outputs, 

    where **inputs** point to:

      - A previous transaction id
      - An output number of that transaction

    and **outputs** point to:

      - An address
      - An amount

    In practice, users want to know the `address` and `amount` corresponding to a 
    particular transaction id and output number. We call this a "resolved input" as 
    it requires to following a chain of transaction in order to resolve the actual 
    values pointed by a transaction id and output number. 

**Why:**
  - Users think of transactions as funds moving between two addresses, very much like
    it works in classic accounting. It makes mental representation easier.
  - Having this done by the wallet backend makes the deployment and service management
    easier, although in practice, we could imagine different solutions like a local
    or remote explorer service, or simply a different UI (with deeplinks to an external
    service for instance).

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :gift: Wallet Backend on Windows

**When:**
  - Low priority ?
  - Byron rewrite

**Who:**
  - Daedalus 

**What:**
  - The wallet backend is a crucial piece of software in the Daedalus' pipeline which 
    runs on the same host-machine as the frontend part that is Daedalus. Users running
    on Windows therefore need the ability to run the wallet backend on their host system
    (a.k.a Windows).

**Why:**
  - Many (most) users of Daedalus are running on Windows. Not being to run on
    Windows means the inability for Windows users to run a local version of the
    wallet software on their machine. This would be an important disruption of
    the existing service. 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :gift: Wallet Backend on OSX

**When:**
  - Low priority ?
  - Byron rewrite

**Who:**
  - Daedalus 

**What:**
  - The wallet backend is a crucial piece of software in the Daedalus' pipeline which 
    runs on the same host-machine as the frontend part that is Daedalus. Users running
    on OSX therefore need the ability to run the wallet backend on their host system.

**Why:**
  - Many users of Daedalus are running on OSX. Not being to run on OSX means
    the inability for OSX users to run a local version of the wallet software
    on their machine. This would be an important disruption of the existing
    service. 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

--- 

## :gift: Make protocol parameters available to clients

**When:**
  - Low Priority
  - Byron rewrite

**Who:**
  - Daedalus
  - High-level client applications 

**What:**
  - The Cardano blockchain has a few parameters that are relevant for running
    the consensus protocol, but also, for client applications dealing with the
    blockchain. High-level clients like Daedalus are interested in consuming
    these pieces of information directly from the wallet backend which should 
    keep track of them and provide them on demand. In particular, Daedalus wants to know:

    - The current tip of the chain
    - The current blockchain height (in number of slots)
    - The "local" blockchain height (in number of slots). Said differently, 
      the syncing status of the wallet software.
    - The NTP drift of a local core node, in milliseconds, if available
    - The software update availability

**Why:**
  - Depending on the client, some have an informative value, and some others
    are necessary for the good functioning of the application. The wallet
    backend already keeps track of a few of these details for its own. Beside,
    keeping track of these requires a client to comprehend update proposals and
    the initial genesis blocks. High-level client applications do not interact
    directly with the network but prefer consuming these pieces of information
    from the wallet backend.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :gift: Support Roll-backs

**When:**
  - Medium Priority
  - Byron rewrite ?

**Who:**
  - Any client application using the wallet backend

**What:**
  - In a standard setup, it is very likely for a core node to roll-back (i.e.
    rewind the chain to a previous point in time). This means that the wallet
    backend must be able to correctly keep track of blocks, rolling backwards
    when needed and not only append blocks to an existing chain. Doing so,
    transactions that could have been inserted in recent blocks might become
    pending again, and balances may change to reflect the chain after a
    rollback.  Beside, we do not roll back to have a significant impact on the
    software usability. Hence, most features should remain available during
    rollbacks. Submitting transactions may be however forbidden during
    rollbacks.

**Why:**
  - Although this doesn't occur in an perfect BFT setup where the network
    connectivity between nodes would be 100% reliable, it can in practice
    happens very rarely. In addition, rollbacks are a mechanism very frequent
    in the Ouroboros-praos protocol which core nodes will implement soon. Not
    handling rollbacks would force any user to purge the wallet database and
    restart the software on every rollbacks. While this maybe _acceptable_ in a
    federated era where all nodes are very much in our control, this is no
    longer a possibility with a fully decentralized network, especially when
    running Ouroboros-praos which may require rolling back every few seconds.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :question: Automatic Software Updates

**When:**
  - ??

**Who:**
  - Daedalus
  - Non-technical users of the wallet backend

**What:**
  - In order update the wallet backend software, one currently has to either 
    compile it from the sources, or, download a pre-compiled binary artefact 
    from our release page. Instead, the wallet backend could receive updates
    directly from the network and download its own new version automatically.

**Why:**
  - Having automatic software updates moves the update logic and complexity
    from the users to the software itself. It is however unclear that the 
    wallet backend would much benefit from this approach since it is now an
    external

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

--- 

## :gift: Friendlier Command-Line

**When:**
  - Low Priority

**Who:** 
  - Users of the wallet backend command-line
  - Maintainers of the wallet backend

**What:**
  - While using the wallet backend command-line, we've observed a few points
    that could be improved and made a bit more user-friendly:
    - Prompt user again when passphrases don't match (instead of failing)
    - Handshake CLI and Server to avoid sending request to another service by accident
    - Find a better name for the command `transaction create` (which does
      construction, signing and building)
    - Improve error message when failing to parse a transaction amount
    - Allow specifying units for coin values like `20lovelace` or `20ada`

**Why:**
  - The command-line interface is human-facing and should help both users and
    developers to avoid mistakes and better understand errors. A nice and
    polish software leads to less frustration and prevent from time loss
    tracking down an issue because of a poor error reporting.
  
<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

--- 

## :gift: Enforce Stronger Master Passphrases

**When:**
  - Low Priority

**Who:**
  - Users of the wallet backend (API & CLI)

**What:**
  - The wallet backend only enforces a particular minimal size for wallet
    master passphrases. Instead, we would rather enforce strong and secure
    passphrases following advices from the OWASP security guides, using
    battle-tested algorithms like `zxcvbn` and a blacklist of popular
    known passphrases. 

**Why:**
  - Users have a tendency to use very weak passphrases as their sole
    protection.  Having the system requiring stronger passphrases forces
    user to raise a bit their protection level. 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :gift: Join Stake Pools

**When:**
  - Low Priority
  - Shelley

**Who:**
  - Client applications who wants to implement delegation features

**What:**
  - It should be possible for a wallet owner to delegate their funds to a
    particular stake pool. We only consider full delegation / all-or-nothing
    delegation at this stage. It's also not question about incentive or
    rewards but boils down to providing capabilities to create, sign and 
    register delegation certificates to a compatible network.
  - A user wants to know whether one of his wallet, once restored, is delegating
    funds to a pool or not.

**Why:**
  - Delegation is a central part of the decentralization era. "Standard users"
    can't be expected to run their own pool and setup a full node for this
    requires a specific technical expertise, resources and time. Instead, users
    want the ability to delegate their fund to an existing operator running a
    node.
  - Having this done by the wallet backend seems reasonable since the wallet
    already manages users' assets and have access to cryptographic materials
    necessary to register to a stake pool.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :hammer: Quit Stake Pools

**When:**
  - Low Priority
  - Shelley

**Who:**
  - Client applications who wants to implement delegation features

**What:**
  - Users that are delegating should have the ability to stop delegating. 
    From the network point of view, there's no such thing as "quitting" a 
    stake pool (to be confirmed?), but instead, users are free to delegate
    to another party, might it be themselves. 
  - TODO: story to clarify, it is unclear what's the expected feature

**Why:**
  - It should be as easy for users to stop participating into the delegation
    protocol than it is for them to join in order to not "lock them up". 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :hammer: Listing Available Stake Pools

**When:**
  - Low Priority
  - Shelley

**Who:**
  - Client applications who wants to implement delegation features

**What:** 
  - Users should be able to see which stake pools are currently registered on the 
    network and have some metadata and metrics about these pools. In particular, we're
    interested in knowing:
      - A 3-4 (non-unique) letter identifier 
      - The (approximate) controlled stake 
      - The number of slots successfully handled in the previous epoch
      - TODO: To clarify, a comparison _performance_ metric 
  
**Why:**
  - Since a wallet would be providing ways to participate in the delegation, it
    makes sense to locate associated feature quite nearby. This allows client
    applications to build a more complete user interface using only the wallet
    backend as a source of information.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>

---

## :gift: Better Restoration Stress Benchmark

**When:**
  - Low Priority
  - Byron Rewrite

**Who:**
  - Maintainers of the wallet backend

**What:**
  - Part of our nightly builds include restoring wallets on current testnet and
    mainnet networks (powered by `cardano-http-bridge`). Yet, Existing chain
    data doesn't necessarily include "extreme" cases that might occur in the
    future. Therefore, we could make data generators to set up transactions for 
    wallets of various sizes.

  - Also, we currently only observe the heap usage of the software and export a 
    visual artifact of an overall heap usage. Instead, we could use dedicated 
    packages (`weigh`) to have more fine-grained measures and also, be able to 
    construct automatic checks comparing against a baseline threshold. 

**Why:**
  - Benchmarks provide some necessary results to assess to good functioning of the 
    software in real-life scenarios. 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>


---

## :gift: Better Supervision Of Launcher's Child Processes

**When:**
  - Low Priority
  - Byron Rewrite

**Who:**
  - Maintainers of the wallet backend
  - Unix users of the wallet backend

**What:**
  - The `launch` command of the command-line currently spawns two child processes:
    - An actual wallet server
    - A corresponding backend node to talk to Yet, when the parent process is
      terminate via a SIGKILL signal on UNIX, child processes aren't killed as
      they're when using a SIGINT (ctrl-c). Ideally, we do want to properly
      clean-up after the process receives a SIGKILL signal from the operating
      system. More details on [#75](https://github.com/input-output-hk/cardano-wallet/pull/75) and [#77](https://github.com/input-output-hk/cardano-wallet/pull/77)

**Why:**
  - Unix users expect every sub-processes started by the system to be
    cleaned-up when terminating a parent process. There's no way for a
    particular application to "handle" a SIGKILL for it's the semantic of that
    signal, but we can rely on OS-level functionality to make sure resources
    are released properly and on-time.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>


---

## :question: Fine Grained Transaction Manipulation

**When:**
  - Low Priority
  - Byron Rewrite

**Who:**
  - Client applications doing key management themselves (e.g. implementing Ledger or Trezor support)

**What:**
  - The wallet backend does input selection, transaction signing and
    transaction submission all in one-step. Users of the API should have the
    ability to do any of these three steps independently (while still having
    the ability to let the wallet backend do them itself as it does currently).

  - Submitting a transaction shouldn't require any access to the wallet's root
    private key.

**Why:**
  - Some users (e.g. big exchanges) do key management on their end and prefer
    not to share the sensitive cryptographic materials with a third-party
    software. This would also allow client applications to offload key
    management to hardware wallets components like Ledger or Trezor.

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>


---

## :hammer: Pagination Of Transaction History 

**When:**
  - Low Priority
  - Byron Rewrite

**Who:**
  - Client applications 

**What:**
  - The wallet backend provides the ability to fetch historical transactions
    of a particular wallet. Users can order transactions and filter them
    according to date ranges. Yet, it'd be ideal for a client application
    to be able to browse a collection of transactions reliably by batches
    of a fixed size. 
  - TODO: What sort of interface does a client expect? 
  - TODO: Is there really a need for this?

**Why:**
  - Using only ranges can be tricky for a client to build a reliable and 
    efficient UI for showing a user all its transactions. Instead, having
    a well-paginated system which makes fetching the history easy helps
    building a much better user experience. 

<p align=right><a href="#overview">top :arrow_heading_up:</a></p>


---

> **Legend**
>
> - :gift:: Confirmed story
> - :question:: Unconfirmed story
> - :hammer:: Partial story, more details are needed