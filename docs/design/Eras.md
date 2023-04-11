# Cardano Eras

A blockchain "hard fork" is a change to the block producing software such that any new blocks produced would be invalid according to the old unchanged software. Therefore, the unchanged software would not accept these blocks into its chain. If nodes running the new and modified software continue to produce blocks on top of their chain, a "fork" will result, starting at the parent block of the first incompatible block.

Cardano manages hard forks by gating the new block generation code behind a feature flag. The network agrees upon a slot at which all nodes will enable the new block generation code. After that slot, the network is said to be operating in a new "era."

Agreement on the slot number (epoch really) to switch to the new era happens by an on-chain voting protocol. If the voting proposal is carried, operators of nodes must ensure that their software version will be compatible with the new era, before the hard fork occurs.

::: {.gotcha}
#### Terminology

One might ask why the eras listed in [Cardano.Api](https://input-output-hk.github.io/cardano-node/cardano-api/lib/Cardano-Api.html) don't all appear in the [Cardano Roadmap](https://roadmap.cardano.org/en/).

The roadmap is actually divided into _phases_, under which one or more eras (hard forks) may occur.
:::

## The Hard Fork Combinator

The mechanism whereby the Cardano Node operates in different modes - depending on which slot it is up to - is called the "Hard Fork Combinator".

The following [seminars](https://input-output.atlassian.net/wiki/spaces/EN/pages/718962750/IOHK+Research+Engineering+Seminar) (company internal use only) are a good introduction:

| Date | Title | Description |
| --- | --- | --- |
| 2020/05/15 | [Hardfork Combinator I: Reconciling The Eras](https://drive.google.com/file/d/1m_jKQM_gxBm0ctLqIq9NGj5_nXPI66Su/view) | This session will explain the technology behind the fabled hardfork combinator for Cardano. As this is quite a bit of machinery, and central to Shelley, we will take the time to talk about it in two sessions, to give enough background that everyone can follow along. This first session will focus on the combinator itself, and how advanced Haskell features allow us to travel from one typed world to another. In a future session, which yet needs to be scheduled, he will present on the subtleties of treating time in the presence of a hardfork. |
| 2020/06/05 | [Hard Fork Combinator II: Time for a Hard Fork](https://drive.google.com/file/d/1QIJ-VBlj-txB6K6E7DIEnY5TzaD89qQm/view) | As any computer scientist, physicist, or cross-timezone remote worker can testify, time is a tricky and unforgiving beast. Cardano, as a slot-based Proof of Stake system, is inherently time-driven, so it is vital to get time right. Across the hardfork from Byron to Shelley, the way in which time is divided into slots will change, which turned out to be a massive complicating factor. In this presentation, Edsko will show us how the hard fork combinator handles time across the fork point. |

In a nutshell, pretty much all types in `Cardano.Api` are indexed by the `Era`. The node will provide a `TimeInterpreter` object by the `LocalStateQuery` protocol, which allows `cardano-wallet` to calculate what time a certain slot occurs at. Calculating the time of a given slot number is more complicated than it sounds.

## Configuration

Both `cardano-node` and `cardano-wallet` have command-line parameters and/or configuration options for the hard-fork combinator.

### Genesis

Nodes must be initialised with a genesis config. The genesis config defines the initial state of the chain. There is the Byron era genesis obviously, and some subsequent eras (Shelley and Alonzo, at the time of writing) also have their own genesis.

Only the hash of a genesis config is stored on-chain. For mainnet, the genesis hash is used as the previous block header hash for the _Epoch Boundary Block_ of epoch 0.

When `cardano-wallet` connects to its local `cardano-node`, it must provide the Byron genesis config hash. Hashes for subsequent genesis configs are contained on-chain within the voting system update proposals.

For _mainnet_, we have hardcoded the genesis hash. It will never change. For _testnet_, users must supply a genesis config to the [[cli]], so that its hash may be used when connecting to the local node.

::: {.gotcha}
#### Epoch Boundary Blocks

Epoch Boundary Blocks (EBBs) only existed in the Byron era, prior to their abolition with the Shelley hard fork. Confusingly, they were also referred to as "genesis" blocks. The EBB is the previous block of the first block in a Byron epoch. EBBs were calculated locally on each full node of mainnet, and were never transmitted across the network between nodes.
:::

### _Instafork_™ Technology

Hard forking to Alonzo (the latest era, at the time of writing) by natural methods requires several epochs' worth of network operation time to take effect. For disposable local testnets, this would delay startup and make our integration tests slower.

Therefore, `cardano-node` can be configured to hard fork itself at the start of any given epoch number, without an update proposal. It's also possible to hard fork directly into all the eras at epoch 0. This is what we use for the integration tests `local-cluster`. The `cardano-node` configuration for this is:

```yaml
TestShelleyHardForkAtEpoch: 0
TestAllegraHardForkAtEpoch: 0
TestMaryHardForkAtEpoch: 0
TestAlonzoHardForkAtEpoch: 0
```

### Historical note: "Cardano Mode"

In theory, it is possible to run `cardano-node` in a single-era mode, where the Hard Fork Combinator is completely disabled. This is called running in Byron-only or Shelley-only mode. Then there is full "Cardano Mode", which enables the Hard Fork Combinator, thereby allowing the software to pass through eras.

For `cardano-cli`, there are `--byron-mode`, `--shelley-mode`, and `--cardano-mode` switches, to tell the network client which mode the local node is using.

Cardano Mode is the default. I'm not aware of too many people using single-era modes for anything much.

## Specifications

Formal specifications (and the actual implementations!) for each Cardano era exist in the [input-output-hk/cardano-ledger](https://github.com/input-output-hk/cardano-ledger) repo. These documents are surprisingly readable, and are our go-to source for answers about how the node operates. See the [`README.md`](https://github.com/input-output-hk/cardano-ledger/blob/master/README.md) for links to PDFs and/or build instructions.

### Decentralized Update Proposals

The [input-output-hk/decentralized-software-updates](https://github.com/input-output-hk/decentralized-software-updates) repo contains research materials for a _future_ implementation of decentralized software updates.
