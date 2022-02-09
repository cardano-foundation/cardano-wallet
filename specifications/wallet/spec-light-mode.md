# Specification: Light mode for cardano-wallet

9th Feb 2022

Status: DRAFT

# Synopsis

This document specifies a light-mode for `cardano-wallet`.
This mode aims to make synchronisation to the blockchain faster by using a less trusted source for blockchain data.

Light wallets often employ this strategy of using a less trusted data source; for example the [Nami][] wallet uses [Blockfrost][] as data source. The purpose of the light-mode of `cardano-wallet` is to make this "trust vs speed" trade-off readily available to downstream software such as [Daedalus][] with *minimal* changes to its downstream API.

In addition, the "trust versus speed" trade-off will be partially obsoleted by [Mithril][] technology, which aims to give us "trust *and* speed" by providing verified ledger state data as opposed to block data. The act of implementing light-mode in `cardano-wallet` does not only offer immediate benefits, but will, in fact, partially prepare the codebase for Mithril technology.

  [daedalus]: https://daedaluswallet.io
  [nami]: https://namiwallet.io
  [blockfrost]: http://blockfrost.io
  [mithril]: https://iohk.io/en/research/library/papers/mithrilstake-based-threshold-multisignatures/

# Motivation

## Background

As the Cardano blockchain grows in size, retrieving and verifying blocks from the consensus network becomes increasingly time consuming. By making a "trust versus speed" trade-off, we can significantly decrease synchronization times.

## Benefits

* *Speed*. With light-mode, we expect synchronisation times of < 1 minute for a wallet with 1'000 transactions. In contrast, synchronisation of an empty wallet currently takes ~ 50 minutes by connecting to a node.
* *Compatiblity*. Light-mode is intended to preserve the API presented to downstream software such as [Daedalus][], only *minimal* changes are expected.
* *Optionality*. A wallet that was started in light-mode can be restarted in full mode without resynchronization, and vice versa. (MVP: no support yet for changing the mode dynamically while the wallet is running.)

## Limitations

* *Trust*. The external data source does *not* provide the same level of protection against *omission* of transactions as the Proof-of-Stake consensus protocol.
* *Privacy*. In addition to the reduction of trust in the blockchain data, we now also reveal private information about addresses belonging to a single wallet.
* *Address schemes*. Only *Shelley* wallets with *sequential address discovery* can be supported.

However, these limitations are shared by all existing light wallets. In fact, some light wallets only provide *single-address* wallets, which is an additional reduction of privacy.

# Specification

## Functionality

### Command line

The `cardano-wallet` executable will feature a new flag `--light` which indicates that the executable is to be started in light-mode:

```
$ cardano-wallet serve --light CRED
```

* `CRED` specifies how to connect to the less trusted blockchain data source. (MVP: Use [Blockfrost][]; `CRED` is a filepath to the secret token.)
* The `--light` argument and the `--node-socket` arguments are mutally exclusive with each other.

### REST API

When the executable was started in light-mode:

* The endpoints in the hierarchy `/v2/byron-wallets/*` MUST return an error.
* The endpoints in the hierarchy `/v2/shared-wallets/*` MAY return an error.
* The `GET /v2/network/information` endpoint MAY not return `sync_progress` as a percentage quantity from [0..100].

## Internal

See also the `light-mode-test` prototype!

* Collect required queries on data source in a data type `LightLayer`. In this way, we can replace the data source with a mock data source and run property tests on the chain following logic. (MVP: Use [Blockfrost][] for demonstration purposes.)
* Provide second implementation of `NetworkLayer` using a `LightLayer`.
    * See prototype for details! Important changes:
    * `watchTip` requires polling (2 seconds interval)
    * `currentNodeEra` may require hard-coding known eras.
    * `performanceEstimate` requires copying from ledger code.
    * `syncProgress` should be ignored for MVP.
    * The node-mode `NetworkLayer` does a lot of caching, we should avoid that for now and instead add a more uniform caching system later  through a function `addCaches :: NetworkLayer -> IO NetworkLayer`.
* New data type

    ```haskell
    data BlockSummary m = BlockSummary
        { from  :: ChainPoint
        , to    :: ChainPoint
        , query :: Address -> m [Transaction]
        }
    ```

    * consumed by `applyBlocks`. Adapt `discoverTransactions` from the prototype.
    * produced by `NetworkLayer`. Adapt `lightSync` from the prototype.
    * Idea: Use an additional GADT wrapper to specialize `applyBlocks` to sequential address states:
    
        ```haskell
            data BlockData m s where
                List
                    :: NonEmpty Block
                    -> BlockData m s
                Summary
                    :: BlockSummary m
                    -> BlockData m (SeqState n k)
        ```
        
        By using this type as argument to `applyBlocks`, we are guaranteed that `BlockSummary` is only used in conjunction with sequential state. Caveat: It would be better if we could avoid parametrizing the `NetworkLayer` type with the address discovery state `s`.


## Quality assurance

### Benchmarks

* Compare restoration times for an empty wallet in node-mode and in light-mode.
* Number of requests that we make to the data source needs to be monitored / kept in check.

### Testing

* Mock implementation of `LightLayer`.
    * We probably won't get good value / work out of implementing a `LightLayer` that interacts with the local cluster. One could look into implementing the light-mode `NetworkLayer` in terms of the node-mode `NetworkLayer`, though.
* Property tests for `applyBlocks` using `BlockSummary`
    * Implement a conversion function `summarize :: NonEmpty Block -> BlockSummary Identity`
    * Create a list of blocks, apply `applyBlocks` to both the list directly, and to the result of `summarize` — the results should be equal.

## External

* [Cardano-launcher][] will have to be modified to not launch the `cardano-node` process when the cardano-wallet is launched in light-mode.
* Provision of the data source is outside the scope of light-mode. We use [Blockfrost][] for demonstration purposes in the MVP.

  [cardano-launcher]: https://github.com/input-output-hk/cardano-launcher