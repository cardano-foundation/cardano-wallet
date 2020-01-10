# Integrating cardano-wallet with the new Byron Haskell node

Goal: identify gaps and risks for integrating with the Haskell node.

Structure:
- [Inventory of Jörmungandr on-chain types we use](#Inventory)
- [Discussion](#Discussion)
- [Other, fuzzier difficulties of integration](#other-fuzzier-differences-of-integration)
- [Summary Byron](#Summary-Byron)
- [Extra: A glimpse into Shelley](#extra-A-glimpse-into-shelley)

## Inventory

### Protocol Parameters & Constants

These are the protocol parameters and constants the wallet is currently using:

| Parameter | Description | Exact equivalent in Haskell Byron Node |
| ----- |:-------------| --- |
| genesisBlockHash | Hash of the very first block | ✔ |
| genesisBlockDate | Start time of the chain. | ✔ |
| feePolicy | Policy regarding transaction fee. | ❌ |
| slotLength | Length, in seconds, of a slot. | ✔ |
| epochLength | Number of slots in a single epoch. | ✔ |
| txMaxSize | Maximum size of a transaction (soft or hard limit). | ✔ |
| epochStability | Length of the suffix of the chain considered unstable | ✔ |
| activeSlotCoefficient | In Genesis/Praos, corresponds to the % of active slots (i.e. slots for which someone can be elected as leader). | ❌ |

Genesis and slotting information should trivially be the same. 

The fee policies work differently. 

Jörmungandr charges `constant + coeff * (inputs + outputs) + optional_cert_specific_fee`

The Haskell Byron node charges `constant + coeff * tx_size_in_bytes`

some links:

- [Wallet BlockchainParameters](https://github.com/input-output-hk/cardano-wallet/blob/468e0fb14ed3b50ab3d16ba127f24242a41ed126/lib/core/src/Cardano/Wallet/Primitive/Types.hs#L1084-L1102)
- [cardano-ledger ProtocolParameters](https://github.com/input-output-hk/cardano-ledger/blob/0ad59ce46b3da8af6f6af97064e7c77cc7c2ac8b/cardano-ledger/src/Cardano/Chain/Update/ProtocolParameters.hs#L32-L62)
- [wallet use of Jörmungandr fee policy](https://github.com/input-output-hk/cardano-wallet/blob/468e0fb14ed3b50ab3d16ba127f24242a41ed126/lib/jormungandr/src/Cardano/Wallet/Jormungandr/Transaction.hs#L105)
- [doc of Jörmungandr fee policy: format.md](https://github.com/input-output-hk/chain-libs/blob/master/chain-impl-mockchain/doc/format.md)
- [cardano-ledger TxFeePolicy](https://github.com/input-output-hk/cardano-ledger/blob/0ad59ce46b3da8af6f6af97064e7c77cc7c2ac8b/cardano-ledger/src/Cardano/Chain/Common/TxFeePolicy.hs#L41-L59)


### Protocol Parameter Updates

Jörmungandr does not fully support protocol parameter updates. If protocol parameters can change, the wallet needs to be aware.

- Jörmungandr: static (updates not implemented)
- Haskell Byron: static (updates possible, but not likely)
- Haskell Shelley: dynamic (updates possible, wallet should handle this)

### Blocks

The Jörmungandr wallet uses the `BlockHeader = (slotId, blockHeight, hash, prevHash)` record to identify points on the chain.

The Byron Haskell node only *needs* `Point ByronBlock = Origin | At (slotNo, hash)` to start giving us blocks. The wallet can retrieve `blockHeight` and `prevHash` from block headers. But those two fields are not needed to initiate the chain-sync protocol. Small ❌. 

We might need to convert the genesis point `Origin`, to a `BlockHeader` if we are to persist it in the DB ❌

Byron `SlotNo` counts slots from genesis in a single number. The current Jörmungandr-compatible wallet's `SlotId` counts the epoch AND the slot in that epoch. ❌

In Jörmungandr a slot can only be inhabited by a single block. On the Haskell side, epoch boundary blocks will share slot with the normal block that comes after it. ❌

There is no limit on how far the Jörmungandr nodes can rollback. The Haskell Nodes have a limit: (`k` blocks). Haskell nodes have a clear separation between mutable and immutable parts of the chain.

### Genesis block

The `block0` of Jörmungandr contains the initial UTxO distribution and protocol parameters. The wallet retrieves it from Jörmungandr REST API like other blocks, but the wallet then treats it specially; to even start the wallet server, we first need the `block0`.

The Haskell node's initial protocol parameters and initial UTxO can be derived from the "Genesis Config", which is read from a config file ([example](https://github.com/input-output-hk/cardano-ledger/blob/0ad59ce46b3da8af6f6af97064e7c77cc7c2ac8b/cardano-ledger/mainnet-genesis.json#L1)).

So from the wallet's perspective, Jörmungandr and Haskell are very similar, except for *how* the genesis data is retrieved. Small ❌.

### Transactions

Transaction inputs in Jörmungandr consist of (txHash, index, coinValue). In the Haskell Byron implementation they are only (txHash, index). ❌

The addresses we get from the Haskell chain-sync protocol are not byte-strings, but rather in their deserialized form.

### Delegation certificates (Shelley)

There are delegation certificates in the Byron Haskell node.
[haskell cardano-ledger `Certificate`](https://github.com/input-output-hk/cardano-ledger/blob/0ad59ce46b3da8af6f6af97064e7c77cc7c2ac8b/cardano-ledger/src/Cardano/Chain/Delegation/Certificate.hs#L87-L99<Paste>)
I think they might be there for compatibility and be nothing we should concern ourselves with.

The Jörmungandr wallet looks at delegation certificates to determine if, and to what it is delegating. This is pointless in Byron.

### Pool registrations (Shelley)

The wallet looks at pool registration certificates to collect metadata (pool-owners), for when it needs to list stake pools.

### Block producer (Shelley)

The stake pool metrics worker needs to know the minter of each block. In Byron there are no pools, so the worker would be pointless.

## Discussion

###  FeePolicy

We supported cardano-http-bridge in the past. Existing abstractions should already be able to deal with this difference. It *should* not be that much trouble.

### Tx inputs

The same goes for the different transaction inputs.
We previously had a `Tx` abstraction for supporting inputs with and without `coin`-values. Removing it was conceptually simple but touched a lot of code. Before re-adding it we could take the opportunity to think of alternative solutions. ⚠️

### Addresses

We can trivially re-encode addresses to work with the wallet core and re-decode them later
(when checking if they are ours or not).

We might be able to treat addresses more abstractly and avoid this.

### Active slot coeff

While missing in Byron, it will exist in Shelley. We could set it to 100% internally in the wallet. Shouldn't be a problem at all. ✅

### Superfluous prevBlockHash in BlockHeader

We cannot drop Jörmungandr-support anytime soon. 

With a good abstraction we could remove the prevBlockHash field in BlockHeader, but it does no damage there. ✅

### Point ByronBlock vs BlockHeader

We can probably get by with magic values to represent the `Origin`-case. ✅

A way to treat points (like ourobouros-network's`Point ByronBlock`) more abstract in the wallet could potentially be needed to avoid polluting other abstractions.

### SlotId vs SlotNo

When slotsPerEpoch cannot change the conversion between SlotId and SlotNo is simple.

But to support protocol parameter updates, we should consider making the core wallet logic deal with SlotNo for the Haskell node.

For Byron, we can assume they are static. ✅

### Epoch Boundary Blocks

I imagine having two blocks with the same slot might break assumptions in our DB-rollbacks, but I don't know.

A simple solution would be to filter them out. ✅

For mainnet Shelley, EBBs do provide an easy way of knowing when a new epoch starts without having to keep track of protocol parameters (and when slots are counted from genesis). This would be an argument for not filtering them out. We will however most likely be maintaining up-to-date protocol parameters anyway, and would be able to tell when we're entering a new epoch through the protocol-params and the current slotNo.

### No rollbacks further than "k"

With the Haskell node we will be able to say that old enough state/checkpoints are immutable and can never be rolled back. We might want to change which checkpoints we store in DB. There is no point in keeping multiple checkpoints in the immutable part of the chain.

### Delegation features

We cannot implement delegation features. We could
1. Keep the API the same, and use dummy implementations internally. E.g return an empty list of stake pools. E.g say that wallets have 0 account balance. I think this should work without any hustle.
2. Serve only a portion of the API. With some trivial tweaks, this should be possible.

## Other, fuzzier difficulties of integration

I tried to get a wallet executable working with the Haskell node. This section contains my impressions.

- Both CLI and API will differ. (E.g. different configuration and CLI arguments)
- Use of BlockHeader as Point could be problematic if inconvenient conversions are needed at inconvenient places.
- `nextBlocks`, `monitorStakePools` and `follow` seem like the wrong abstractions in *core* to support the Haskell node. Reasoning:

The `NetworkLayer` (and therefore cardano-wallet-jörmungandr) implements
```haskell
    { nextBlocks
        :: Cursor target
        -> ExceptT ErrGetBlock m (NextBlocksResult target block)
```
which the `follow` function calls repeatedly with different `Cursor target`.

The Haskell chain sync protocol is stateful in the sense that the node knows where we are on the chain and will tell us when to roll forward and when to roll back automatically. A  `nextBlocks` without the `Cursor target` argument (local tip) would work better. But I thought the most straight-forward abstraction for the Haskell node would be:

```haskell
follow 
    :: [point] -- points/BlockHeaders we know about
    -> (RollBackOrForward block -> IO ()) 
    -> IO () -- syncs forever
```
which we could put inside `NetworkLayer. `[src](https://github.com/input-output-hk/cardano-wallet/blob/ed10bfeabac2e184a3d090d875846091249a3288/lib/core/src/Cardano/Wallet/Network.hs#L65-L68), and note this is different from our current `follow` in `Cardano.Wallet.Network`.

It was my intention for this `follow` to also work with `cardano-wallet-jormungandr`. But it does not! [`monitorStakePools`](https://github.com/input-output-hk/cardano-wallet/blob/cc7b767608399279fe1f88f5be2ed84268af3d35/lib/core/src/Cardano/Pool/Metrics.hs#L155-L229) relies on being able to tell the current `Cardano.Wallet.Network.follow` to `RetryImmediately`. This is to avoid *jörmungandr-specific* race-conditions when fetching the current stake-distribution. We can't tell the Haskell node to give us a block it has already sent us. Or even if we can with some trickery, it would just be unnecessary. Which is why I didn't  think the current `monitorStakePools` or `Cardano.Wallet.Network.follow` would be good abstractions, nor `nextBlocks`.

## Summary Byron
- Need to deal with absence of `coin` value in transaction inputs
- Need to deal with of tx fee policy working differently
- Absence of delegation features requires dummy implementation or slight changes to server-startup
- There are a few other small problems with trivial immediate solutions.
- I had trouble connecting the existing `NetworkLayer` abstraction with the Haskell Node.

## Extra: A glimpse into Shelley

We need to support changing protocol parameters. Most likely via a function from cardano-ledger, as specified in the [WB-44](https://jira.iohk.io/secure/RapidBoard.jspa?rapidView=46&projectKey=WB&view=planning&selectedIssue=WB-44&issueLimit=100) proposal.

We will use different mechanisms for retrieving data than for the jörmungandr delegation features:

> For protocol params, stake pools etc, you have a choice:
>
> Use the ledger rules (via the lib) to maintain the appropriate subset of the ledger state (everything other than the big things like utxo, reward accounts & stake distribution). With this approach you always have locally the full values and they are automatically in-sync with the block you're up to.
>
> Don't use the ledger rules, and every time you want to find some bit of the ledger state (other than the wallet utxo), you can use the local state query protocol to get reference to a snapshot of the ledger state as of a recent block, and then run queries against it to get what you want.
>
>[...]

>If it were not for the reward accounts I'd say don't even think about 2 and do everything via 1.

—- [Duncan](https://input-output-rnd.slack.com/archives/C819S481Y/p1576777084005800?thread_ts=1576776488.005100&cid=C819S481Y)

In Jörmungandr we could only retrieve the latest stake-distribution and we had to cleverly make sure that it corresponded to the same chain as our local chain. With the `LocalStateQuery` protocol we should be able to ask for the ledger state of any point within `k` to the tip. This is great as we won't have to be concerned about race-conditions.