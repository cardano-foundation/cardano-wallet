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

The same goes for the different transaction inputs:
- We previously had a `Tx` abstraction for supporting inputs with and without `coin`-values. Removing it was conceptually simple but touched a lot of code. Before re-adding it we could take the opportunity to think of alternative solutions. ⚠️
- The coins in the Jörmungandr tx inputs are exposed in the Wallet API. Something needs to be changed to be able to serve Haskell Byron tx inputs in the API. If we want to continue exposing them in for the Jörmungandr-compatible wallet we need some added abstraction in the wallet API code. ⚠️

### Addresses

We can trivially re-encode addresses to work with the wallet core and re-decode them later
(when checking if they are ours or not).

We might be able to treat addresses more abstractly and avoid this.

### Active slot coeff

While missing in Byron, it will exist in Shelley. We could set it to 100% internally in the wallet. Shouldn't be a problem at all. ✅

### Superfluous prevBlockHash in BlockHeader

With a good abstraction, or dropped Jörmungandr support we could remove the prevBlockHash field in BlockHeader, but it does no damage there. ✅

### Point ByronBlock vs BlockHeader

We can probably get by with magic values to represent the `Origin`-case. ✅

A way to treat points (like ourobouros-network's`Point ByronBlock`) more abstract in the wallet could potentially be needed to avoid sabotaging other abstractions.

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
2. Make the wallet modular enough to not have to do the above. I am not sure how difficult this would be.

## Other, fuzzier difficulties of integration

I tried to get a wallet executable working with the Haskell node and discovered new kinds of difficulties.

- Chain parameters are not in the Haskell genesis block. The wallet will need to hard-code or read them from a config file at startup.
- The genesis block does not have to be treated differently because of above. The Jörmungandr wallet currently treats it completely separate.
- NetworkLayer and `follow` seem like the wrong abstractions to work with both Jörmungandr and Haskell.
- If `follow` is removed from core, `monitorStakePools` breaks (or will have be made jormungandr-specific).
- Both CLI and API will differ. (E.g. different configuration and CLI arguments)
- Use of BlockHeader as Point could be problematic if inconvenient conversions are needed at inconvenient places.

## Summary Byron
- Need to deal with absence of `coin` value in transaction inputs (both internally and in the API)
- Need to deal with of tx fee policy working differently
- Absence of delegation features requires dummy implementation or re-designed API for Byron
- There are a few small problems with trivial immediate solutions.
- There seem to be several fuzzier differences. For instance, the wallet `NetworkLayer` seem to be the wrong abstraction.
- Some of the fuzzier difficulties might lack cheap temporary solutions. More careful consideration might be useful there, re-evaluating current architecture while keeping future goals in mind.

## Extra: A glimpse into Shelley

- We need to support changing protocol parameters.
- Delegation features will likely work differently. E.g through imported ledger-rules or by asking for certain ledger state once per epoch. (See Duncan's messages: https://input-output-rnd.slack.com/archives/C819S481Y/p1576776488005100) The very good thing is that we will be able to ask for the ledger state of any point in the unstable chain. In Jörmungandr we could only retrieve the latest stake-distribution and we didn't know what point it belonged to when we got it. This was very tricky to deal with.
