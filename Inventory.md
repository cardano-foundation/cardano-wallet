# Integrating cardano-wallet with the new Byron Haskell node

Goal: identify gaps and risks for integrating with the Haskell node.

Structure:
- [Inventory of Jörmungandr on-chain types we use](#Inventory)
- [Discussion](#Discussion)
- [Summary Byron](#Summary-Byron)
- [A glimpse into Shelley](#A-glimpse-into-shelley)

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

The Jörmungandr wallet uses the `BlockHeader = (slotId, hash, prevHash)` record to identify
points on the chain.

The Byron Haskell node only *needs* `(slotNo, hash)` to start giving us blocks, but 
`prevHash` is available. Small ❌.

Byron `SlotNo` counts slots from genesis in a single number. The current Jörmungandr-compatible wallet `SlotId` counts the epoch AND the slot in that epoch. ❌

### Transactions

Transaction inputs in Jörmungandr consist of (txHash, index, coinValue). In the Haskell Byron implementation they are only (txHash, index). ❌

### Delegation certificates (Shelley)

There are delegation certificates in the Byron Haskell node.
[haskell cardano-ledger `Certificate`](https://github.com/input-output-hk/cardano-ledger/blob/0ad59ce46b3da8af6f6af97064e7c77cc7c2ac8b/cardano-ledger/src/Cardano/Chain/Delegation/Certificate.hs#L87-L99<Paste>)
I think they might be there for compatibility and be nothing we should concern ourselves with. (TODO: confirm)

The Jörmungandr wallet looks at delegation certificates to determine if, and to what it is delegating. This is pointless in Byron.

### Pool registrations (Shelley)

The wallet looks at pool registration certificates to collect metadata (pool-owners), for when it needs to list stake pools.

### Block producer (Shelley)

The stake pool metrics worker needs to know the minter of each block. In Byron there are no pools, so the worker would be pointless.

## Discussion

###  FeePolicy

We supported cardano-http-bridge in the past. Existing abstractions should already be able to deal with this difference. It *should* not be that much trouble, but it could be. ⚠️

Making sure related functionality (fee-estimation, coin selection) could maybe be subtly difficult. (@paweljakubas thoughts?)

### Tx inputs

The same goes for the different transaction inputs:
- We previously had a `Tx` abstraction for supporting inputs with and without `coin`-values. Removing it was conceptually simple but touched a lot of code. Before re-adding it we could take the opportunity to think of alternative solutions. ⚠️
- The coins in the Jörmungandr tx inputs are exposed in the Wallet API. Something needs to be changed to be able to serve Haskell Byron tx inputs in the API. If we want to continue exposing them in for the Jörmungandr-compatible wallet we need some added abstraction in the wallet API code. ⚠️

### Active slot coeff

While missing in Byron, it will exist in Shelley. We could set it to 100% internally in the wallet. Should be a problem at all. ✅

### Superfluous prevBlockHash in BlockHeader

With a good abstraction, or dropped Jörmungandr support we could remove the prevBlockHash field in BlockHeader, but it does no damage there. ✅

### SlotId vs SlotNo

When slotsPerEpoch cannot change the conversion between SlotId and SlotNo is simple.

But to support protocol parameter updates, we should consider making the core wallet logic deal with SlotNo for the Haskell node.

For Byron, we can assume they are static. ✅

### Delegation features

We cannot implement delegation features. We could
1. Keep the API the same, and use dummy implementations internally. E.g return an empty list of stake pools. E.g say that wallets have 0 account balance. I think this should work without any hustle.
2. Make the wallet modular enough to not have to do the above. I am not sure how difficult this would be.

### Summary Byron
- Deal with absence of `coin` value in transaction inputs (both internally and in the API)
- Deal with of tx fee policy working differently
- Absence of delegation features requires dummy implementation or re-designed API for Byron
- A few small problems with trivial solutions.

### A glimpse into Shelley (TODO)

- We need to support changing protocol parameters.
- Delegation features will likely work differently.
- Interesting thread: https://input-output-rnd.slack.com/archives/C819S481Y/p1576776488005100 