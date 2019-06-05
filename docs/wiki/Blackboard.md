# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as many details as you can so that, during release meeting, we can discuss what's on the board and improve. 

# Ideas

## Defer use of `IO` in the various layers

We currently have a variety of interfaces that we bundle together in the `WalletLayer`, which is yet another interface. All these interfaces are parameterized over a monad `m`, although all the constructor we provide specialize it directly to `IO`. 

As we know,`IO` is very powerful and allows for _any_ sort of effects to happen. It means that we don't leverage the compiler to help us making sure we aren't doing bad thing like throwing exceptions or doing unrelated sort of effects. When debugging comes, it is also much more complex to identify where a bug might come from by looking at some functions over `IO`. It's a rather opaque type.

Instead, without going _too fancy_ with free effects and/or layers of transformers, we could simply leverage Haskell's class-constraint system and define some appropriate Monad to represent these effects; As a result, the constructor for the bridge network layer could become:

```hs
newHttpBridge
   :: (MonadLogger m, MonadHttp m)
   => Port
   -> Manager
   -> NetworkLayer m
```

Which makes it much easier to understand, from the type-system, where are the boundaries of a particular module.



## Run integration tests against mainnet

> Reviewed on **week 21**:
>
> - There's little value for running wallet management integration scenarios against mainnet for these are mostly using features not requiring any chain. Apart from the shape of addresses influenced by the protocol magic, we already cover restoration in nightly benchmarks against mainnet. 
>
> - It'll be nice however to have some sort of automated testing running less often than on each PR (could be every time something gets merged into master, or prior to a release) which attempts to make a small transaction on mainnet. With a wallet funded with only 100 ADA, we could already do more than 500 calls before running out of funds (calculations based on current network fees). 
>
> - All-in-all, it's probably not worth the effort at this stage since this would have to go through the `http-bridge` which is a temporary solution that will be dropped once fully integrated with Shelley Haskell or Shelley Rust nodes. At this time, we might re-assess this item.

We are now running unit tests against both `mainnet` and `testnet` enviroment. It would be good to run our integration tests also against `testnet` and `mainnet` clusters. Although integration scenarios seem to be agnostic to protocol magic, there is always a risk that something may misbehave on particular network and if it is the case we want to know about it ASAP. Therfore we _could_, with a few changes to the bridge, run integration tests on either a local network using the testnet magic, or a cluster using the mainnet magic.

For now we will wait after the integration with the rust node, and do it with the rust node directly.
The good thing with the rust node is that, we don't need a full cluster anymore. A single node is sufficient and can "emulate" delegation within itself, like an identity crisis.

## Better error reporting in the API

> Reviewed on **week 21**:
>
> Still waiting for a new release for servant. 

For instance `GET v2/wallets/{wallet-id}` always reports `404` for any - valid (but not existing) and non-valid wallet id. For non-valid wallet id it should be reporting `400`. This can be implemented probably after next Servant release, see -> https://github.com/input-output-hk/cardano-wallet/pull/252#discussion_r282786569.

## Consider db state transition testing in the context of DB corruption

DB corruption testing is an important topic when delivering db-dependent software. On the one hand one might assume that SQlite and persistent-sqlite, DB and haskell language handle of our choice, has been well tested in this context. On the other, we may add testing boosting our confidence in this area. When db state, as represented by tables with its schema and constraints, is well tested, db state transitions are still to be better covered. So the we can assume that separate db state is fine as it resides in DB layer. The next level is developing the observation that state cannot evaluate freely (ie., the value search spaces are not the same between the connected states). There are forbidden states if previous one is known. If the next db state assumes forbidden state we can treat it as corrupted state. In the context of what we store, it would be valuable to delve into `dbstate -> dbstate` transitions. And here put special attention on what is possible and what is contradictory when WalletMeta and TxHistory evolves. Foreign keys introduced should cleanup rows amidst tables when the major row is deleted. But what about changes of values in transactions with other fields not changed? What if db stops and later the same transaction appears in pending and inledger?    