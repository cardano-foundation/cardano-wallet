## unit

```
$ stack test cardano-wallet:unit
```

Alternatively, one can run tests of a particular module by running:

```
$ stack test cardano-wallet:unit --test-arguments "--match MyModule"
```

## integration

#### pre-requisites

1. Install our fork of [cardano-http-bridge](https://github.com/KtorZ/cardano-http-bridge)

```
$ cargo install --branch cardano-wallet-integration --git https://github.com/KtorZ/cardano-http-bridge.git
```

2. Install [cardano-sl@cardano-node-simple](https://github.com/input-output-hk/cardano-sl)

```
$ git clone git@github.com:input-output-hk/cardano-sl.git
$ cd cardano-sl
$ stack install cardano-sl-node:exe:cardano-node-simple
```

Alternatively, if you're running on linux, you may use a pre-compiled version:

```
$ curl -L -o cardano-node-simple-3.0.1.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/test/data/cardano-node-simple/cardano-node-simple-3.0.1.tar.gz
$ tar xzf cardano-node-simple-3.0.1.tar.gz -C /usr/local/bin && rm cardano-node-simple-3.0.1.tar.gz
```

3. Import the initial testnet chain bootstrap for the `cardano-http-bridge`

```
$ curl -L -o hermes-testnet.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/test/data/cardano-http-bridge/hermes-testnet.tar.gz
$ tar xzf hermes-testnet.tar.gz -C $HOME && rm hermes-testnet.tar.gz
```

#### test

```
$ stack test cardano-wallet:integration
```

## benchmark

So far, only one type of benchmarks is available: fully restoring wallets
against a target network (e.g. `mainnet`).

#### pre-requisites

1. Follow the pre-requisites from `integration` above

2. (Optional) Install [hp2pretty](https://www.stackage.org/nightly-2019-03-25/package/hp2pretty-0.9)

```
$ stack install hp2pretty
```

#### test

> ⚠️  Disclaimer ⚠️
>
> Restoration benchmarks will catch-up with the chain before running which can be
> quite long in the case of `mainnet`. For a better experience, make sure you're 
> system isn't too far behind the tip before running.

```
$ stack bench
```

Alternatively, one can specify the a target network (by default, benchmarks run on `testnet`):

```
$ stack bench --benchmark-arguments "mainnet"
```

Also, it is interesting to look at the heap consumption during the benchmark:

```
$ stack bench --benchmark-arguments "mainnet +RTS -h -RTS"
$ hp2pretty restore.hp
$ eog restore.svg
```
