> :warning: **Disclaimer** :warning: 
>
> The library expects a global ENV variable 'NETWORK' to be available. Possible values for this variable are:
>
> - mainnet
> - testnet
> - staging
> - local
> 
> It indicates the target network for the library and has an influence on a few things like the address binary format,
> the fee calculation and the underlying chain producer content.


## unit

```
$ stack test cardano-wallet-core:unit
$ stack test cardano-wallet-http-bridge:unit
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
$ curl -L -o cardano-node-simple-3.0.1.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/lib/http-bridge/test/data/cardano-node-simple/cardano-node-simple-3.0.1.tar.gz
$ tar xzf cardano-node-simple-3.0.1.tar.gz -C /usr/local/bin && rm cardano-node-simple-3.0.1.tar.gz
```

3. Import the initial testnet chain bootstrap for the `cardano-http-bridge`

```
$ curl -L -o hermes-testnet.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/lib/http-bridge/test/data/cardano-http-bridge/hermes-testnet.tar.gz
$ tar xzf hermes-testnet.tar.gz -C $HOME && rm hermes-testnet.tar.gz
```

#### test

```
$ stack test cardano-wallet-http-bridge:integration
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

> :warning: Disclaimer :warning: 
>
> Restoration benchmarks will catch-up with the chain before running which can be
> quite long in the case of `mainnet`. For a better experience, make sure you're 
> system isn't too far behind the tip before running.

```
$ stack bench cardano-wallet-http-bridge:restore
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

## Code Coverage

#### pre-requisites

1. Follow the pre-requisites from `integration` above

#### test

Running combined code coverage on all components is pretty easy. This generates code coverage reports in an HTML format as well as a short summary in the console. Note that, because code has to be compiled in a particular way to be "instrumentable" by the code coverage engine, it is recommended to run this command using another working directory (`--work-dir` option) so that one can easily switch between coverage testing and standard testing (faster to run):

```
$ stack test --coverage --fast --work-dir .stack-work-coverage
```