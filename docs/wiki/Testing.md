## Unit Tests

```
$ stack test cardano-wallet-core:unit
$ stack test cardano-wallet-http-bridge:unit
$ stack test cardano-wallet-jormungandr:unit
```

Alternatively, one can run tests of a particular module by running:

```
$ stack test cardano-wallet:unit --test-arguments "--match MyModule"
```

## Integration Tests

#### Pre-requisites

1. Install our fork of [cardano-http-bridge](https://github.com/KtorZ/cardano-http-bridge)

    ```
    $ cargo install --branch cardano-wallet-integration --git https://github.com/KtorZ/cardano-http-bridge.git
    ```

    Check that it works and that you have the correct version by running:
    ```
    $ cardano-http-bridge --help
    cardano-http-bridge 0.0.5
    ```

2. Install [cardano-sl@cardano-node-simple](https://github.com/input-output-hk/cardano-sl)

    ```
    $ git clone git@github.com:input-output-hk/cardano-sl.git
    $ cd cardano-sl
    $ stack install cardano-sl-node:exe:cardano-node-simple
    ```

    Alternatively, if you're running on Linux, you may use a pre-compiled version:

    ```
    $ curl -L -o cardano-node-simple-3.0.1.tar.gz https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/lib/http-bridge/test/data/cardano-node-simple/cardano-node-simple-3.0.1.tar.gz
    $ tar xzf cardano-node-simple-3.0.1.tar.gz -C /usr/local/bin && rm cardano-node-simple-3.0.1.tar.gz
    ```

    Check that it works by running the following:
    ```
    $ cardano-node-simple --version
    cardano-node-3.0.3, git revision 41b73a146e5d15e1dd48dd0a272ba646f588e7b5
    ```

3. Import the initial testnet chain bootstrap data for `cardano-http-bridge`

    ```
    $ curl -L https://github.com/KtorZ/cardano-http-bridge/releases/download/v0.0.5/hermes-testnet.tar.gz | tar xz -C $HOME
    ```

#### Test

```
$ stack test cardano-wallet-http-bridge:integration
$ stack test cardano-wallet-jormungandr:integration
```

## Benchmark

So far, only one type of benchmark is available: fully restoring wallets
against a target network (e.g. `mainnet`).

#### Pre-requisites

1. Follow the pre-requisites from `integration` above

2. (Optional) Install [hp2pretty](https://www.stackage.org/nightly-2019-03-25/package/hp2pretty-0.9)

```
$ stack install hp2pretty
```

#### Test

> :warning: Disclaimer :warning: 
>
> Restoration benchmarks will catch up with the chain before running which can
> take quite a long time in the case of `mainnet`. For a better experience, make
> sure your system isn't too far behind the tip before running.

```
$ stack bench cardano-wallet-http-bridge:restore
```

Alternatively, one can specify a target network (by default, benchmarks run on `testnet`):

```
$ stack bench --benchmark-arguments "mainnet"
```

Also, it's interesting to look at heap consumption during the running of the benchmark:

```
$ stack bench --benchmark-arguments "mainnet +RTS -h -RTS"
$ hp2pretty restore.hp
$ eog restore.svg
```

## Code Coverage

#### Pre-requisites

1. Follow the pre-requisites from `integration` above

#### Test

Running combined code coverage on all components is pretty easy. This generates code coverage reports in an HTML format as well as a short summary in the console. Note that, because code has to be compiled in a particular way to be "instrumentable" by the code coverage engine, it is recommended to run this command using another working directory (`--work-dir` option) so that one can easily switch between coverage testing and standard testing (faster to run):

```
$ stack test --coverage --fast --work-dir .stack-work-coverage
```