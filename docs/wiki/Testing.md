## Unit Tests

```
$ stack test cardano-wallet-core:unit
$ stack test cardano-wallet:unit
```

Alternatively, one can run tests of a particular module by running:

```
$ stack test cardano-wallet:unit --test-arguments "--match MyModule"
```

## Integration Tests

#### Pre-requisites

Install [`cardano-node`](https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html) and [`cardano-cli`](https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html); make sure to use one of the [compatible versions](https://github.com/input-output-hk/cardano-wallet/blob/master/README.md#latest-releases).

Alternatively, use `stack test --nix --ta "-j8"`.

#### Test

```
$ stack test cardano-wallet:integration
```

Many tests require a cardano network with stake pools. To support
this, the integration tests run a local `cardano-node` cluster with
one Ouroboros BFT node and three Ouroboros Praos nodes for the three
stake pools.

#### Environment Variables

Several environment variables control debugging features of the
integration tests and test cluster.

| **Variable** | **Type** | **Meaning**                       | **Default** |
|--------------|----------|-----------------------------------|-------------|
| `CARDANO_WALLET_PORT` | number | Set a specific port for the wallet HTTP server | Random unused port |
| `NO_CLEANUP` | bool | Leave the temporary directory after tests have finished. | Delete directory on exit |
| `CARDANO_WALLET_TRACING_MIN_SEVERITY` | severity | Log level for the cardano-wallet server under test. | Critical |
| `CARDANO_NODE_TRACING_MIN_SEVERITY` | severity | Log level for the test cluster nodes | Info |
| `TESTS_TRACING_MIN_SEVERITY` | severity | Log level for test suite and cluster | Notice |
| `TESTS_LOGDIR` | path | Write log files in the given directory | Log files are written to the tests temp directory |
| `TESTS_RETRY_FAILED` | bool | Enable retrying once of failed tests | No retrying |
| `NO_POOLS` | bool | Don't start any stake pool nodes in the local test cluster. | Pool nodes are started |
| `TOKEN_METADATA_SERVER` | URL | Use this URL for querying asset metadata | Asset metadata fetching disabled |

Here are the possible values of different types of environment variables:

| **Type** | **Values**                                        |
|----------|---------------------------------------------------|
| bool     | unset or empty ⇒ _false_, anything else ⇒ _true_ |
| severity | debug, info, notice, warning, error, critical     |

#### Logging and debugging

If your test has failed, viewing the logs often helps. They are
written to file in the integration tests temporary directory.

To inspect this directory after the tests have finished, set the `NO_CLEANUP`
variable.

<details>
    <summary>Here is an example tree</summary>

```
/tmp/test-8b0f3d88b6698b51
├── bft
│   ├── cardano-node.log
│   ├── db
│   ├── genesis.json
│   ├── node.config
│   ├── node-kes.skey
│   ├── node.opcert
│   ├── node.socket
│   ├── node.topology
│   └── node-vrf.skey
├── pool-0
│   ├── cardano-node.log
│   ├── db
│   ├── dlg.cert
│   ├── faucet.prv
│   ├── genesis.json
│   ├── kes.prv
│   ├── kes.pub
│   ├── metadata.json
│   ├── node.config
│   ├── node.socket
│   ├── node.topology
│   ├── op.cert
│   ├── op.count
│   ├── op.prv
│   ├── op.pub
│   ├── pool.cert
│   ├── sink.prv
│   ├── sink.pub
│   ├── stake.cert
│   ├── stake.prv
│   ├── stake.pub
│   ├── tx.raw
│   ├── tx.signed
│   ├── vrf.prv
│   └── vrf.pub
├── pool-1
│   ├── cardano-node.log
│   ├── db
│   ├── dlg.cert
│   ├── faucet.prv
│   ├── genesis.json
│   ├── kes.prv
│   ├── kes.pub
│   ├── metadata.json
│   ├── node.config
│   ├── node.socket
│   ├── node.topology
│   ├── op.cert
│   ├── op.count
│   ├── op.prv
│   ├── op.pub
│   ├── pool.cert
│   ├── sink.prv
│   ├── sink.pub
│   ├── stake.cert
│   ├── stake.prv
│   ├── stake.pub
│   ├── tx.raw
│   ├── tx.signed
│   ├── vrf.prv
│   └── vrf.pub
├── pool-2
│   ├── cardano-node.log
│   ├── db
│   ├── dlg.cert
│   ├── faucet.prv
│   ├── genesis.json
│   ├── kes.prv
│   ├── kes.pub
│   ├── metadata.json
│   ├── node.config
│   ├── node.socket
│   ├── node.topology
│   ├── op.cert
│   ├── op.count
│   ├── op.prv
│   ├── op.pub
│   ├── pool.cert
│   ├── sink.prv
│   ├── sink.pub
│   ├── stake.cert
│   ├── stake.prv
│   ├── stake.pub
│   ├── tx.raw
│   ├── tx.signed
│   ├── vrf.prv
│   └── vrf.pub
├── wallets-b33cfce13ce1ac74
│   └── stake-pools.sqlite
├── cluster.log
└── wallet.log

```
</details>

The default log level for log files is Info.

Only Error level logs are shown on stdout during test execution. To
change this, set the `*_MIN_SEVERITY` variables shown above.

## Mock servers

Use the `cardano-wallet:mock-token-metadata-server` executable as a
mock server for asset metadata. See the `--help` output for full
instructions.

## Benchmarks

### Database

```
$ stack bench cardano-wallet-core:db
```

### Restoration

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
$ stack bench cardano-wallet:restore
```

Alternatively, one can specify a target network (by default, benchmarks run on `testnet`):

```
$ stack bench cardano-wallet:restore --benchmark-arguments "mainnet"
```

Also, it's interesting to look at heap consumption during the running of the benchmark:

```
$ stack bench cardano-wallet:restore --benchmark-arguments "mainnet +RTS -h -RTS"
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

Note that, integration tests are excluded from the basic coverage report because the cardano-wallet server runs in a separate process. It it still possible to combine coverage from various sources (see [this article](https://medium.com/@_KtorZ_/continuous-integration-in-haskell-9ad2a73e8e46) for some examples / details).
