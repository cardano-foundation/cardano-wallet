# Testing

## Unit Tests

```
$ cabal test cardano-wallet:unit
```

Alternatively, one can run tests of a particular module by running:

```
$ cabal test cardano-wallet:unit --test-options "--match MyModule"
```

## Integration Tests

#### Pre-requisites

Install [`cardano-node`](https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html) and [`cardano-cli`](https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html); make sure to use one of the [compatible versions](https://github.com/cardano-foundation/cardano-wallet/blob/master/README.md#latest-releases).

Alternatively, use `cabal test all -j8`.

#### Test

```
$ cabal test cardano-wallet:integration
```

Many tests require a cardano network with stake pools. To support
this, the integration tests run a local `cardano-node` cluster with
one Ouroboros BFT node and three Ouroboros Praos nodes for the three
stake pools.

#### Environment Variables

Several environment variables control debugging features of the
integration tests and test cluster.

| **Variable**                          | **Type** | **Meaning**                                                                                                                    | **Default**                                            |
| ------------------------------------- | -------- | ------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------ |
| `CARDANO_WALLET_PORT`                 | number   | Set a specific port for the wallet HTTP server                                                                                 | Random unused port                                     |
| `NO_CLEANUP`                          | bool     | Leave the temporary directory after tests have finished.                                                                       | Delete directory on exit                               |
| `CARDANO_WALLET_TRACING_MIN_SEVERITY` | severity | Log level for the cardano-wallet server under test.                                                                            | Critical                                               |
| `CARDANO_NODE_TRACING_MIN_SEVERITY`   | severity | Log level for the test cluster nodes                                                                                           | Info                                                   |
| `TESTS_TRACING_MIN_SEVERITY`          | severity | Log level for test suite and cluster                                                                                           | Notice                                                 |
| `TESTS_LOGDIR`                        | path     | Write log files in the given directory                                                                                         | Log files are written to the tests temp directory      |
| `TESTS_RETRY_FAILED`                  | bool     | Enable retrying once of failed tests                                                                                           | No retrying                                            |
| `TOKEN_METADATA_SERVER`               | URL      | Use this URL for querying asset metadata                                                                                       | Asset metadata fetching disabled                       |
| `NO_CACHE_LISTPOOLS`                  | bool     | Do not cache pool listing retrieved from cardano-node. *Testing only. Use `--no-cache-listpools` command line for executable*. | Stake distribution is cached to improve responsiveness |
| `CACHE_LISTPOOLS_TTL`                 | number   | Cache time to live (TTL) for pool listing. *Testing only. Use `--no-cache-listpools` command line for executable*.             | 6 seconds for test builds                              |


Here are the possible values of different types of environment variables:

| **Type** | **Values**                                       |
| -------- | ------------------------------------------------ |
| bool     | unset or empty ⇒ _false_, anything else ⇒ _true_ |
| severity | debug, info, notice, warning, error, critical    |

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

#### Common Failures and Resolution

##### No More Wallets

If your test fails with something like:

```
user error (No more faucet wallet available in MVar!)
```

Generate more wallet mnemonics and populate the appropriate list in `lib/wallet/src/Test/Integration/Faucet.hs`.

Generate new mnemonics with:

```
nix build .#cardano-wallet
# Size may vary depending on which array you need to add to.
./result/bin/cardano-wallet recovery-phrase generate --size 15
```

## Mock servers

Use the `cardano-wallet:mock-token-metadata-server` executable as a
mock server for asset metadata. See the `--help` output for full
instructions.

## Benchmarks

### Database

```
$ cabal bench cardano-wallet:db
```

### Restoration

#### Pre-requisites

1. Follow the pre-requisites from `integration` above

2. (Optional) Install [hp2pretty](https://hackage.haskell.org/package/hp2pretty)

    ```
    $ cabal install hp2pretty
    ```

#### Test

```admonish warning
 Restoration benchmarks will catch up with the chain before running which can
 take quite a long time in the case of `mainnet`. For a better experience, make
 sure your system isn't too far behind the tip before running.
```

```
$ cabal bench cardano-wallet:restore
```

Alternatively, one can specify a target network (by default, benchmarks run on `testnet`):

```
$ cabal bench cardano-wallet:restore --benchmark-options "mainnet"
```

Also, it's interesting to look at heap consumption during the running of the benchmark:

```
$ cabal bench cardano-wallet:restore --benchmark-options "mainnet +RTS -h -RTS"
$ hp2pretty restore.hp
$ eog restore.svg
```

## Code Coverage

#### Pre-requisites

1. Follow the pre-requisites from `integration` above

#### Test

Running combined code coverage on all components is pretty easy. This generates code coverage reports in an HTML format as well as a short summary in the console. Note that, because code has to be compiled in a particular way to be "instrumentable" by the code coverage engine, it is recommended to run this command using another working directory (`--builddir` option) so that one can easily switch between coverage testing and standard testing (faster to run):

```
$ cabal test all --enable-coverage --builddir .dist-coverage
```

Note that, integration tests are excluded from the basic coverage report because the cardano-wallet server runs in a separate process. It it still possible to combine coverage from various sources (see [this article](https://medium.com/@_KtorZ_/continuous-integration-in-haskell-9ad2a73e8e46) for some examples / details).

## E2E Tests
See: [README](https://github.com/cardano-foundation/cardano-wallet/blob/master/test/e2e/README.md).

## QA Schedule

|                             | **PR Push** | **Bors Merge** | **Nightly** | **Pre-release** |     | **Hydra** | **Buildkite** | **GitHub** |     | **Required for** | **Status**                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| --------------------------- | :---------: | :------------: | :---------: | :-------------: | --- | :-------: | :-----------: | :--------: | --- | ---------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Unit tests                  |     LMw     |       LM       |      W      |                 |     |    LMw    |       L       |     W      |     | Merge            | <a href="https://buildkite.com/input-output-hk/cardano-wallet"><img src="https://img.shields.io/buildkite/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf/master?label=BUILD&style=for-the-badge"/></a>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Integation tests            |             |       LM       |      W      |                 |     |    LMw    |       L       |     W      |     | Merge            | <a href="https://buildkite.com/input-output-hk/cardano-wallet"><img src="https://img.shields.io/buildkite/7ea3dac7a16f066d8dfc8f426a9a9f7a2131e899cd96c444cf/master?label=BUILD&style=for-the-badge"/></a><br><a href="https://github.com/cardano-foundation/cardano-wallet/actions?query=workflow%3A%22cardano-wallet+Windows+Tests%22"><img src="https://img.shields.io/github/workflow/status/input-output-hk/cardano-wallet/cardano-wallet%20Windows%20Tests?label=Windows&style=for-the-badge" /></a>                                                                                                                                                                                                                                                                                                                                |
| E2E tests                   |             |                |    LDMW     |                 |     |           |               |    LDMW    |     | Merge            | [![E2E Linux](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-linux.yml/badge.svg)](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-linux.yml)<br>[![E2E Docker](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-docker.yml/badge.svg)](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-docker.yml)<br>[![E2E MacOS](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-macos.yml/badge.svg)](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-macos.yml)<br>[![E2E Windows](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-windows.yml/badge.svg)](https://github.com/cardano-foundation/cardano-wallet/actions/workflows/e2e-windows.yml)<br> |
| Code linting                |      X      |                |             |                 |     |           |       X       |            |     | Merge            |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Code review                 |      X      |       X        |             |                 |     |           |               |            |     | Merge            |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| DB Migration Path tests     |             |                |             |                 |     |           |               |            |     |                  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Release file contents tests |     LMW     |      LMW       |             |                 |     |    LMW    |               |            |     | Release          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Manual tests                |             |                |             |       LMW       |     |           |               |            |     | Release          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Wiki docs checks            |             |                |             |        X        |     |           |               |            |     | Release          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Database benchmark          |             |                |      L      |                 |     |           |       L       |            |     |                  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| API Latency benchmark       |             |                |      L      |                 |     |           |       L       |            |     |                  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Restore benchmark           |             |                |      L      |                 |     |           |       L       |            |     |                  |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| Docker image push           |             |      L--       |             |        X        |     |           |       L       |            |     | Merge            |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| ChangeLog review            |             |                |             |        X        |     |           |               |            |     | Release          |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |


| **Key** | **Meaning**                         |
| :-----: | ----------------------------------- |
|    L    | Run for Linux platform              |
|    D    | Run under Docker for Linux platform |
|    M    | Run fox macOS platform              |
|    W    | Run for Windows platform            |
|    w    | Run under Wine on Linux             |
|    -    | N/A                                 |
|    X    | Test is executed                    |


## Handy things to know

1. Normal PR builds do not run integration tests. This is to save CI
   resources and time.

2. Bors job builds also execute integration tests. This can be the
   reason why your PR is green, but Bors still fails.

3. Retrying a build in Buildkite or Hydra will have absolutely no
   effect if the Bors job has already failed.
