# E2E testing
| Flavor | Results |
|--|--|
|**Full mode** |[![E2E Docker](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml) [![E2E Linux](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml) [![E2E MacOS](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml) [![E2E Windows](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml)  |
|**Light mode** | [![E2E Linux --light](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux-lite.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux-lite.yml) [![E2E MacOS --light](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos-lite.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos-lite.yml) [![E2E Windows --light](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows-lite.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows-lite.yml) |
|**Docker compose** | [![Docker-compose Linux](https://github.com/input-output-hk/cardano-wallet/actions/workflows/docker_linux.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/docker_linux.yml) [![Docker-compose MacOS](https://github.com/input-output-hk/cardano-wallet/actions/workflows/docker_macos.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/docker_macos.yml)
|

E2E functional tests of cardano-wallet are running nightly on [cardano testnet](https://testnets.cardano.org/en/cardano/overview/). Running tests against public testnet allows to exercise cardano-wallet on environment close to production (mainnet) utilizing and integrating maximally all components of the Cardano ecosystem like Stake pools, SMASH, metadata token server etc.


## Running tests

In order to run tests one needs to [have ruby](https://www.ruby-lang.org/en/documentation/installation/) (at least 2.7.1).

### Configuring test project
1. Get necessary _gems_.
```bash
cd test/e2e
bundle install
```
2. Decrypt secret files using `$TESTS_E2E_FIXTURES` secret:
  - `fixture_wallets.json.gpg` containing mnemonics of testnet fixture wallets
  - `blockfrost.api.key.gpg` containing Blockfrost API key for `testnet`

```bash
export TESTS_E2E_FIXTURES=*******
rake secrets_decode
```
> :information_source:  **_TESTS_E2E_FIXTURES_** secret  is defined on https://github.com/input-output-hk/cardano-wallet/settings/secrets and also used by GH actions. Note that this step is also executed on very first test run.
>
### Running all tests
In order to run all `e2e` tests one can simply run single [rake](https://github.com/ruby/rake) task:
```bash
$ rake run_on[testnet]
```
This master task is performing also all the necessary configuration steps (i.e. getting latest testnet configs and wallet/node binaries from [Hydra](https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs), starting everything up). All steps can also be executed as separate tasks , i.e.:

```bash
$ rake secrets_decode
$ rake get_latest_bins
$ rake get_latest_configs[testnet]
$ rake start_node_and_wallet[testnet]
$ rake wait_until_node_synced
$ rake spec
$ rake stop_node_and_wallet[testnet]
```
> :information_source:  **_Linux / MacOS_**
cardano-node and cardano-wallet are started as separate [screen](https://www.gnu.org/software/screen/manual/screen.html) sessions. One can attach to the respective session using:
>```bash
>$ screen -r NODE_testnet
>$ screen -r WALLET_testnet
>```

> :information_source: **_Windows_**
cardano-node and cardano-wallet are started as separate Windows services using [nssm](https://nssm.cc/) tool. One can examine services using Windows service manager like `services.msc`.

> :information_source: **_Docker_**
One can also start tests against cardano-wallet docker. There is docker-compose-test.yml provided that includes cardano-node and cardano-wallet. To start it several env variables need to be set to feed docker-compose:
>```bash
>NETWORK=testnet \
>TESTS_E2E_TOKEN_METADATA=https://metadata.cardano-testnet.iohkdev.io/ \
>WALLET=dev-master \
>NODE=1.30.1 \
>NODE_CONFIG_PATH=`pwd`/state/configs/$NETWORK \
>DATA=`pwd`/state/node_db/$NETWORK
>docker-compose -f docker-compose-test.yml up
>```
> Then running tests against docker is just:
>```bash
>$ rake wait_until_node_synced
>$ rake spec
>```

#### Running tests against local wallet
One can also run tests against `cardano-wallet` and `cardano-node` which are specified on machine's `$PATH`:

```bash
$ TESTS_E2E_BINDIR="" rake run_on[testnet]
```
Running tests as such skips downloading latest binaries from Hydra.

#### Running tests against wallet started in `--light` mode

One can also run some tests against wallet started in `--light` mode. Similar rake workflow can be applied except there is no need to start `cardano-node` and wait for it to be synced with the network:
```bash
$ rake secrets_decode
$ rake get_latest_bins
$ rake get_latest_configs[testnet]
$ rake start_wallet_light[testnet]
$ rake spec SPEC_OPTS="-t light"
$ rake stop_wallet_light[testnet]
```
All tests that are suitable for `--light` mode are tagged with `:light` tag.

### Test artifacts

By default following locations are used for different artifacts used by the tests:
- `./bins` - location for wallet and node binaries (will be downloaded here from [Hydra](https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs))
- `./state` - wallet/node databases, logs and configs

Locations are relative to `test/e2e` directory.
Default values can be changed by providing environment variables, for instance:

```bash
TESTS_E2E_STATEDIR=~/state \
TESTS_E2E_BINDIR=~/bins \
rake run_on[testnet]
```
Full list of environment variables is available in `e2e/env.rb`.

### Running specific tests
There are two types of tests within the suite:
 - `e2e` - tests that require node and wallet to be synced with the network (i.e. `rake wait_until_node_synced` step needs to pass before running them)
 - `non-e2e` - tests do not require node to be fully synced

One can run specific tests using by providing `SPEC` or `SPEC_OPTS` arguments to the rake task. For example:

 - run all tests (by default it'll download binaries from Hydra and wait for node to be synced before starting tests)
 ```ruby
 $ rake run_on[testnet]
 ```
 - run only `e2e` tests
 ```ruby
 $ rake run_on[testnet] SPEC_OPTS="-t e2e"
 ```
  - run only `non-e2e` tests on downloaded binaries and don't wait for node to be synced
 ```ruby
 $ rake run_on[testnet,bins,no-sync] SPEC_OPTS="-t ~e2e"
 ```
  - run only tests matching specific string
 ```ruby
 $ rake run_on[testnet] SPEC_OPTS="-e 'CardanoWallet::Shelley::Wallets'"
 ```
  - run only specific test identified by line of test code against node and wallet from the `$PATH` (skips downloading from Hydra)
 ```ruby
 $ TESTS_E2E_BINDIR="" rake run_on[testnet] SPEC=spec/shelley_spec.rb:9
 ```

### Skipping / making test pending

When test is failing due to a bug it can be marked as `pending`. This mark expects test to fail. When such test actually passes the report will indicate such test as failure indicating that it can be turned on again.

```ruby
it "Wallet can make multi-address transaction" do
  pending "ADP-777 - Failures on multi-address transactions"
  ...
end
```

Test can be also skipped, so it is not executed at all.

```ruby
it "Wallet can show utxo distribution" do
  skip "This functionality works intermittently - to be investigated"
  ...
end
```

### Running tests from GH actions workflow

There are several e2e workflows in GH actions for testing against different platforms (Docker, Linux, MacOS, Windows) and against different wallet modes (full, light).

#### Node DB cache

For speeding up execution in wallet's full mode we use cardano-node DB from cache. Thanks to this we don't have to wait for hours on each execution until cardano-node is synced with the chain. There are two ways of acquiring node DB:
 - GH actions cache - node db is cached on every execution and reused on subsequent run
 - AWS cache - node db is downloaded from https://updates-cardano-testnet.s3.amazonaws.com/cardano-node-state/index.html (the snapshot there is updated on every epoch)

While GH actions cache is realized by [actions/cache](https://github.com/actions/cache) for AWS one we have a dedicated rake step:

```ruby
$ rake get_latest_node_db[testnet]
```

#### Test schedule

All tests are scheduled to be executed on nightly basis against latest `master` version
of cardano-wallet.

It is also possible to trigger each workflow manually from [GH actions](https://github.com/input-output-hk/cardano-wallet/actions). In particular:
 - workflows can be executed against the binaries of particular PR,
 - for full wallet mode one can choose whether to use Node DB cached from GH cache or AWS

<img src="../../.github/images/e2e-workflow-form.png"/>

## Documentation

Cardano-wallet-rb repository: https://github.com/piotr-iohk/cardano-wallet-rb.

Ruby doc: https://rubydoc.info/gems/cardano_wallet.
