


# E2E testing
[![E2E Docker](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml) [![E2E Linux](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml) [![E2E MacOS](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml) [![E2E Windows](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml)

E2E functional tests of cardano-wallet are running nightly on [cardano testnet](https://testnets.cardano.org/en/cardano/overview/). Running tests against public testnet allows to excercise cardano-wallet on environment close to production (mainnet) utilizing and integrating maximally all components of the Cardano ecosystem like Stake pools, SMASH, metadata token server etc.


## Running tests

In order to run tests one needs to [have ruby](https://www.ruby-lang.org/en/documentation/installation/) (at least 2.7.1).

### Configuring test project
1. Get necessary _gems_.
```
cd test/e2e
bundle install
```
2. Decrypt `fixture_wallets.json.gpg` containing mnemonics of testnet fixture wallets using `$TESTS_E2E_FIXTURES` secret.

```
export TESTS_E2E_FIXTURES=*******
rake fixture_wallets_decode
```
> :information_source:  **_TESTS_E2E_FIXTURES_** secret  is defined on https://github.com/input-output-hk/cardano-wallet/settings/secrets and also used by GH actions. Note that this step is also executed on very first test run.
>
### Running all tests
In order to run all `e2e` tests one can simply run single [rake](https://github.com/ruby/rake) task:
```ruby
$ rake run_on[testnet]
```
This master task is performing also all the necessary configuration steps (i.e. getting latest testnet configs and wallet/node binaries from [Hydra](https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs), starting everything up). All steps can also be executed as separate tasks , i.e.:

```ruby
$ rake fixture_wallets_decode
$ rake get_latest_bins
$ rake get_latest_configs[testnet]
$ rake start_node_and_wallet[testnet]
$ rake wait_until_node_synced
$ rake spec
$ rake stop_node_and_wallet
```

##### Running tests against local wallet

One can also run tests against `cardano-wallet` and `cardano-node` which are specified on machine's `$PATH`:

```
$ rake run_on[testnet,local]
```

Running tests as such skips downloading latest binaries from Hydra.

> :information_source:  **_Linux / MacOS_**
cardano-node and cardano-wallet are started as separate [screen](https://www.gnu.org/software/screen/manual/screen.html) sessions. One can attach to the respective session as follows:
```
$ screen -r NODE
$ screen -r WALLET
```
> :information_source: **_Windows_**
cardano-node and cardano-wallet are started as separate Windows services using [nssm](https://nssm.cc/) tool. One examine services using Windows service manager like `services.msc`.

> :information_source: **_Docker_**
One can also start tests against cardano-wallet docker. There is docker-compose-test.yml provided that includes cardano-node and cardano-wallet. To start it several env variables need to be set to feed docker-compose:
```
NETWORK=testnet \
TESTS_E2E_TOKEN_METADATA=https://metadata.cardano-testnet.iohkdev.io/ \
WALLET=dev-master-shelley \
NODE=1.25.1 \
NODE_CONFIG_PATH=`pwd`/state/configs/$NETWORK \
docker-compose -f docker-compose-test.yml up
```
> Then running tests against docker is just:
```ruby
$ rake wait_until_node_synced
$ rake spec
```

### Test artifacts

By default following locations are used for different artifacts used by the tests:
- `./bins` - location for wallet and node binaries (will be downloaded here from [Hydra](https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs))
- `./state` - wallet/node databases, logs and configs

Locations are relative to `test/e2e` directory.
Default values can be changed by providing environment variables, for instance:

```
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
 $ rake run_on[testnet,local] SPEC=spec/shelley_spec.rb:9
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

## Documentation

Ruby doc of cardano-wallet-rb (API wrapper): https://rubydoc.info/gems/cardano_wallet.
