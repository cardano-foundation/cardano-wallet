

# E2E testing
[![E2E Docker](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-docker.yml) [![E2E Linux](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-linux.yml) [![E2E MacOS](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-macos.yml) [![E2E Windows](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml/badge.svg)](https://github.com/input-output-hk/cardano-wallet/actions/workflows/e2e-windows.yml)

E2E functional tests of cardano-wallet are running nightly on [cardano testnet](https://testnets.cardano.org/en/cardano/overview/). Running tests against public testnet allows to excercise cardano-wallet on environment close to production (mainnet) utilizing and integrating maximally all components of the Cardano ecosystem like Stake pools, SMASH, metadata token server etc.


## Running tests

In order to run tests one needs to [have ruby](https://www.ruby-lang.org/en/documentation/installation/) (at least 2.7.1).

### Configuring test project
```
cd test/e2e
bundle install
```
### Running all tests
In order to run all `e2e` tests one can simply run single [rake](https://github.com/ruby/rake) task:
```ruby
rake run_on["testnet"]
```
This master task is performing also all the necessary configuration steps (i.e. getting latest testnet configs and wallet/node binaries from hydra, starting everything up) which can also be executed as separate tasks , i.e.:

```ruby
rake get_latest_bins
rake get_latest_configs["testnet"]
rake start_node_and_wallet
rake wait_until_node_synced
rake spec
rake stop_node_and_wallet
```

> **_Linux / MacOS_**  
cardano-node and cardano-wallet are started as separate [screen](https://www.gnu.org/software/screen/manual/screen.html) sessions. One can attach to the respective session as follows:
```
screen -r NODE
screen -r WALLET
```
> **_Windows_**  
cardano-node and cardano-wallet are started as separate Windows services using [nssm](https://nssm.cc/) tool. One examine services using Windows service manager like `services.msc`.


### Running specific tests
There are two types of tests within this suite:
 - `e2e` - tests that require node and wallet to be synced with the network (i.e. `rake wait_until_node_synced` step needs to pass before running them)
 - `non-e2e` - tests do not require node to be fully synced

One can run specific tests using `rspec`. (Note that the wallet and node needs to be already running):
 - run all tests
 ```ruby
 rspec .
 ```
 - run only `e2e` tests
 ```ruby
 rspec . -t e2e
 ```
  - run only `non-e2e` tests
 ```ruby
 rspec . -t ~e2e
 ```
  - run only tests matching specific string
 ```ruby
 rspec . -e "CardanoWallet::Shelley::Wallets"
 ```
  - run only specific test identified by line of test code
 ```ruby
 rspec spec/shelley_spec.rb:9
 ```

### Environment variables

By default following locations are used for different artifacts used by tests:
- `./bins` - wallet and node binaries
- `./state` - wallet and node databases
 - `./logs` - wallet and node logs

Locations are relative to `test/e2e` directory.
Default values can be changed by providing environment variables, for instance:

```
TESTS_E2E_STATEDIR=~/state \
TESTS_E2E_LOGDIR=~/logs \
TESTS_E2E_BINDIR=~/bins \
rake run_on["testnet"]
```

There are other parameters that can be changed via env variables. The full list is available in `e2e/env.rb`.


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
