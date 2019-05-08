# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as many details as you can so that, during release meeting, we can discuss what's on the board and improve. 

# Ideas

## Better Restoration Stress Benchmark

Existing chain data doesn't necessarily include "extreme" cases that might occur in the future

* Make data generators which set up transactions for wallets of various sizes.
* Use the [weigh](https://www.fpcomplete.com/blog/2016/05/weigh-package) package to measure and display the GHC heap usage of test scenarios.

- [ ] Figure out a way of generating semi-realistic transactions in blocks
- [ ] Use a mock network layer to feed generated blocks to wallet layer
- [ ] Set up a test case which checks heap usage after applying a certain number of transactions in a certain number of blocks.
- [ ] Also measure how long it took to apply those blocks/transactions.

## Benchmark tests - automatically check the results

We don't have any ways of automatically checking the benchmark results (https://buildkite.com/input-output-hk/cardano-wallet-nightly) at the moment, so this is still something which requires a manual checks. A bullet point to the release checklist(https://github.com/input-output-hk/cardano-wallet/wiki/Release-Checklist) has been added to remind ourselves to do so.

Maybe it would be nice to have some tresholds defined and fail the CI once they are crossed. 

## Run integration tests against mainnet

We are now running unit tests against both `mainnet` and `testnet` enviromnet. It would be good to run our integration tests also against `testnet` and `mainnet` clusters. Although integration scenarios seem to be agnostic to protocol magic, there is always a risk that something may misbehave on particular network and if it is the case we want to know about it ASAP. Therfore we _could_, with a few changes to the bridge, run integration tests on either a local network using the testnet magic, or a cluster using the mainnet magic.

For now we will wait after the integration with the rust node, and do it with the rust node directly.
The good thing with the rust node is that, we don't need a full cluster anymore. A single node is sufficient and can "emulate" delegation within itself, like an identity crisis.

## Improvements to the CLI

 - if the passphrases don't match - prompt user to put them again twice
 - make wallet backend server introduce itself in the HTTP header server (and make CLI to check for that value and put some warning/error if it don't match)
 - `create wallet` wizard to be a whole responsive/prompt CLI
 - different levels of `--help`, e.g.:
>cardano-wallet --help
>cardano-wallet wallet --help
>cardano-wallet wallet list --help
