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

`cardano-wallet --help`

`cardano-wallet wallet --help`

`cardano-wallet wallet list --help`

 - Improve error msg for `FromText AddressAmount`
```haskell
            [l, r] -> AddressAmount . ApiT <$> fromText r <*> fromText l
```
if `fromText r` or `fromText l` fails we will get generic failure that won't tell user the info that parsing of payment `<amount>@<address>` have failed

 - chose a better name for CLI option `transaction create`. Currently we use it to do all three steps: coin selection, sign, submit. See discussion https://github.com/input-output-hk/cardano-wallet/pull/225#discussion_r281454697

 - `Qualtity "lovelace" Natural` is used to parse number of lovelace/coins from CLI. This is defined in `FromText (Quantity sym b)` https://github.com/input-output-hk/cardano-wallet/pull/225/files/fff43a4e5a70ed93bf028217ebdc90429252be2d#diff-27d87fed0f151afbb3b4e829fb315ba3R107 . We might want to use more fine grained parser for coins and parse "20lovelace" and "20ada" differently (and do coin conversion autimatically) and default "20" to lovelace

 - imoprove password validation in API and CLI which validate for strong passwords (passwords should have enough entropy)

## Better error reporting in the API
For instance `GET v2/wallets/{wallet-id}` always reports `404` for any - valid (but not existing) and non-valid wallet id. For non-valid wallet id it should be reporting `400`. This can be implemented probably after next Servant release, see -> https://github.com/input-output-hk/cardano-wallet/pull/252#discussion_r282786569.

## Verify Size of "seed" in various generators

When using `unsafeGenerateKeyFromSeed` or `generateKeyFromSeed` from the `Cardano.Wallet.Primitive.AddressDerivation` module, the length of the seed should be at least 16-bytes (cf: [cardano-crypto@Cardano/Crypto/Wallet.hs#L119-L124](https://github.com/input-output-hk/cardano-crypto/blob/3c5db489c71a4d70ee43f5f9b979fcde3c797f2a/src/Cardano/Crypto/Wallet.hs#L119-L124))

It's probably a good idea to add a proper disclaimer in the corresponding function documentation on our side and, to check various generators or method using them and make sure they actually always generate seed of at least 16-bytes.