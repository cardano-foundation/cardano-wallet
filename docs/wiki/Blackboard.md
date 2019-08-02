# Forewords

This wiki page is a "blackboard" where anyone in the team can write ideas or suggestions about various things on the team, the project, the code or the development process. Give as many details as you can so that, during the release meeting, we can discuss what's on the board and improve. 

# Contents

* [Investigate the `Arbitrary` instance for `UTCTime`](#investigate-the-arbitrary-instance-for-utctime-in-testquickcheckinstancestime)
* [Support multiple `--quiet` or `--verbose` flags in the CLI](#support-multiple---quiet-or---verbose-flags-in-the-cli)
* [Use shared libraries instead of symbolic links for code sharing](#use-shared-libraries-instead-of-symbolic-links-for-code-sharing)
* [Defer use of `IO` in the various layers](#defer-use-of-io-in-the-various-layers)
* [Run integration tests against mainnet](#run-integration-tests-against-mainnet)
* [Better error reporting in the API](better-error-reporting-in-the-api)
* [Consider DB state transition testing in the context of DB corruption](#consider-db-state-transition-testing-in-the-context-of-db-corruption)

# Ideas

## Run integration tests against mainnet

> Reviewed on **week 21**:
>
> - There's little value for running wallet management integration scenarios against mainnet for these are mostly using features not requiring any chain. Apart from the shape of addresses influenced by the protocol magic, we already cover restoration in nightly benchmarks against mainnet. 
>
> - It'll be nice however to have some sort of automated testing running less often than on each PR (could be every time something gets merged into master, or prior to a release) which attempts to make a small transaction on mainnet. With a wallet funded with only 100 ADA, we could already do more than 500 calls before running out of funds (calculations based on current network fees). 
>
> - All-in-all, it's probably not worth the effort at this stage since this would have to go through the `http-bridge` which is a temporary solution that will be dropped once fully integrated with Shelley Haskell or Shelley Rust nodes. At this time, we might re-assess this item.

We are now running unit tests against both `mainnet` and `testnet` enviroment. It would be good to run our integration tests also against `testnet` and `mainnet` clusters. Although integration scenarios seem to be agnostic to protocol magic, there is always a risk that something may misbehave on particular network and if it is the case we want to know about it ASAP. Therfore we _could_, with a few changes to the bridge, run integration tests on either a local network using the testnet magic, or a cluster using the mainnet magic.

For now we will wait after the integration with the rust node, and do it with the rust node directly.
The good thing with the rust node is that, we don't need a full cluster anymore. A single node is sufficient and can "emulate" delegation within itself, like an identity crisis.

## Better error reporting in the API

> Reviewed on **week 21**:
>
> Still waiting for a new release for servant. 

For instance `GET v2/wallets/{wallet-id}` always reports `404` for any - valid (but not existing) and non-valid wallet id. For non-valid wallet id it should be reporting `400`. This can be implemented probably after next Servant release, see -> https://github.com/input-output-hk/cardano-wallet/pull/252#discussion_r282786569.           