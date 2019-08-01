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

## Investigate the `Arbitrary` instance for `UTCTime` in `Test.QuickCheck.Instances.Time`

Informal testing of the `Arbitrary` instance for `UTCTime` shows that the values generated seem to be biased towards dates from a very long time ago (in the past):

```hs
% replicateM_ 10 (generate @UTCTime arbitrary >>= print)
1864-05-08 15:57:52.608452450367 UTC
1864-05-27 22:26:35.875633272764 UTC
1864-05-31 22:34:41.532543415989 UTC
1864-05-13 03:32:49.388750554273 UTC
1864-05-12 11:19:37.589857718608 UTC
1864-05-08 09:54:51.660554300608 UTC
1864-04-30 18:28:11.658332851904 UTC
1864-05-02 00:05:04.983741204058 UTC
1864-05-24 03:20:51.109800641012 UTC
1864-05-14 18:32:08.199434389462 UTC
```

We use this instance quite a bit in our tests. We might therefore consider making a new instance that generates a wider range of dates, both from the past and the future. It would also be nice to generate times with a mixture of precisions. For example:

```hs
% replicateM_ 10 (generate @UTCTime arbitrary >>= print)
2373-01-14 19:28:07.105907934381 UTC
1992-09-27 11:44:23 UTC
2671-08-02 07:12:23 UTC
2199-11-25 11:58:47 UTC
2800-06-08 14:04:45.178860422648 UTC
2572-07-11 16:22:26 UTC
2409-11-09 01:59:49 UTC
2059-02-02 15:29:48 UTC
2223-02-04 02:39:34.015153212017 UTC
2423-08-24 15:05:50.351400953777 UTC
```

Conclusion: Implemented in PR https://github.com/input-output-hk/cardano-wallet/pull/594

## Support multiple `--quiet` or `--verbose` flags in the CLI

Currently, our CLI provides `--quiet` and `--verbose` flags, which when specified will make the output quieter or more verbose, respectively.

However, many tools (including [Jörmungandr](https://github.com/input-output-hk/jormungandr)) support different levels of quietness and verbosity, by allowing the user to specify these flags multiple times. For example:

| Long Form | Short Form | Effect |
| -- | -- | -- |
| `--quiet --quiet --quiet` | `-qqq` | Extremely quiet |
| `--quiet --quiet` | `-qq` | Very quiet |
| `--quiet` | `-q` | Quiet |
| (nothing) | (nothing) | Default |
| `--verbose` | `-v` | Verbose |
| `--verbose --verbose` | `-vv` | Very verbose |
| `--verbose --verbose --verbose` | `-vvv` | Extremely verbose |

We should consider making our wallet CLI behave in a consistent way with any commands (such as `jormungandr`) on which it depends, so that different levels of quietness (or verbosity) correspond to the same log levels.

This is fairly easy to achieve with `optparse-applicative`. See https://github.com/pcapriotti/optparse-applicative#flags

## Use shared libraries instead of symbolic links for code sharing

Currently, the integration test suites for Jörmungandr and HTTP bridge share a **common code base** through the use of **symbolic links**.

Unfortunately, using symbolic links in this way **breaks code introspection and refactoring tools** such as `haskell-ide-engine`, `ghc-mod`, `HaRe` and so on, that depend on having a standard Cabal package structure.

We should consider moving these common source code files into a shared library that can be referenced from both integration test suites.

This work will have ramifications for:
- documentation generation (we don't necessarily want to generate documentation for this shared library)
- code coverage (we want to make sure that code coverage is not negatively affected).

We should consider the above ramifications as part of any solution.

Prototype: https://github.com/input-output-hk/cardano-wallet/pull/499

**Decision**: address this as issue https://github.com/input-output-hk/cardano-wallet/issues/583. 

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