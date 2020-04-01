# Overview


### Compatible with [`Jörmungandr@v0.8.14`](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14)

<!-- Short high-level description about the content of the release.
This should also include a short presentation of files that are included,
the target operating systems etc ... -->


| Artifact                                                                                                                                                                                         | Platform                  | Description                                                  |
| ---                                                                                                                                                                                              | ---                       | ---                                                          |
| [`cardano-wallet-jormungandr-linux64-{TAG_VERSION}.tar.gz`](https://github.com/input-output-hk/cardano-wallet/releases/download/{{GIT_TAG}}/cardano-wallet-jormungandr-linux64-{{GIT_TAG}}.tar.gz) | <pre>Linux 64-bit</pre>   | Linux executable with auto-completion script for bash shell. |
| [`cardano-wallet-jormungandr-macos64-{TAG_VERSION}.tar.gz`](https://github.com/input-output-hk/cardano-wallet/releases/download/{{GIT_TAG}}/cardano-wallet-jormungandr-macos64-{{GIT_TAG}}.tar.gz) | <pre>MacOs 64-bit</pre>   | MacOS executable with required system libraries.             |
| [`cardano-wallet-jormungandr-win64-{TAG_VERSION}.zip`](https://github.com/input-output-hk/cardano-wallet/releases/download/{{GIT_TAG}}/cardano-wallet-jormungandr-win64-{{GIT_TAG}}.zip)           | <pre>Windows 64-bit</pre> | Windows executable with required DLLs.                       |

# Changelog

<!-- A CHANGELOG, organized in milestones. Ideally, we put it within
some <details></details> elements to avoid cluttering the release notes -->

<details>
    <summary>Miscellaneous</summary>

PR | Description
-- | --
#1529 | Bump from 2020.3.16 to 2020.4.1
#1528 | update cardano-node matrix
#1524 | nix: Update test-suite name to jormungandr-integration
#1523 | Sync with libs used by cardano-node-1.9.3
#1518 | bump cardano-node's libraries to 1.9.1
#1515 | Update cardano-node matrix
#1513 | nix: Fix build of cardano-wallet-jormungandr-tests-win64
#1510 | bump ntp-client revision to 1.9.1
#1509 | Fix fee calculation on Byron
#1508 | skip jormungandr integration tests in CI, but enable cardano-node instead
#1506 | Update byron launcher and fix integration tests
#1504 | Allow serving API through TLS w/ client authentication
#1500 | cardano-node: bump to 1.9.1
#1491 | Add tracer showing raw binary transaction on submission
#1488 | Shutdown handler rework
#1487 | nix: add cardano-node configuration to the windows build products
#1485 | Revise exception handling in the chain follower to be more resilient
#1484 | disable Mux & Chain sync traces
#1481 | Fix testnet discrimination and intersection always being genesis 
#1475 | make NETWORK_BYRON work on cardano-node and jormungandr
#1474 | Mark wallet as "not_responding" when underlying workers die
#1473 | Use less verbose QuickCheck helpers in tests
#1472 | Staging / Temporary
#1451 | Bump stack.yaml dependencies to support cardano-node 1.8
#1449 | read protocol magic from genesis file instead of command-line option
#1447 | Update cardano-node to latest master version
#1441 | cardano-wallet server on _any_ arbitrary testnet
#1425 | Fix launcher unit tests on windows
</details>
<details>
    <summary>(ADP-192) Restore from public account key in CLI</summary>

PR | Description
-- | --
#1440 | more HWWallets CLI tests
</details>
<details>
    <summary>(ADP-210) Restore byron wallets from xpriv+passwd hash</summary>

PR | Description
-- | --
#1497 | Integration test updating wallet name, that is restored from Xprv
#1476 | Byron wallet restoration from xprv endpoint integration test
#1456 | Distinguish (internally) between legacy and new passphrase encryptions
#1454 | Byron wallet restoration from xprv endpoint impl
#1450 | Small update of byron restore summary
#1439 | Byron wallet restoration from xprv endpoint preparation
</details>
<details>
    <summary>(ADP-223, ADP-224) Byron wallet CRUD operations (update-name, update-spending password) and getting UTxO statistics</summary>

PR | Description
-- | --
#1520 | Fix passphrase conversion when coming from legacy wallets
#1512 | report empty legacy passphrase as no passphrase
#1511 | Fix passphrase check when coming from empty legacy passphrase
#1495 | Update password endpoint for byron
#1486 | Update wallet name and utxo statistics for byron
</details>
<details>
    <summary>(ADP-229) CLI support for Byron wallet</summary>

PR | Description
-- | --
#1493 | API endpoints for byron addresses endpoints 
#1478 | Support for wallet & transactions commands in cardano-wallet-byron
</details>
<details>
    <summary>(ADP-97) Nightly syncing tests with Byron TestNet/Mainnet</summary>

PR | Description
-- | --
#1507 | Store bench node socket in tmp dir
#1499 | Fix indentation in nightly.yml
#1498 | Add Buildable instance for UTxO Statistics & print from bench
#1482 | Re-add old wallet restoration benchmark
#1480 | Add cardano-node to "stack --nix test" environment
#1459 | Move cardano-node config/launch setup to separate library
</details>

## Bug Fixes

<!-- Fixes included in this release that were present in the previous release -->
<!-- TODO: can this be merged with the changelog above? -->

## Known Issues

<!-- Bugs known at the moment of the release, or discovered after and not fixed -->

# Weekly Reports

- [Week 12 - 2020-03-20](https://github.com/input-output-hk/cardano-wallet/tree/weekly-reports/2020-03-20)

# Documentation

<!-- A snapshot of the documentation at the time of releasing. -->

| Link                                                                                                                                        | Audience                                                   |
| ---                                                                                                                                         | ---                                                        |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/{{GIT_TAG}})                                                       | Users of the Cardano Wallet API                            |
| [CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface/c5eec9ab3937592513adf6e233aaa176eb671e02) | Users of the Cardano Wallet API                            |
| [Docker Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Docker/11246e7633eba794fb90fab385239753ba32d70e)                     | Users of the Cardano Wallet API                            |
| [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/{{GIT_TAG}})                                               | Haskell Developers using the `cardano-wallet` as a library |

# Installation Instructions 

<!-- Specific installation steps for this particular release. This should
basically captures whatever is currently available on the repository at
the moment of releasing. -->

## Linux 64-bit

1. Install [jormungandr@0.8.14](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14) from the official repository.

2. Download `cardano-wallet-jormungandr-linux64-{{GIT_TAG}}.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

  ```
  $ curl -L https://github.com/input-output-hk/cardano-wallet/releases/download/{{GIT_TAG}}/cardano-wallet-jormungandr-linux64-{{GIT_TAG}}.tar.gz | tar xz -C $HOME/.local/bin
  ```

3. (optional) Install the bash/zsh auto-completion script:

  ```
  $ mv $HOME/.local/bin/cardano-wallet.sh > /etc/bash_completion.d/cardano-wallet.sh
  $ source /etc/bash_completion.d/cardano-wallet.sh
  ```

4. Start `cardano-wallet --help` and see available parameters.

## Mac OS 64-bit 

1. Install [jormungandr@0.8.14](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14) from the official repository.

2. Download `cardano-wallet-jormungandr-macos64-{{GIT_TAG}}.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

*Note:* Make sure all `*.dylib` files are in the same directory as `cardano-wallet` binary.

## Windows 64-bit

1. Install [jormungandr@0.8.14](https://github.com/input-output-hk/jormungandr/releases/tag/v0.8.14) from the official repository.

2. Download `cardano-wallet-jormungandr-win64-{{GIT_TAG}}.zip` and uncompress it in a directory that is on your `%PATH%`.

## Docker

1. Pull from DockerHub.

```
$ docker pull inputoutput/cardano-wallet:2020.3.16-jormungandr
```

2. Verify the image using the command-line.

```
$ docker run --rm inputoutput/cardano-wallet:2020.3.16-jormungandr version
```

# Signatures

<!-- Signatures of people responsible for the release -->

Name                           | Role                | Approval
---                            | ---                 | ---:
Matthias Benkort @KtorZ        | Technical Team Lead | :hourglass: 
Piotr Stachyra @piotr-iohk     | QA Engineer         | :hourglass: 
Tatyana Valkevych @tatyanavych | Release Manager     | :hourglass: 

[new]: https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/.github/images/badge-new.png