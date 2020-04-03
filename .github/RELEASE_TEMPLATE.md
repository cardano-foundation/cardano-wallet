<!-- Short optional summary -->

Compatible with [`jormungandr@{{JORM_TAG}}`](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}) and [`cardano-node@{{CARDANO_NODE_TAG}}`](https://github.com/input-output-hk/cardano-node/releases/tag/{{CARDANO_NODE_TAG}}).


## New Features

## Improvements

## Resolved Issues

<!-- Fixes included in this release that were present in the previous release -->

## Known Issues

<!-- Bugs known at the moment of the release, or discovered after and not fixed -->

## Changelog


<!-- A CHANGELOG, organized in milestones. Ideally, we put it within
some <details></details> elements to avoid cluttering the release notes -->

{{CHANGELOG}}


## Weekly Reports

- [Week 12 - 2020-03-20](https://github.com/input-output-hk/cardano-wallet/tree/weekly-reports/2020-03-20)

## Documentation

<!-- A snapshot of the documentation at the time of releasing. -->

| Link                                                                                                                                        | Audience                                                   |
| ---                                                                                                                                         | ---                                                        |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/{{GIT_TAG}})                                                       | Users of the Cardano Wallet API                            |
| CLI Manual: [jormungandr](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface/{{JORM_CLI_WIKI_COMMIT}}) / [byron](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface-(cardano-wallet-byron)/{{BYRON_CLI_WIKI_COMMIT}}) | Users of the Cardano Wallet API                            |
| [Docker Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Docker/{{DOCKER_WIKI_COMMIT}})                     | Users of the Cardano Wallet API                            |
| [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/{{GIT_TAG}})                                               | Haskell Developers using the `cardano-wallet` as a library |

## Installation Instructions 

<!-- Specific installation steps for this particular release. This should
basically captures whatever is currently available on the repository at
the moment of releasing. -->

### Jormungandr / ITN

1. Install [`jormungandr@{{JORM_TAG}}`](https://github.com/input-output-hk/jormungandr/releases/tag/{{JORM_TAG}}).

2. Download the provided `cardano-wallet-itn` for your platform, and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`. Or `%PATH%` on Windows.

3. (optional) Install the bash/zsh auto-completion script according to the [jormungandr cli manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface/{{JORM_CLI_WIKI_COMMIT}})

4. Start `cardano-wallet --help` and see available parameters.

### cardano-node / Byron 

1. Install [`cardano-node@{{CARDANO_NODE_TAG}}`](https://github.com/input-output-hk/cardano-node/releases/tag/{{CARDANO_NODE_TAG}}).

2. Download the provided `cardano-wallet-byron` for your platform, and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`. Or `%PATH%` on Windows.

3. (optional) Install the bash/zsh auto-completion script according to the [byron cli manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-Command-Line-Interface-(cardano-wallet-byron)/{{BYRON_CLI_WIKI_COMMIT}})

4. Start `cardano-wallet --help` and see available parameters.

### Additional notes

- On macOS: Make sure all `*.dylib` files are in the same directory as `cardano-wallet` binary.


#### TroubleShooting

<details>
<summary>cardano-wallet: error while loading shared libraries: <code>libcrypto.so.1.0.0</code>: cannot open shared object file: No such file or directory</summary>

`libcrypto 1.0.0` is no longer shipped with RHEL for a while (since ver. 6) and
SuSE. On these distributions, it is impossible to install it using native
package manager (e.g. `yum`).

One possible work-around is to create an artificial symbolic link from a newer version of `libcrypto`:

```bash
sudo ln -s /usr/lib64/libcrypto.so.1.1.1 /usr/lib64/libcrypto.so.1.0.0
```

Alternatively, one may also try using the docker image instead.

</details>

#### Docker

1. Pull from DockerHub.

```
$ docker pull inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr
```

2. Verify the image using the command-line.

```
$ docker run --rm inputoutput/cardano-wallet:{{CABAL_VERSION}}-jormungandr version
```

## Signatures

<!-- Signatures of people responsible for the release -->

Name                           | Role                | Approval
---                            | ---                 | ---:
Matthias Benkort @KtorZ        | Technical Team Lead | :hourglass: 
Piotr Stachyra @piotr-iohk     | QA Engineer         | :hourglass: 
Tatyana Valkevych @tatyanavych | Release Manager     | :hourglass: 
