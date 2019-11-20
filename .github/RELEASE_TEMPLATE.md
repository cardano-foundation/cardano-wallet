# Overview

<!-- Short high-level description about the content of the release.
This should also include a short presentation of files that are included, 
the target operating systems etc ... -->

| Artifact                                                | Platform                  | Description                                                  |
| ---                                                     | ---                       | ---                                                          |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.linux64.sha256` | <pre>Linux 64-bit</pre>   | Checksum for the Linux executable.                           |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.linux64.tar.gz` | <pre>Linux 64-bit</pre>   | Linux executable with auto-completion script for bash shell. |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.macos64.sha256` | <pre>MacOs 64-bit</pre>   | Checksum for the MacOS executable.                           |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.macos64.tar.gz` | <pre>MacOs 64-bit</pre>   | MacOS executable with necessary system libraries.            |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.win64.sha256`   | <pre>Windows 64-bit</pre> | Checksum for Windows executable.                             |
| `cardano-wallet-jormungandr@vYYYY-MM-DD.win64.zip`      | <pre>Windows 64-bit</pre> | Windows executable with necessary DLLs.                      |

# Installation Instruction

<!-- Specific installation steps for this particular release. This should
basically captures whatever is currently available on the repository at
the moment of releasing. --> 

## Linux 64-bit

1. Install [jormungandr@X.Y.Z](https://github.com/input-output-hk/jormungandr/releases/tag/vX.Y.Z) from the official repository.

2. Download `cardano-wallet-jormungandr@vYYYY-MM-DD.linux64.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

  ```bash
  $ curl -L https://github.com/input-output-hk/cardano-wallet/releases/download/vYYYY-MM-DD/cardano-wallet-jormungandr@vYYYY-MM-DD.linux64.tar.gz | tar xz -C $HOME/.local/bin
  ```

3. (optional) Install the bash/zsh auto-completion script:

  ```bash
  $ mv $HOME/.local/bin/cardano-wallet.sh > /etc/bash_completion.d/cardano-wallet.sh
  $ source /etc/bash_completion.d/cardano-wallet.sh
  ```

4. Start `cardano-wallet --help` and see available parameters.

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

## Mac OS 64-bit 

1. Install [jormungandr@X.Y.Z](https://github.com/input-output-hk/jormungandr/releases/tag/vX.Y.Z) from the official repository.

2. Download `cardano-wallet-jormungandr@vYYYY-MM-DD.macos64.tar.gz` and uncompress it in a directory that is on your `$PATH`, e.g. `/usr/local/bin`.

*Note:* Make sure all `*.dylib` files are in the same directory as `cardano-wallet` binary.

## Windows 64-bit

1. Install [jormungandr@X.Y.Z](https://github.com/input-output-hk/jormungandr/releases/tag/vX.Y.Z) from the official repository.

2. Download `cardano-wallet-jormungandr@vYYYY-MM-DD.win64.zip` and uncompress it in a directory that is on your `%PATH%`.

## Docker

Since vYYYY-MM-DD, Docker images are available for the wallet backend and published on [hub.docker.com](https://hub.docker.com/repository/registry-1.docker.io/inputoutput/cardano-wallet).

Assuming `docker` is installed on the host machine, go through the following steps:

1. Pull the `docker` image from docker-hub:

  ```bash
  $ docker pull inputoutput/cardano-wallet:YYYY.MM.DD-jormungandr
  ```

2. Interact with the image using `docker run`:

  ```bash
  $ docker run inputoutput/cardano-wallet:YYYY.MM.DD-jormungandr --help
  ```

# Known Issues

<!-- Bugs that are known at the moment at the moment of the release but aren't
show-stoppers. -->

# Key Features

<!-- What's included in the software, mention what's new in this particular
release. Use: ![][new] -->

## Command-Line

## Server

## Internal

## Known Limitations

<!-- What's included in the software, mention what's new in this particular
release. Use: ![][new] -->

# Documentation 

<!-- A snapshot of the documentation at the time of releasing. -->

| Link                                                                                                          | Audience                                                   |
| ---                                                                                                           | ---                                                        |
| [API Documentation](https://input-output-hk.github.io/cardano-wallet/api/vYYYY-MM-DD)                         | Users of the Cardano Wallet API                            |
| [Haddock Documentation](https://input-output-hk.github.io/cardano-wallet/haddock/vYYYY-MM-DD)                 | Haskell Developers using the `cardano-wallet` as a library |
| [CLI Manual](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface/<revision>) | Users of the Cardano Wallet API                            |

# Changelog

<!-- A CHANGELOG, organized in milestones. Ideally, we put it within
some <details></details> elements to avoid cluttering the release notes -->

# Bug Fixes

<!-- Fixes included in this release that were present in the previous release.
e.g.

  - <bug description> [#NNNN](<link-to-ticket>)
-->

# Weekly Reports

<!-- A link to the relevant weekly report, giving extra details about
development and progress -->

# Signatures

<!-- Signatures of people responsible for the release -->

Name                           | Role                | Approval
---                            | ---                 | ---:
Matthias Benkort @KtorZ        | Technical Team Lead | :hourglass:
Piotr Stachyra @piotr-iohk     | QA Engineer         | :hourglass:
Tatyana Valkevych @tatyanavych | Release Manager     | :hourglass:

[new]: https://raw.githubusercontent.com/input-output-hk/cardano-wallet/master/.github/images/badge-new.png 
