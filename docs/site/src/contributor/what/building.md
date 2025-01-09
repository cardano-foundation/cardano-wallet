# Building

## Prerequisites

`cardano-wallet` needs `nix` to build.


|                               | **Supported version** | **Dependency?** |
| ----------------------------- | --------------------- | --------------- |
| [Nix][]                       | >= 2.5.1              | Required        |


Follow the instructions on the [Nix][] page to install and configure Nix.

```admonish warning
Make sure that you have set up the **binary cache for Haskell.nix**,
[according to the instructions][haskell-nix-cache],
or you **will** wait a long time building multiple copies of GHC.
If Nix builds GHC, this is an indication that the cache
has not been set up correctly.

## Commands

```
> nix build
```

The resulting executable will appear at `./result/bin/cardano-wallet`.

Unless you have local changes in your git repo, Nix will download the
build from a nix cache rather than building locally. At the moment the cache is
available only for developers at CF.

You may also run the executable directly with:

```console
> nix run . -- <cardano wallet arguments>
```

or more comfortably, for pre-configured networks (`mainnet`, `testnet`, ...):
```console
> CARDANO_NODE_SOCKET_PATH=../cardano-node/node.socket

> nix run .#mainnet/wallet -- <optional additional cardano wallet arguments>
```

#### Cross-compiling with Nix

To build the wallet for Windows, from **Linux**:

```
> nix build .#ci.artifacts.win64.release
```

#### Building straight from GitHub

The following command will build the `master` branch, with the resulting executable appearing at `./result/bin/cardano-wallet`. To build another branch, add `/<branch name, tag, or commit hash>` (see [Nix Manual: Flake References](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html#flake-references) for syntax). As before, if the target ref has already been built by Hydra, then it will be fetched from cache rather than built locally.

```console
> nix build github:cardano-foundation/cardano-wallet
> ./result/bin/cardano-wallet version
v2022-01-18 (git revision: ce772ff33623e2a522dcdc15b1d360815ac1336a)
```

#### Cabal+Nix build

Use the Cabal+Nix build if you want to develop with incremental
builds, but also have it automatically download cached builds of
all dependencies.

If you run `nix develop`, it will start a
[development environment](https://input-output-hk.github.io/haskell.nix/user-guide/development/)
for `cardano-wallet`. This will contain:

- `cabal-install` and a GHC configured with a package database containing all Haskell package dependencies;
- system library dependencies;
- a Hoogle index and `hoogle` command for searching documentation;
- development tools such as `haskell-language-server`, `hlint`, `stylish-haskell`, and `weeder`;
- the `sqlite3` command;
- the Shelley node backend `cardano-node` and `cardano-cli`; and
- other Adrestia utility programs such as `cardano-address` and `bech32`

Inside this shell you can use `cabal build` and `ghci` for development.

For example, you might start an incremental build of the integration test suite with:

```console
ghcid -c "cabal repl test:integration"
```

and run the test suite with:

```console
cabal run test:integration
```

##### Profiling build with cached dependencies

Use `nix develop .#profiled` to get a shell where Haskell
dependencies are built with profiling enabled. You won't need to
rebuild all of the dependencies because they can be downloaded from
the Hydra cache.

```console
> nix develop .#profiled

[nix-shell:~/iohk/cardano-wallet]$ cabal build \
    --enable-tests --enable-benchmarks \
    --enable-profiling \
    all
```

## Haskell-Language-Server

The [haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/) provides an IDE for developers with some typical features:
  - Jump to definition.
  - Find references.
  - Documentation on hover.
  - etc.

### Prerequisites

The following must be installed:

- [direnv](https://direnv.net/)
- [nix-direnv](https://github.com/nix-community/nix-direnv)

We do not require a special version per-project so these executables can be installed system-wide.

Additionally, the following tools are provided by the cardano-wallet nix development shell:

- [hie-bios](https://github.com/haskell/hie-bios)
- [haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/)

We require a particular version of each per-project, so it's recommended to use the nix development environment to ensure you have the correct version.

In these instructions we enter a nix development environment using `direnv allow` rather than `nix develop` or `nix-shell` (see [Editor Setup](#editor-setup)).

### Setup

**haskell-language-server** requires some priming to work with *cardano-wallet*:

```
# Symlink hie.yaml to hie-direnv.yaml, which is the project configuration for haskell-language-server
ln -sf hie-direnv.yaml hie.yaml

# Build and cache the nix development environment
direnv allow

# Generate a build plan
cabal configure --enable-tests --enable-benchmarks -O0

# Build entire project
cabal build all
```

This will prime **haskell-language-server** to work with all modules of the project (tests, benchmarks, etc.) and be fully featured. Without these steps, **haskell-language-server** may fail to:
  - Find auto-generated modules (such as Paths_* modules).
  - Navigate across projects (jump-to-definition).
  - Provide documentation on hover.

### Testing

To test the **haskell-language-server**, use the following commands (these should be in your $PATH because you executed `direnv allow` previously, or have entered a nix development environment):

```
hie-bios check lib/wallet/src/Cardano/Wallet.hs
haskell-language-server lib/wallet/exe/cardano-wallet.hs
```

Occasionally `hie-bios` will fail with a `Segmentation Fault`. In these cases just run `hie-bios` again.

Note that these commands will only test a couple of files. To test the whole project, see [Troubleshooting](#troubleshooting).

### Editor Setup

With a working installation of **haskell-language-server**, we can integrate with our IDE of choice. See [Configuring Your Editor](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-your-editor).

IMPORTANT: you need to ensure that your editor invokes the same version of **haskell-language-server** that we have configured above. A simple way to do that is to launch your editor from within a nix development environment (e.g. `nix develop --command 'vim'`), or, more practically, to configure your editor with `direnv` support. Here are some examples:
  - [direnv.vim](https://github.com/direnv/direnv.vim)
  - [emacs-direnv](https://github.com/wbolster/emacs-direnv)

### Troubleshooting

Helpful resources:
  - [Configuring haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#configuring-haskell-language-server)
  - [hie-bios BIOS Configuration](https://github.com/haskell/hie-bios#bios)
  - [Troubleshooting haskell-language-server](https://haskell-language-server.readthedocs.io/en/latest/troubleshooting.html)

The [Testing](#testing) commands only tested a subset of the files in the project. To troubleshoot configuration issues, it's important to determine the source of the error.

If you know the source of the error, you can reproduce it on the command line with `haskell-language-server <the file>`. There are debug flags which might be useful (see `--help`).

If you do not know the source of the error, you can test every file in the project with:

```
# Provide list_sources function.
source "$(dirname "$0")/../cabal-lib.sh"

# Get every file in the project.
mapfile -t srcs < <(list_sources)

# Execute haskell-language-server on every file in the project.
# Note that this command can take upwards of an hour.
haskell-language-server "${srcs[@]}"
```

Once you can reproduce the error, look through the [Worked Examples](#worked-examples) below and see if you can resolve the issue. If you cannot, raise a [GitHub issue](https://github.com/cardano-foundation/cardano-wallet/issues/new?assignees=&labels=BUG&template=bug_report.yml) (external) or a JIRA issue (internal) with reproduction steps and tag @sevanspowell or @rvl.

#### Worked Examples

NOTE: [hie-bios BIOS Configuration](https://github.com/haskell/hie-bios#bios) is helpful background reading.

##### Source Filtering

In the past **haskell-language-server** failed when processing the `lib/wallet/extra/Plutus/FlatInteger.hs` file, as it was technically a Haskell file in the repository, but wasn't intended to be compiled with the project.

To fix this issue, we excluded the `lib/wallet/extra` folder from the project sources.

The bash function `list_sources` in `scripts/cabal-lib.sh` is responsible for determining the source files **haskell-language-server** sees. Modify this function to further remove any other files you wish to exclude:

```
list_sources() {
  # Exclude lib/wallet/extra. Those files are Plutus scripts intended
  # to be serialised for use in the tests. They are not intended to be built
  # with the project.
  # Exclude prototypes dir because it's a different project.
  git ls-files 'lib/**/*.hs' | grep -v Main.hs | grep -v prototypes/ | grep -v lib/wallet/extra
}
```

##### GHCI Flags

There were previously issues debugging overlapping/ambiguous instances as the error message printed by **haskell-language-server** did not contain enough information. We rectified this by adding `-fprint-potential-instances` to the GHCI flags of the **haskell-language-server** BIOS.

The bash function `ghci_flags` in `scripts/cabal-lib.sh` is responsible for providing the GHCI flags **haskell-language-server** uses. Modify this file with any other GHC flags you may require:

```
ghci_flags() {
  cat <<EOF
-XOverloadedStrings
-XNoImplicitPrelude
-XTypeApplications
-XDataKinds
-fwarn-unused-binds
-fwarn-unused-imports
-fwarn-orphans
-fprint-potential-instances
-Wno-missing-home-modules
EOF

...
}
```
