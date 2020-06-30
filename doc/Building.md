# Building

## Stack build (recommended)

Use [Haskell Stack](https://haskellstack.org/) to build this project:

```
stack build --test --no-run-tests
```

You may need to install the [`libsodium-dev`](https://doc.libsodium.org/installation), `libghc-hsopenssl-dev`, `gmp`, `sqlite` and `systemd` development
libraries for the build to succeed.


## Nix build

Use the Nix build if:

1. You don't have Haskell development tools installed, but you do have
   Nix installed.
2. You would like to cross-compile a build for Windows, or run the
   tests under Wine.
3. You would like to quickly grab a build of another branch from the
   Hydra cache, without needing to build it yourself.

Follow the instructions in
[iohk-nix/docs/nix.md](https://github.com/input-output-hk/iohk-nix/blob/master/docs/nix.md)
to install Nix and set up the IOHK binary cache.

To build the wallet for your current platform:

```
nix-build -A cardano-wallet-byron
```

If you have no local changes in your git repo, then this will download
the build from the Hydra cache rather than building locally.

### Cross-compiling

To build the wallet for Windows, from **Linux**:

```
nix-build release.nix -A x86_64-w64-mingw32.cardano-wallet-byron.x86_64-linux
```

If you're using **macOS**, then change `x86_64-linux` to
`x86_64-darwin`, and enable the cross-building flag (macOS is disabled
by default to reduce the load on CI):

```
nix-build \
    release.nix \
    --arg supportedCrossSystems '["x86_64-darwin"]' \
     -A x86_64-w64-mingw32.cardano-wallet-byron.x86_64-darwin
```

### Building straight from GitHub

To build another branch (replace `master` with the branch name, tag, or commit hash):

```
nix-build https://github.com/input-output-hk/cardano-wallet/archive/master.tar.gz --argstr gitrev master -A cardano-wallet-byron
```

### Navigating Hydra

The Hydra [Jobset page](https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs)
shows all jobs defined in `release.nix`. Some of the release jobs have a download link.

- [Windows](https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-byron-win64/latest)
- [macOS](https://hydra.iohk.io/job/Cardano/cardano-wallet/cardano-wallet-byron-macos64/latest)


### Code generation

The Nix build depends on code which is generated from `stack.yaml` and
the Cabal files. If you change these files, then you will probably
need to update the generated files.

To do this, run:

```
./nix/regenerate.sh
```

Then add and commit the files that it creates.

Alternatively, wait for Buildkite to run this same command, and apply
the patch that it produces.

### iohk-nix pin

The Nix build also depends on the
[iohk-nix](https://github.com/input-output-hk/iohk-nix) library of
common code. It may be necessary to update `iohk-nix` when moving to a
new Haskell LTS version.

To update iohk-nix, run the following command:

```
./nix/update-iohk-nix.sh
```

Then commit the updated
[iohk-nix-src.json](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/iohk-nix-src.json)
file.


## Cabal build with Nix

Use the Cabal build if you want to develop with incremental builds,
and have it automatically download all dependencies.

If you run `nix-shell`, it will start a
[development environment](https://input-output-hk.github.io/haskell.nix/user-guide/development/)
for `cardano-wallet`. This will contain:

- a GHC configured with a package database containing all Haskell package dependencies;
- system library dependencies;
- a Hoogle index and `hoogle` command for searching documentation;
- development tools such as `hlint`, `stylish-haskell`, and `weeder`;
- the `sqlite3` command; and
- the node backend `jormungandr`, and `jcli`.
- the Byron reboot node backend `cardano-node`

Inside this shell you can use `cabal new-build` and `ghci` for development.

```
nix-shell --run "cabal new-build --enable-tests --enable-benchmarks all"
```

### Release build

Enable the `release` Cabal flag to disallow compiler warnings and get build optimization:

```
cabal new-configure -frelease --enable-tests --enable-benchmarks
```
