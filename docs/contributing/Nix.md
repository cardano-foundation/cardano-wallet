# Nix

[Nix](https://nixos.org) is a package manager and build tool. It is used in `cardano-wallet` for:
 - [[Hydra]]
 - Provisioning dependencies in [Buildkite CI](https://github.com/input-output-hk/cardano-wallet/blob/master/.buildkite/pipeline.yml#L1).
 - Reproducible development environments ([nix-shell](https://github.com/input-output-hk/cardano-wallet/blob/master/shell.nix#L1)).

Nix is not required for `cardano-wallet` development, but it can help you a lot if you trit.

## Installing/Upgrading Nix

The minimum required version of Nix is 2.4.

- [Nix Package Manager Guide: Installation](https://nixos.org/manual/nix/stable/#ch-installing-binary)
- [Nix Package Manager Guide: Upgrading Nix](https://nixos.org/manual/nix/stable/#ch-upgrading-nix)

## Binary cache

To improve build speed, it is **highly recommended** (but not mandatory) to configure the **binary cache maintained by IOHK**.

See [iohk-nix/docs/nix.md](https://github.com/input-output-hk/iohk-nix/blob/8b1d65ba294708b12d7b15103ac35431d9b60819/docs/nix.md) or [cardano-node/doc/getting-started/building-the-node-using-nix.md](https://github.com/input-output-hk/cardano-node/blob/468f52e5a6a2f18a2a89218a849d702481819f0b/doc/getting-started/building-the-node-using-nix.md#building-under-nix)
for instructions on how to configure the Hydra binary cache on your system.

## Building with Nix

See the [[Building#Nix]] page.

## Reproducible Development Environment

This uses [`shell.nix`](https://github.com/input-output-hk/cardano-wallet/blob/master/shell.nix).

Short instructions are in [`cabal-nix.project`](https://github.com/input-output-hk/cardano-wallet/blob/master/cabal-nix.project).

Full instructions are on the [[Building#cabalnix-build]] page.

## Development tips

### Code generation

The Nix build depends on code which is generated from `stack.yaml` and
the Cabal files. If you change these files, then you will probably
need to update the generated files.

To do this, run:

```
./nix/regenerate.sh
```

Then add and commit the files that it creates.

Alternatively, wait for Buildkite to run this same command. It will
produce a patch, and also push a commit with updates back to your
branch.

### Haskell.nix pin

The Nix build also depends on the [Haskell.nix](https://github.com/input-output-hk/haskell.nix) build infrastructure. It may be necessary to update `haskell.nix` when moving to a
new Haskell LTS version or adding Hackage dependencies.

To update to the latest version, run the following command in a `nix-shell`:

```
niv update haskell.nix
```

Then commit the updated
[sources.json](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/sources.json#L1)
file.

When updating Haskell.nix, consult the [ChangeLog](https://github.com/input-output-hk/haskell.nix/blob/master/changelog.md#L1) file. There may have been API changes which need corresponding updates in `cardano-wallet`.

### iohk-nix pin

The procedure for updating the [`iohk-nix`](https://github.com/input-output-hk/iohk-nix) library of common code is much the same as for Haskell.nix. Run this in a `nix-shell` and commit the updated `nix/sources.json` file:

```
niv update iohk-nix
```

It is not often necessary to update `iohk-nix`. Before updating, ask devops whether there may be changes which affect our build.

## Common problems

### Warning: dumping very large path

```
warning: dumping very large path (> 256 MiB); this may run out of memory
```

Make sure you don't have large files or directories in your git worktree.

When building, Nix will copy the project sources into
`/nix/store`. Standard folders such as `.stack-work` will be filtered
out, but everything else will be copied.
