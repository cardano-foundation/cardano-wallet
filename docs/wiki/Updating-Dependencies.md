If you use Nix to manage build and runtime dependencies you can be
confident that you will always have the correct versions for the
branch you are on, and that these will be exactly the same as those
versions used in CI.

It is possible to specify any git revision for the dependency and Nix
will automatically build it -- unless it has already been built by
Hydra -- in which case the build result will be downloaded instead.

## nix-shell

The default `nix-shell` contains build tools, utilities and GHC
configured with a global package-db which matches `stack.yaml`. This
is defined in the `shell` attribute of `default.nix`.

## Stack

On Buildkite, the project will be built with `stack build --nix`. This
means that all stack commands will run inside the `nix-shell` defined
by [`nix/stack-shell.nix`](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/stack-shell.nix). If there is a program that is needed by the
build or tests, make sure that it is present there.

## niv

The [`niv`](https://github.com/nmattia/niv) utility manages the file
[`nix/sources.json`](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/sources.json).
To get this program, just run `nix-shell`.

## Updating node backends

### `cardano-node`

To update the version of `cardano-node` used by the Nix shells, run:

```
niv update cardano-node --branch REF
```

The `REF` argument can be a tag or a branch. Commit the updated
`nix/sources.json`.

### `cardano-node` Haskell dependencies

These are defined by the `resolver` in [`stack.yaml`](https://github.com/input-output-hk/cardano-wallet/blob/master/stack.yaml).

It points to a file in [`cardano-haskell/snapshots`](https://github.com/input-output-hk/cardano-haskell/tree/master/snapshots).

To bump to a new version:

1. Make a copy of the latest snapshot file.
2. Update the dependency revisions to match your chosen version of
   `cardano-node/cabal.project`. Refer to `cabal.project` rather than
   `stack.yaml` because it is more likely to be correct.
3. Check that `extra-deps` are correct and there are no conflicts with
   those in `cardano-wallet`.
4. Run `./nix/regenerate.sh` (or let Buildkite do it)
5. Open a PR on [`cardano-haskell`](https://github.com/input-output-hk/cardano-haskell).
6. Temporarily the snapshot URL in `cardano-wallet/stack.yaml` to
   point to the PR branch so that you can test the build.

**Tip**: The `stack.yaml` `resolver` file can also refer to a local file path -- so you needn't necessarily open a PR on `cardano-haskell` to make a temporary change.

### Jörmungandr

Follow the instructions in [`nix/jormungandr.nix`](https://github.com/input-output-hk/cardano-wallet/blob/master/nix/jormungandr.nix).
