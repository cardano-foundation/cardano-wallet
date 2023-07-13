# Updating Dependencies

If you use [Nix](nix.md)
to manage build and runtime dependencies you can be
confident that you will always have the correct versions for the
branch you are on, and that these will be exactly the same as those
versions used in CI.

It is possible to specify any git revision for the dependency and Nix
will automatically build it -- unless it has already been built
in a nix cache -- in which case the build result will be downloaded instead.

## nix develop

The default `nix develop` contains build tools, utilities and GHC
configured with a global package-db which matches `cabal.project`. This
is defined in the `devShells.default` attribute of `flake.nix`.

## nix flake lock

[`nix flake`](https://nixos.wiki/wiki/Flakes) manages the file [`flake.lock`](https://github.com/cardano-foundation/cardano-wallet/blob/master/flake.lock).

## Updating node backends

### `cardano-node` Haskell dependencies

To bump to a new version:

1. In `cardano-wallet/cabal.project`, update
   the dependency revisions to match your chosen version of
   `cardano-node/cabal.project`.
2. Run `./nix/regenerate.sh` (or let Buildkite do it)

### Jörmungandr

Follow the instructions in [`nix/jormungandr.nix`](https://github.com/cardano-foundation/cardano-wallet/blob/master/nix/jormungandr.nix).
