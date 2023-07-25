# Nix flake

## Status

Accepted - [ADP-983](https://input-output.atlassian.net/browse/ADP-983)

## Context

[PR]: https://github.com/cardano-foundation/cardano-wallet/pull/2997

The DevOps team have contributed a [PR][] which converts the Nix build to the new Nix flake format.

This [blog series](https://www.tweag.io/blog/2020-05-25-flakes/) provides some background information on flakes.

DevOps team also wish to convert Daedalus to use Nix flakes.
For this to work well, it's better that Daedalus dependencies
such as cardano-wallet are also defined as flakes.

The _tl;dr_ for flakes is that `default.nix` and `shell.nix` are deprecated in favour of `flake.nix`.
You type `nix build .#cardano-wallet` instead of `nix-build -A cardano-wallet` and you type `nix develop` instead of `nix-shell`.

## Decision

Review and merge [PR \#2997][PR], since flakes seem to be the future and offer some benefits to developers.

This will add a `flake.nix` file, and replace `default.nix`, `shell.nix`, and `release.nix` with backwards compatibility shims.

- Pay close attention to any broken builds or CI processes which may result from these changes.
- Ensure that the documentation is updated with the new build commands.
- Notify developers that the Nix build files have changed, and they may need to modify the commands which they use.
- After a while, remove `default.nix` and `shell.nix` and associated compatibility code.

## Consequences

1. Developers and users of the Nix build will now need Nix version 2.4 at least - Nix 2.5 is probably better.
2. The `nix build` CLI for building with flakes seems nicer than the old `nix-build`.
3. Apparently, `nix develop` has better caching than `nix-shell`, and so it will be faster to start the dev environment.
4. The process for updating Nix dependencies (e.g. Haskell.nix) is easier and more sane - `nix flake lock`.
5. Other PRs which are open may need to be rebased on latest `master` for their CI to pass.
