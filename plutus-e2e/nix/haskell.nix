############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# Enable profiling
, profiling ? config.haskellNix.profiling or false
# Enable Haskell Program Coverage for cardano-wallet libraries and test suites.
, coverage ? config.haskellNix.coverage or false
# Project top-level source tree
, src
# GitHub PR number (when building a PR jobset on Hydra)
, pr ? null
# Bors job type (when building a bors jobset on Hydra)
, borsBuild ? null
# Git revision of sources
, gitrev ? null
# Whether to set the `defer-plugin-errors` flag on those packages that need
# it. If set to true, we will also build the haddocks for those packages.
, deferPluginErrors ? false
}:

let
  inherit (pkgs.commonLib.cabalProjectIndexState ../cabal.project)
    index-state compiler-nix-name;

  project = haskell-nix.cabalProject {
    inherit src;
    inherit compiler-nix-name;
    modules = [
      ({ lib, pkgs, ... }: {
        # Use our forked libsodium from iohk-nix crypto overlay.
        packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      })
      ({ ... }: {
        packages = {
          plutus-contract.doHaddock = deferPluginErrors;
          plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

          plutus-use-cases.doHaddock = deferPluginErrors;
          plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

          plutus-ledger.doHaddock = deferPluginErrors;
          plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
        };
      })
    ];
  };
in
  project
