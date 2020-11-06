######################################################################
#
# Creates a development environment for Cabal builds or ghci sessions,
# with various build tools included.
#
# Short instructions are in ./cabal-nix.project.
#
# Full instructions - https://github.com/input-output-hk/cardano-wallet/wiki/Building#cabalnix-build
#
# If you need a nix-shell with Cabal, GHC, and system dependencies, but *without*
# Haskell dependencies, see ./nix/cabal-shell.nix.
#
######################################################################

{ walletPackages ? import ./default.nix { inherit sourcesOverride; }
, pkgs ? walletPackages.pkgs
, profiling ? false  # enable profiling in haskell dependencies
, sourcesOverride ? {}  # see sourcesOverride in nix/default.nix
, checkMaterialization ? false  # Use when updating tools
}:

with pkgs.lib;
with pkgs.haskell-nix.haskellLib;

let
  mkShell = name: hp: hp.shellFor rec {
    inherit name;
    packages = ps: attrValues (selectProjectPackages ps);
    buildInputs = (with walletPackages; [
        jormungandr
        jormungandr-cli
        cardano-node
        cardano-cli
        cardano-address
        cardano-tx
        bech32
      ]) ++ (with pkgs; [
        niv
        pkgconfig
        python3Packages.openapi-spec-validator
        ruby
        sqlite-interactive
        yq
      ]);
    tools = let
      # NOTE: Updating versions of Haskell tools
      #
      # Modify the tool version number to a different Hackage version.
      # If you have chosen a recent version, you may also need to
      # advance the "index-state" variable to include the upload date
      # of your package.
      #
      # When increasing the "index-state" variable, it's possible that
      # you will also need to update Haskell.nix to get a recent
      # Hackage package index.
      #
      #   niv update haskell.nix
      #
      # After changing tool versions, you can update the generated
      # files which are cached in ./nix/materialized. Run this
      # command, follow the instructions shown, then commit the
      # updated files.
      #
      #   nix-shell --arg checkMaterialization true
      #
      mkTool = name: args: args // {
        index-state = "2020-10-20T00:00:00Z";
        inherit checkMaterialization;
        materialized = ./nix/materialized + "/${name}";
      };
    in mapAttrs mkTool {
      cabal.version                   = "3.2.0.0";
      haskell-language-server.version = "0.5.1";
      hoogle.version                  = "5.0.18";
      hlint.version                   = "3.2";
      lentil.version                  = "1.3.2.0";
      stylish-haskell.version         = "0.11.0.3";
      weeder.version                  = "1.0.9";
    };

    CARDANO_NODE_CONFIGS = walletPackages.cardano-node.deployments;

    # If any build input has bash completions, add it to the search
    # path for shell completions.
    XDG_DATA_DIRS = concatStringsSep ":" (
      [(builtins.getEnv "XDG_DATA_DIRS")] ++
      (filter
        (share: builtins.pathExists (share + "/bash-completion"))
        (map (p: p + "/share") buildInputs))
    );

    meta.platforms = platforms.unix;
  };
in
  if profiling
    then mkShell "cardano-wallet-shell-profiled" walletPackages.profiledHaskellPackages
    else mkShell "cardano-wallet-shell" walletPackages.haskellPackages
