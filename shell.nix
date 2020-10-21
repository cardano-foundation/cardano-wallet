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
      # NOTE: Updating versions
      #
      # You will need to increase index-state and
      # "niv update haskell.nix" to get recent Hackage packages.
      #
      # Modify the version number, then run:
      #   nix-shell --arg checkMaterialization true
      index-state = "2020-10-20T00:00:00Z";
      mkTool = name: args: {
        inherit index-state checkMaterialization;
      } // (if checkMaterialization then builtins.removeAttrs args ["plan-sha256"] else args);
    in mapAttrs mkTool {
      cabal = {
        version = "3.2.0.0";
        plan-sha256 = "0ik4ws852dk7wchbhc84w5ac149myvc1az7rxapfy0hdmiiwjh2j";
        materialized = ./nix/materialized/cabal;
      };
      ghcid = {
        version = "0.8.7";
        plan-sha256 = "1qqhwx1xkqllfbmiwcwfknx9595l10173rni0gfdwm9bax69j7ik";
        materialized = ./nix/materialized/ghcid;
      };
      haskell-language-server = {
        version = "0.5.1";
        plan-sha256 = "1iz60r763ksarr222lcd7rx3rh2lv8ascqsjw7wf54bgrl3c0nhw";
        materialized = ./nix/materialized/haskell-language-server;
      };
      hoogle = {
        version = "5.0.18";
        plan-sha256 = "1qsbmh7yxswj3wcjv6hvln67bkh87c1nn53ij44n89cj8l5apzss";
        materialized = ./nix/materialized/hoogle;
      };
      hlint = {
        version = "3.2";
        plan-sha256 = "0qjj9a98j3wj3ar0zsmdqsydnnl6bj82xf0f087nrx3x0c6d7jzl";
        materialized = ./nix/materialized/hlint;
      };
      lentil = {
        version = "1.3.2.0";
        plan-sha256 = "08vs2h03n5p9lkkmfx5anqgn2fbx2a3cs23p74bn2kl61fqhkb88";
        materialized = ./nix/materialized/lentil;
      };
      stylish-haskell = {
        version = "0.11.0.3";
        plan-sha256 = "0sys32gfz5hxavwgvv55cc8fwfrw3n8244gvsz49vv7d845fkq66";
        materialized = ./nix/materialized/stylish-haskell;
      };
      weeder = {
        version = "1.0.9";
        plan-sha256 = "0b2sv30jyh0xh3cqi31mc6nzv9p8496s8q2r7dpw7r95mbrlw0dx";
        materialized = ./nix/materialized/weeder;
      };
    };
    CARDANO_NODE_CONFIGS = walletPackages.cardano-node.deployments;
    meta.platforms = platforms.unix;
    shellHook = ''
      setup_completion() {
        local p
        for p in $buildInputs; do
          if [ -d "$p/share/bash-completion" ]; then
            addToSearchPath XDG_DATA_DIRS "$p/share"
          fi
        done
      }
      setup_completion
    '';
  };
in
  if profiling
    then mkShell "cardano-wallet-shell-profiled" walletPackages.profiledHaskellPackages
    else mkShell "cardano-wallet-shell" walletPackages.haskellPackages
