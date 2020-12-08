# Builds stack2cabal and makes a script to run it using the latest
# Hackage index-state known to the current revision of Haskell.nix.

{ pkgs ? import ./default.nix {} }:

let
  index-state-hashes = import pkgs.haskell-nix.indexStateHashesPath;
  latest-haskell-nix-index-state = pkgs.lib.last (builtins.attrNames index-state-hashes);
  # Doesn't build with ghc-8.6.5
  stack2cabal = pkgs.haskell-nix.tool "ghc884" "stack2cabal" {
    version = "1.0.12";
    index-state = latest-haskell-nix-index-state;
  };
in
  pkgs.writeScript "run-stack2cabal" ''
    #!${pkgs.runtimeShell}
    exec ${stack2cabal}/bin/stack2cabal -p ${latest-haskell-nix-index-state} "$@"
  ''
