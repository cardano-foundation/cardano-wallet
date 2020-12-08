# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  iohkNixMain = import sources.iohk-nix {};
  pkgs2009 = import iohkNixMain.nixpkgs { inherit system crossSystem config; };
  pkgs1903 = import sources."nixpkgs-19.03" {};
in self: super: {
  # Use Cabal-3.2.0.0 from latest stable nixpkgs-20.09.
  # Currently we are stuck on 20.03, which only provides Cabal-3.0.
  inherit (pkgs2009) cabal-install;

  # Use stack-1.9.3 - something is not quite right with the pipeline
  # script when using 2.3.3.
  inherit (pkgs1903) stack;
}
