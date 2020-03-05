# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
in pkgs: _: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  cardanoNodePkgs = import sources."cardano-node" { inherit system crossSystem config; };
  inherit (pkgs1903) stack;
}
