# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
in pkgs: super: with pkgs; {
  inherit (pkgs1903) stack;
}
