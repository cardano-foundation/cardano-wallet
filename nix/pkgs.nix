# our packages overlay
{ system, crossSystem, config }:
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
  cardanoNodePkgs = import sources.cardano-node { inherit system crossSystem config; };
in pkgs: super: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  cardano-node = cardanoNodePkgs.cardano-node // {
    # provide configuration directory as a convenience
    configs = pkgs.runCommand "cardano-node-configs" {} ''
      cp -R ${sources.cardano-node}/configuration $out;
    '';
  };
  inherit (pkgs1903) stack;
}
