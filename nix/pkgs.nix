# our packages overlay
let
  sources = import ./sources.nix {};
  pkgs1903 = import sources."nixpkgs-19.03" {};
in pkgs: _: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  # TODO: do we need a system/config here somehow???
  nodePkgs = import sources."cardano-node" {};
  inherit (pkgs1903) stack;
}
