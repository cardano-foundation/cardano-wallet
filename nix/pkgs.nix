# our packages overlay
let
  pkgs1903 = import (import ./sources.nix {})."nixpkgs-19.03" {};
in pkgs: _: with pkgs; {
  jmPkgs = import ./jormungandr.nix { inherit (pkgs) commonLib; inherit pkgs; };
  inherit (pkgs1903) stack;
  cardanoWalletHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      pkgs
      haskell-nix
      buildPackages
      ;
  };
}
