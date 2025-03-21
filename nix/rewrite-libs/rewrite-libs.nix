{ system, nixpkgs, haskellNix, flake-utils, ... }:
let
  src = ./.;
  indexState = "2024-08-20T21:35:22Z";
  pkgs = import nixpkgs {
    overlays = [ haskellNix.overlay ];
    inherit system;
  };
in import ./nix/project.nix {
  inherit system;
  inherit indexState;
  inherit src;
  inherit (pkgs) haskell-nix;
  inherit pkgs;
}
