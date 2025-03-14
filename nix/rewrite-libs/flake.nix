{
  inputs = {
    haskellNix = { url = "github:input-output-hk/haskell.nix"; };
    nixpkgs = {
      url = "github:NixOS/nixpkgs";
      follows = "haskellNix/nixpkgs-unstable";
    };
    flake-utils = {
      url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
    };
  };
  outputs = inputs@{ flake-utils, ... }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      perSystem = system:
        import ./rewrite-libs.nix {
          inherit system;
          inherit (inputs) nixpkgs haskellNix flake-utils;
        };
    in flake-utils.lib.eachSystem supportedSystems perSystem;
}
