{ pkgs ? import ../../nix/default.nix {} }:

let
  gems = pkgs.bundlerEnv {
    name = "gems-cardano-wallet-e2e";
    gemdir = ./.;
    ruby = pkgs.ruby_2_7;
  };
in pkgs.mkShell {
  name = "cardano-wallet-e2e";
  buildInputs = [
    gems
    gems.wrappedRuby
    pkgs.bundix
    pkgs.screen
  ];
}
