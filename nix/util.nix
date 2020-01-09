{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

{
  isCardanoWallet = package:
    (package.isHaskell or false) &&
      ((hasPrefix "cardano-wallet" package.identifier.name) ||
       (elem package.identifier.name [ "text-class" "bech32" ]));

  inherit (pkgs.haskell-nix.haskellLib) collectComponents;
}
