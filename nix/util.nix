{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

{
  isCardanoWallet = package:
    (hasPrefix "cardano-wallet" package.identifier.name) ||
    (elem package.identifier.name [ "text-class" "bech32" ]);

  collectComponents = group: packageSel: haskellPackages:
    (mapAttrs (_: package: package.components.${group} // { recurseForDerivations = true; })
     (filterAttrs (name: package: (package.isHaskell or false) && packageSel package) haskellPackages))
    // { recurseForDerivations = true; };
}
