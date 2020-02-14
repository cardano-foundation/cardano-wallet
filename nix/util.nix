{ lib, haskell-nix }:

with lib; with haskell-nix.haskellLib;
{

  inherit
    selectProjectPackages
    collectComponents';

  inherit (extra)
    recRecurseIntoAttrs
    collectChecks;

  isCardanoWallet = package:
    (package.isHaskell or false) &&
      ((hasPrefix "cardano-wallet" package.identifier.name) ||
       (elem package.identifier.name [ "text-class" ]));
}
