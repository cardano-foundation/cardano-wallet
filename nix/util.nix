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

  # lib.cleanSourceWith filter function which removes socket files
  # from a source tree. This files can be created by cardano-node and
  # cause errors when nix attempts to copy them into the store.
  removeSocketFilesFilter = _path: type:
    elem type ["regular" "directory" "symlink" ];
}
