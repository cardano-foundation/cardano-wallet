self: super: let
  inherit (self) lib cardanoWalletHaskellProject;
  inherit (self.haskell-nix) haskellLib;
in {
  cardanoWalletLib = {

    # Retrieve the list of local project packages by
    # filtering the set of *all* packages by their homepage.
    projectPackageList = lib.attrNames (lib.filterAttrs
        (_: p: p != null
          && haskellLib.isLocalPackage p.package
          && p.package.homepage == "https://github.com/cardano-foundation/cardano-wallet")
        cardanoWalletHaskellProject.pkg-set.config.packages);

  };
}
