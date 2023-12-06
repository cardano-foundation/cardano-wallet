self: super: let
  inherit (self) lib cardanoWalletHaskellProject;
  inherit (self.haskell-nix) haskellLib;
in {
  cardanoWalletLib = {

    # Convert version strings from Cabal format (YYYY.M.D)
    # to git tag format (vYYYY-MM-DD).
    versionTag = cabalName: let
        versionRegExp = "(^.*)([[:digit:]]{4})\.([[:digit:]]{1,2})\.([[:digit:]]{1,2})(.*$)";
        parts = builtins.match versionRegExp cabalName;
        name = lib.head parts;
        cabalVer = lib.take 3 (lib.drop 1 parts);
        rest = lib.drop 4 parts;
        leading0 = str: if lib.stringLength str == 1 then "0" + str else str;
        tag = "v" + lib.concatMapStringsSep "-" leading0 cabalVer;
      in
        assert lib.assertMsg (parts != null)
          "versionTag: \"${cabalName}\" does not have a version in YYYY.M.D format";
        (name + tag + lib.concatStrings rest);

    # Retrieve the list of local project packages by
    # filtering the set of *all* packages by their homepage.
    projectPackageList = lib.attrNames (lib.filterAttrs
        (_: p: p != null
          && haskellLib.isLocalPackage p.package
          && p.package.homepage == "https://github.com/cardano-foundation/cardano-wallet")
        cardanoWalletHaskellProject.pkg-set.config.packages);

  };
}
