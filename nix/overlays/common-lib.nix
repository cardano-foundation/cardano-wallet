self: super: let
  inherit (self) lib cardanoWalletHaskellProject;
  inherit (self.haskell-nix) haskellLib;
in {
  cardanoWalletLib = {
    # Retrieve the list of local project packages by
    # using the haskell.nix selectProjectPackages function.
    projectPackageList = let
      project = haskellLib.selectProjectPackages cardanoWalletHaskellProject.hsPkgs;
      names = map (key: (builtins.getAttr key project).identifier.name) (builtins.attrNames project);
    in
      lib.lists.unique names;
  };
}
